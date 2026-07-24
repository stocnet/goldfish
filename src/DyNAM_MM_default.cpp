#include <RcppArmadillo.h>
#include "broadcast_updates.h"
#include "flat_updates.h"
#include "stable_softmax.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inheritParams estimate_REM

//' Calculation for estimating an DyNAM-coordination model
//'
//' Output the derivative of log-likelihood, the Fisher information matrix,
//'   the log-Likelihood, and the log-likelihood of each event given input data
//'
//' @noRd
// [[Rcpp::export]]
List estimate_DyNAM_MM(
    const arma::vec& parameters,
    const arma::mat& dep_event_mat,
    const arma::mat& stat_mat_init,
    const arma::mat& stat_mat_update,
    const arma::vec& stat_mat_update_pointer,
    const arma::mat& stat_mat_broadcast,
    const arma::vec& stat_mat_broadcast_pointer,
    const arma::vec& active_sender_init,
    const arma::mat& active_sender_update,
    const arma::vec& active_sender_update_pointer,
    const arma::vec& active_dyad_init,
    const arma::mat& active_dyad_update,
    const arma::vec& active_dyad_update_pointer,
    const int n_actors_1,
    const int n_actors_2,
    const bool twomode_or_reflexive,
    bool impute = true,
    const bool active_dyad_is_point = false,
    const bool return_event_scores = false,
    const bool return_ranks = false,
    const bool return_margins = false
) {
    // initialize stat_mat and numbers
    arma::mat stat_mat = stat_mat_init;
    int n_events = dep_event_mat.n_cols;
    int n_parameters = stat_mat.n_cols;
    // dyad-triangle buffers reused across events. The coordination
    // likelihood is a softmax over the d = n(n-1)/2 unordered dyads; `E` holds
    // the per-sender expected statistics (row i = E_i), `logZ` the per-sender
    // softmax log-normalizers, and `D` the compact d x p deviation buffer
    // (row for dyad {a, b}, a > b, is grad log w_{ab} = s_ab + s_ba - E_a - E_b)
    // allocated once here instead of the per-event full n^2 x p `P_3` copy.
    const int n_dyads = n_actors_1 * (n_actors_1 - 1) / 2;
    arma::mat E(n_actors_1, n_parameters);
    arma::vec logZ(n_actors_1);
    arma::mat D(n_dyads, n_parameters);
    arma::vec logw_dyad(n_dyads);
    arma::vec allowed_dyad(n_dyads);
    arma::vec sender_weights;
    arma::vec dyad_weights;
    int stat_mat_update_id = 0;
    int stat_mat_broadcast_id = 0;
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);
    // Opt-in per-event score matrix. Each row is the per-event
    // increment already accumulated into `derivative` (the dyad-triangle
    // observed-minus-expected deviation D.row(idx_obs) - g); allocated only when
    // requested so the default path pays nothing.
    arma::mat event_scores;
    if (return_event_scores) event_scores.set_size(n_events, n_parameters);
    // Opt-in per-event rank of the observed dyad among the risk set
    // (rank 1 = highest fitted probability); allocated only when requested.
    IntegerVector observed_rank;
    if (return_ranks) observed_rank = IntegerVector(n_events, NA_INTEGER);
    // Opt-in per-actor margin accumulators over unordered pairs. Each event
    // credits both members of the observed pair on the observed side and, on the
    // expected side, both members of every risk-set pair by its fitted
    // probability; each vector therefore totals 2n. Allocated only when requested.
    arma::vec margin_observed;
    arma::vec margin_expected;
    if (return_margins) {
        margin_observed = arma::vec(n_actors_1, fill::zeros);
        margin_expected = arma::vec(n_actors_1, fill::zeros);
    }


    // Check whether there are composition change and initialize
    // the presence of actor1 and actor2
    bool has_composition_change1 = true;
    int active_sender_update_id = 0;
    if (active_sender_update.n_elem == 0) {
        has_composition_change1 = false;
    }
    arma::vec active_sender = active_sender_init;

    bool has_composition_change2 = true;
    int active_dyad_update_id = 0;
    if (active_dyad_update.n_elem == 0) {
        has_composition_change2 = false;
    }
    // `active_dyad` is the folded per-event risk mask. At the outer encoding it is
    // the length-n2 receiver vector (dyad (i, j) available iff active_sender(i) &
    // active_dyad(j)). At the point encoding it is a flattened n1 x n2 mask
    // (sender-major: dyad (i, j) at i * n_actors_2 + j) — a symmetrised support
    // constraint folded in — maintained by a (node1, node2, replace)
    // buffer and read cell-wise.
    arma::vec active_dyad = active_dyad_init;


    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        // update stat_mat
        apply_flat_updates(
          stat_mat, stat_mat_update, stat_mat_update_id,
          stat_mat_update_pointer(id_event), n_actors_2
        );
        apply_broadcast_updates(
          stat_mat, stat_mat_broadcast, stat_mat_broadcast_id,
          stat_mat_broadcast_pointer(id_event), n_actors_1, n_actors_2,
          twomode_or_reflexive
        );
        // impute the missing statistics if necessary
        if (impute) {
            for (int i = 0; i < n_parameters; i++) {
                // Construct a view for the i-th column of
                // the stat_matrix and do the impute
                arma::vec current_col(
                    stat_mat.colptr(i),
                    n_actors_1 * n_actors_2,
                    false
                );
                current_col.elem(find_nonfinite(current_col)).fill(
                    mean(current_col.elem(find_finite(current_col))));
            }
        }

        // update presence
        if (has_composition_change1) {
            while (active_sender_update_id < active_sender_update_pointer(id_event)) {
                active_sender(active_sender_update(0, active_sender_update_id) - 1) =
                  active_sender_update(1, active_sender_update_id);
                active_sender_update_id++;
            }
        }
        if (has_composition_change2) {
            while (active_dyad_update_id < active_dyad_update_pointer(id_event)) {
                if (active_dyad_is_point) {
                    active_dyad(
                      (active_dyad_update(0, active_dyad_update_id) - 1) * n_actors_2 +
                      (active_dyad_update(1, active_dyad_update_id) - 1)
                    ) = active_dyad_update(2, active_dyad_update_id);
                } else {
                    active_dyad(active_dyad_update(0, active_dyad_update_id) - 1) =
                      active_dyad_update(1, active_dyad_update_id);
                }
                active_dyad_update_id++;
            }
        }

        // TO check(gutian): handle ignorant


        // We calculate the derivative, log-Likelihood, and fisher information
        // matrix of a current event. Staged dyad-triangle softmax:
        // a per-sender stable softmax gives the log-normalizers and expected
        // statistics; the coordination likelihood is then a d-alternative
        // softmax over the unordered dyads with log-weight
        // log w_{ab} = log p(a->b) + log p(b->a), replacing the n^2 `P = p % p.t()`
        // temporaries and the underflow-prone final log(P(s, r)). The observed
        // dyad's logL comes from the shifted predictor (finite under underflow).
        const int id_sender = dep_event_mat(0, id_event) - 1;
        const int id_receiver = dep_event_mat(1, id_event) - 1;

        // linear predictors x_{ij} = beta^T s_{ij}, sender-major (row i*n2 + j)
        arma::vec lin_pred = stat_mat * parameters;

        // risk-set mask over directed dyads (sender-major), as in REM_ordered:
        // dyad (i -> j) available iff sender i active, receiver j in the folded
        // support/presence mask, and (twomode_or_reflexive || j != i).
        arma::vec allowed(n_actors_1 * n_actors_2, fill::zeros);
        for (int i = 0; i < n_actors_1; ++i) {
            if (active_sender(i) == 1) {
                int not_allowed_receiver = twomode_or_reflexive ? -1 : i;
                // point encoding: sender i's row starts at i * n_actors_2;
                // outer encoding: the receiver vector is read directly.
                int dyad_offset = active_dyad_is_point ? i * n_actors_2 : 0;
                for (int j = 0; j < n_actors_2; ++j) {
                    if (active_dyad(dyad_offset + j) == 1 &&
                        (j != not_allowed_receiver)) {
                        allowed(i * n_actors_2 + j) = 1;
                    }
                }
            }
        }

        // per-sender softmax: log-normalizer logZ_i and expected statistic E_i
        // (one GEMV per sender). A fully masked sender maps to E_i = 0; every
        // dyad involving it is masked out below, so it never contributes.
        for (int i = 0; i < n_actors_1; ++i) {
            arma::vec lin_pred_i(
                lin_pred.memptr() + i * n_actors_2, n_actors_2, false);
            arma::vec allowed_i(
                allowed.memptr() + i * n_actors_2, n_actors_2, false);
            logZ(i) = stable_softmax_masked(lin_pred_i, allowed_i, sender_weights);
            double norm_i = accu(sender_weights);
            if (norm_i > 0) {
                E.row(i) = (sender_weights.t() *
                  stat_mat.rows(i * n_actors_2,
                                i * n_actors_2 + n_actors_2 - 1)) / norm_i;
            } else {
                E.row(i).zeros();
            }
        }

        // build the length-d dyad list (a > b): log-weights and the compact
        // deviation buffer D_d = s_ab + s_ba - E_a - E_b. A dyad is allowed iff
        // both directed dyads are (either direction masked => pair weight 0).
        int idx = 0;
        for (int a = 1; a < n_actors_1; ++a) {
            for (int b = 0; b < a; ++b) {
                if (allowed(a * n_actors_2 + b) == 1 &&
                    allowed(b * n_actors_2 + a) == 1) {
                    logw_dyad(idx) = lin_pred(a * n_actors_2 + b) - logZ(a) +
                                     lin_pred(b * n_actors_2 + a) - logZ(b);
                    allowed_dyad(idx) = 1;
                } else {
                    logw_dyad(idx) = -arma::datum::inf;
                    allowed_dyad(idx) = 0;
                }
                D.row(idx) = stat_mat.row(a * n_actors_2 + b) +
                             stat_mat.row(b * n_actors_2 + a) -
                             E.row(a) - E.row(b);
                ++idx;
            }
        }

        // d-alternative dyad softmax, then one weighted-crossprod GEMM Fisher
        double log_normalizer =
          stable_softmax_masked(logw_dyad, allowed_dyad, dyad_weights);
        double normalizer = accu(dyad_weights);
        const int a_obs = (id_sender > id_receiver) ? id_sender : id_receiver;
        const int b_obs = (id_sender > id_receiver) ? id_receiver : id_sender;
        const int idx_obs = a_obs * (a_obs - 1) / 2 + b_obs;
        if (return_ranks) {
            const double obs_weight = dyad_weights(idx_obs);
            int rank = 1;
            for (int d = 0; d < n_dyads; d++) {
                if (allowed_dyad(d) == 1 && dyad_weights(d) > obs_weight) rank++;
            }
            observed_rank[id_event] = rank;
        }
        if (return_margins) {
            // Walk the same unordered-dyad triangle as the build loop above so
            // dyad index `idx_m` reconstructs its members (a > b); credit each
            // member with the dyad's fitted probability.
            int idx_m = 0;
            for (int a = 1; a < n_actors_1; ++a) {
                for (int b = 0; b < a; ++b) {
                    if (allowed_dyad(idx_m) == 1) {
                        const double p = dyad_weights(idx_m) / normalizer;
                        margin_expected(a) += p;
                        margin_expected(b) += p;
                    }
                    ++idx_m;
                }
            }
            margin_observed(id_sender) += 1;
            margin_observed(id_receiver) += 1;
        }
        // expected gradient g = sum_d P_d D_d; score = grad log w_obs - g
        arma::rowvec g = (dyad_weights.t() * D) / normalizer;
        if (return_event_scores) {
            event_scores.row(id_event) = D.row(idx_obs) - g;
        }
        derivative += D.row(idx_obs) - g;
        // Fisher: sum_d P_d D_d D_d^T - g^T g
        fisher += (D.each_col() % dyad_weights).t() * D / normalizer -
          g.t() * g;
        // logLikelihood from the shifted predictor (finite under underflow)
        intervalLogL(id_event) = logw_dyad(idx_obs) - log_normalizer;
        logLikelihood += intervalLogL(id_event);
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("logLikelihood") = logLikelihood,
      Named("intervalLogL") = intervalLogL,
      Named("event_scores") = event_scores,
      Named("observed_rank") = observed_rank,
      Named("margin_observed") = margin_observed,
      Named("margin_expected") = margin_expected
    );
}


