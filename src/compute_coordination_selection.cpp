#include <RcppArmadillo.h>
#include "stable_softmax.h"
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// @inherit estimate_REM return

//' Estimate a DyNAM-coordination model with gathered data
//'
//' Given the gathered and distilled data, it outputs the derivative of
//'     the loglikelihood, the Fisher information matrix, the logLikelihood,
//'     and the loglikelihood of each event  for DyNAM-coordination models.
//'
//' @param parameters An n_effects by 1 matrix, which is the input parameter
//' @param stat_all_events An matrix with n_effects columns.
//'     Each row represent the values of all effects of
//'     a sender-receiver pair in an event.
//'     For example, for a model with 2 effects, stat_all_events 
//'     might looks like this.
//'     \tabular{rr}{
//'       3.2 \tab 1.9\cr
//'       3.2 \tab 4.5\cr
//'       1.2 \tab 5.2\cr
//'       4.3 \tab 3.1\cr
//'       2.4 \tab 4.7\cr
//'       9.2 \tab 5.6\cr
//'       2.9 \tab 8.9\cr
//'       ... \tab ...\cr
//'     }
//'     The first row means in a event, the values of the two effects of
//'     a candidate pair is (3.2,1.9).
//' @param n_candidates An n_events by 1 matrix.
//'     It record how many candidate sender-receiver pairs are in each event.
//'     It indicates which row in stat_all_events belongs to which event.
//'     For example if the first two element of n_candidates are (2, 4),
//'     then the first 2 rows of stat_all_events correspond to
//'     the candidate pairs in the first event, which is
//'     \tabular{rr}{
//'       3.2 \tab 1.9\cr
//'       3.2 \tab 4.5\cr
//'     }
//'     And the 3rd-6th rows correspond to the candidate pairs in
//'     the second event, which is
//'     \tabular{rr}{
//'       1.2 \tab 5.2\cr
//'       4.3 \tab 3.1\cr
//'       2.4 \tab 4.7\cr
//'       9.2 \tab 5.6\cr
//'     }
//' @param n_candidates1 An n_events by 1 matrix, which is only used
//'     for estimating the DyNAM-coordination model.
//'     It record how many candidate sender are in each event.
//'     And we have n_candidates1 * n_candidates2 = n_candidates.
//' @param n_candidates2 An n_events by 1 matrix, which is only used
//'     for estimating the DyNAM-coordination model.
//'     It record how many candidate receiver are in each event.
//'     And we have n_candidates1 * n_candidates2 = n_candidates.
//' @param selected An n_events by 1 matrix.
//'     It records the position of the selected candidate sender-receiver pair
//'     in each event.
//'     For example  if the first two element of n_candidates are 2, 4,
//'     and the first two elements of selected is (0,2).
//'     Then the value of the effects of the pair selected in the first event is
//'     (3.2,1.9), which is the 1st(=0+1) row of
//'     \tabular{rr}{
//'       3.2 \tab 1.9\cr
//'       3.2 \tab 4.5\cr
//'     }
//'     And the value of the effects of the pair selected in the second event is
//'     (2.4, 4.7), which is the 3rd(=2+1) row of
//'     \tabular{rr}{
//'       1.2 \tab 5.2\cr
//'       4.3 \tab 3.1\cr
//'       2.4 \tab 4.7\cr
//'       9.2 \tab 5.6\cr
//'     }
//' @param selected_actor1 An n_events by 1 matrix.
//'     It records the index of the selected candidate sender among
//'     all candidate sender in each event.
//' @param selected_actor2 An n_events by 1 matrix.
//'     It records the index of the selected candidate receiver among
//'     all candidate receiver in each event.
//' @noRd
// [[Rcpp::export]]
List compute_coordination_selection(
    arma::colvec& parameters,
    const arma::mat& stat_all_events,
    const arma::uvec& n_candidates,
    const arma::uvec& n_candidates1,
    const arma::uvec& n_candidates2,
    const arma::uvec& selected,
    const arma::uvec& selected_actor1,
    const arma::uvec& selected_actor2,
    const bool twomode_or_reflexive
) {
    int n_events = selected.size();
    int n_parameters = parameters.size();
    // declare return variables
    arma::mat fisher(n_parameters, n_parameters, fill::zeros);
    arma::mat derivative(1, n_parameters, fill::zeros);
    double logLikelihood = 0;
    arma::vec intervalLogL(n_events, fill::zeros);

    // Linear predictors beta^T s for every candidate dyad in every event (one
    // GEMV); the staged dyad-triangle softmax below works in log space.
    arma::vec lin_pred_all = stat_all_events * parameters;

    // dyad-triangle buffers sized to the largest event and reused (design D9),
    // mirroring the default_c coordination kernel. `E` holds the per-sender
    // expected statistics, `logZ` the per-sender softmax log-normalizers, and
    // `D` the compact d x p deviation buffer (d = n(n-1)/2) replacing the
    // per-event full n^2 x p `P_3` copy. The per-event rectangular n1 x n2
    // candidate grid (sender-major) is retained.
    const arma::uword max_n_actors =
      n_candidates1.empty() ? 0 : n_candidates1.max();
    const arma::uword max_n_dyads = max_n_actors * (max_n_actors - 1) / 2;
    arma::mat E(max_n_actors, n_parameters);
    arma::vec logZ(max_n_actors);
    arma::mat D(max_n_dyads, n_parameters);
    arma::vec logw_dyad(max_n_dyads);
    arma::vec allowed_dyad(max_n_dyads);
    arma::vec sender_weights;
    arma::vec dyad_weights;
    // start address in stat_all_events of current events
    int id_start = 0;

    // Go through all events
    for (int id_event = 0; id_event < n_events; id_event++) {
        int id_end = id_start + n_candidates(id_event);
        const int id_sender = selected_actor1(id_event);
        const int id_receiver = selected_actor2(id_event);
        const int n_actors_1 = n_candidates1(id_event);
        const int n_actors_2 = n_candidates2(id_event);
        const int n_dyads = n_actors_1 * (n_actors_1 - 1) / 2;

        // risk set for the rectangular grid: every candidate dyad is allowed
        // except the reflexive diagonal when the model is not two-mode /
        // reflexive (mirrors the old p.diag().zeros()).
        arma::vec allowed(n_actors_1 * n_actors_2, fill::ones);
        if (!twomode_or_reflexive) {
            for (int i = 0; i < n_actors_1; ++i) {
                allowed(i * n_actors_2 + i) = 0;
            }
        }

        // per-sender softmax: log-normalizer logZ_i and expected statistic E_i
        // (one GEMV per sender), reading the event's rows of the gathered stack.
        for (int i = 0; i < n_actors_1; ++i) {
            arma::vec lin_pred_i(
                lin_pred_all.memptr() + id_start + i * n_actors_2,
                n_actors_2, false);
            arma::vec allowed_i(
                allowed.memptr() + i * n_actors_2, n_actors_2, false);
            logZ(i) =
              stable_softmax_masked(lin_pred_i, allowed_i, sender_weights);
            double norm_i = accu(sender_weights);
            if (norm_i > 0) {
                E.row(i) = (sender_weights.t() *
                  stat_all_events.rows(
                    id_start + i * n_actors_2,
                    id_start + i * n_actors_2 + n_actors_2 - 1)) / norm_i;
            } else {
                E.row(i).zeros();
            }
        }

        // length-d dyad list (a > b): log-weights and the compact deviation
        // buffer D_d = s_ab + s_ba - E_a - E_b = grad log w_{ab}. A dyad is
        // allowed iff both directed dyads are.
        int idx = 0;
        for (int a = 1; a < n_actors_1; ++a) {
            for (int b = 0; b < a; ++b) {
                if (allowed(a * n_actors_2 + b) == 1 &&
                    allowed(b * n_actors_2 + a) == 1) {
                    logw_dyad(idx) =
                      lin_pred_all(id_start + a * n_actors_2 + b) - logZ(a) +
                      lin_pred_all(id_start + b * n_actors_2 + a) - logZ(b);
                    allowed_dyad(idx) = 1;
                } else {
                    logw_dyad(idx) = -arma::datum::inf;
                    allowed_dyad(idx) = 0;
                }
                D.row(idx) =
                  stat_all_events.row(id_start + a * n_actors_2 + b) +
                  stat_all_events.row(id_start + b * n_actors_2 + a) -
                  E.row(a) - E.row(b);
                ++idx;
            }
        }

        // d-alternative dyad softmax, then one weighted-crossprod GEMM Fisher
        arma::vec logw_event(logw_dyad.memptr(), n_dyads, false);
        arma::vec allowed_event(allowed_dyad.memptr(), n_dyads, false);
        double log_normalizer =
          stable_softmax_masked(logw_event, allowed_event, dyad_weights);
        double normalizer = accu(dyad_weights);
        const int a_obs = (id_sender > id_receiver) ? id_sender : id_receiver;
        const int b_obs = (id_sender > id_receiver) ? id_receiver : id_sender;
        const int idx_obs = a_obs * (a_obs - 1) / 2 + b_obs;
        arma::subview<double> D_event = D.rows(0, n_dyads - 1);
        // expected gradient g = sum_d P_d D_d; score = grad log w_obs - g
        arma::rowvec g = (dyad_weights.t() * D_event) / normalizer;
        derivative += D.row(idx_obs) - g;
        // Fisher: sum_d P_d D_d D_d^T - g^T g
        fisher += (D_event.each_col() % dyad_weights).t() * D_event /
          normalizer - g.t() * g;
        // logLikelihood from the shifted predictor (finite under underflow)
        intervalLogL(id_event) = logw_dyad(idx_obs) - log_normalizer;
        logLikelihood += intervalLogL(id_event);

        // renew the starting index
        id_start = id_end;
    }

    return List::create(
      Named("derivative") = derivative,
      Named("fisher") = fisher,
      Named("logLikelihood") = logLikelihood,
      Named("intervalLogL") = intervalLogL
    );
}
