# new_pool_attachment validates its stats and loglik_ref

    Code
      new_pool_attachment(stats = "not-a-list", loglik_ref = -1)
    Condition
      Error:
      ! `stats` must be a list.

---

    Code
      new_pool_attachment(stats = list(), loglik_ref = NA_real_)
    Condition
      Error:
      ! `loglik_ref` must be a single finite numeric value.

# rate_queries without a specification aborts naming spec

    Code
      eval_fn(fx$sequences[[1]]$data, rate_queries = queries)
    Condition
      Error:
      ! Rate queries need the completed joint specification.
      i Supply `spec` to `make_proposal_evaluator()`.

# compute_lik_seq() validates its specification and theta

    Code
      compute_lik_seq(list(), spec$data, c(0, 0, 0))
    Condition
      Error in `compute_lik_seq()`:
      ! `spec` must be a <goldfishJointSpec>.
      i Build one with `make_joint_specification()`.
    Code
      compute_lik_seq(spec, spec$data, c(0, 0))
    Condition
      Error in `compute_lik_seq()`:
      ! `theta` must have one value per free coefficient.
      x The specification has 3 free coefficients; `theta` has 2 values.
    Code
      compute_lik_seq(spec, spec$data, "a")
    Condition
      Error in `compute_lik_seq()`:
      ! `theta` must be a numeric vector without missing values.
    Code
      compute_lik_seq(spec, spec$data, c(a = 0, b = 0, c = 0))
    Condition
      Error in `compute_lik_seq()`:
      ! A named `theta` must name each free coefficient once.
      x Not a free coefficient: "a", "b", and "c".
      i The free coefficients are "friendship › choice: indeg/friendship", "calls › rate: Intercept", and "calls › rate: indeg/friendship".

