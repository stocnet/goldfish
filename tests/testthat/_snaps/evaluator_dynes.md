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

# make_proposal_evaluator does not fabricate rate-at-position queries

    Code
      eval_fn(fx$sequences[[1]]$data, rate_queries = list(c(1L, 2L)))
    Condition
      Error:
      ! Named-pair rate-at-position queries are not yet implemented.
      i Only loglik is available from this version's proposal evaluator.

