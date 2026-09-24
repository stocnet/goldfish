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

