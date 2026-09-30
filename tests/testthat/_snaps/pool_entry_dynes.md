# new_pool_attachment validates its theta_ref and layout

    Code
      new_pool_attachment(stats = list(), loglik_ref = -1, theta_ref = "a", layout = "tag")
    Condition
      Error:
      ! `theta_ref` must be a numeric vector.

---

    Code
      new_pool_attachment(stats = list(), loglik_ref = -1, theta_ref = 0, layout = c(
        "a", "b"))
    Condition
      Error:
      ! `layout` must be a single string.

