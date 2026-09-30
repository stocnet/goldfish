# per-sequence score and information must match the pool

    Code
      new_dynes_estep(lambda, w, score = matrix(0, 2, 2), fisher = list())
    Condition
      Error:
      ! `score` must be a matrix with one row per pooled sequence (3).
    Code
      new_dynes_estep(lambda, w, score = matrix(0, 3, 2), fisher = list(diag(2), diag(
        2)))
    Condition
      Error:
      ! `fisher` must be a list with one matrix per pooled sequence (3).
    Code
      new_dynes_estep(lambda, w, score = matrix(0, 3, 2), fisher = list(diag(2), diag(
        2), diag(3)))
    Condition
      Error:
      ! Each `fisher` matrix must be 2 x 2, matching the columns of `score`.
      x Sequence does not match: 3.

