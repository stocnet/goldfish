# The prototype-path pool evaluator behind the ABMCEM loop's evaluator contract
# (driven by estimate_dynes()). A pool is a set of endpoint-hitting sequences;
# each is preprocessed once, on pool entry, through the existing estimation
# pipeline, and its per-sub-model statistics are stored so that re-evaluation at
# any parameter vector reuses them without re-preprocessing. This file owns the
# pool structure and the entry path; the zero-iteration evaluation adapter that
# reads the stored statistics is defined alongside it.
#
# The loop is fid-indexed: theta concatenates each fid's free (non-offset)
# sub-model parameters, and every pooled sequence carries one goldfishStat per
# sub-model. A sub-model descriptor is pool-level (shared across sequences,
# since only the drawn event stream differs from one sequence to the next); the
# statistics, the data object, and the reference record are per sequence.

# A pool-level sub-model descriptor: the formula/model/sub_model the existing
# engine preprocesses and evaluates, plus `theta_index`, the positions this
# sub-model's free effects occupy in the concatenated parameter vector. A fid's
# fixed effects are the formula's offset() terms, baked into the preprocessed
# statistics, so they never enter `theta_index`.
new_pool_submodel <- function(
  formula,
  model = c("DyNAM", "REM", "DyNAMi"),
  sub_model = c("choice", "rate", "rate_ordered", "choice_coordination"),
  theta_index,
  call = rlang::caller_env()
) {
  if (!rlang::is_formula(formula)) {
    cli::cli_abort("{.arg formula} must be a formula.", call = call)
  }
  model <- rlang::arg_match(model)
  sub_model <- rlang::arg_match(sub_model)
  if (
    !is.numeric(theta_index) ||
      length(theta_index) == 0 ||
      anyNA(theta_index) ||
      any(theta_index < 1) ||
      any(theta_index != as.integer(theta_index))
  ) {
    cli::cli_abort(
      "{.arg theta_index} must be a vector of positive integer positions.",
      call = call
    )
  }
  list(
    formula = formula,
    model = model,
    sub_model = sub_model,
    theta_index = as.integer(theta_index)
  )
}

# Preprocess one pooled sequence: run every sub-model's formula through the
# existing pipeline over the sequence's data object and return the stored
# goldfishStat objects, one per sub-model, in sub-model order. This is the
# per-sequence unit the entry map seam drives; it is the only place preprocess
# runs, so evaluation (which reuses these objects) never re-preprocesses.
preprocess_pool_sequence <- function(
  sub_models,
  data,
  control_prep = set_preprocessing(),
  call = rlang::caller_env()
) {
  lapply(sub_models, function(sm) {
    estimate_wrapper(
      x = sm$formula,
      model = sm$model,
      sub_model = sm$sub_model,
      data = data,
      control_prep = control_prep,
      preprocessing_only = TRUE,
      verbose = FALSE,
      progress = FALSE
    )
  })
}

# Build a sequence pool: preprocess each sequence once, on entry, through the
# injectable per-sequence map seam (serial lapply by default), and store the
# statistics, the data object, and the reference record with the sequence.
#
# * `sub_models`: a list of new_pool_submodel() descriptors, shared across
#   sequences.
# * `sequences`: a list of per-sequence inputs, each a list with `data` (a
#   stocnet the augmenter drew) and `reference` (a new_reference_record()). An
#   optional `attachment` slot is accepted and ignored in this version -- the
#   from-scratch entry path is uniform for every augmenter -- so a later
#   precomputed-statistics fast path is a drop-in with no signature churn.
# * `theta_length`: the length of the concatenated parameter vector the pool is
#   evaluated at.
# * `map_fn`: the per-sequence map seam, `function(x, f)`; the serial lapply
#   default requires no optional dependency.
build_sequence_pool <- function(
  sub_models,
  sequences,
  theta_length,
  control_prep = set_preprocessing(),
  map_fn = lapply,
  call = rlang::caller_env()
) {
  if (!is.list(sub_models) || length(sub_models) == 0) {
    cli::cli_abort(
      "{.arg sub_models} must be a non-empty list of sub-model descriptors.",
      call = call
    )
  }
  if (!is.list(sequences) || length(sequences) == 0) {
    cli::cli_abort(
      "{.arg sequences} must be a non-empty list of pooled sequences.",
      call = call
    )
  }
  if (
    !rlang::is_scalar_integerish(theta_length, finite = TRUE) ||
      theta_length < 1
  ) {
    cli::cli_abort(
      "{.arg theta_length} must be a single positive integer.",
      call = call
    )
  }
  theta_length <- as.integer(theta_length)
  for (sm in sub_models) {
    if (any(sm$theta_index > theta_length)) {
      cli::cli_abort(
        "A sub-model's {.field theta_index} runs past {.arg theta_length}
         ({.val {theta_length}}).",
        call = call
      )
    }
  }
  for (i in seq_along(sequences)) {
    seq_i <- sequences[[i]]
    if (!is.list(seq_i) || is.null(seq_i$data) || is.null(seq_i$reference)) {
      cli::cli_abort(
        "Pooled sequence {.val {i}} must be a list with {.field data} and
         {.field reference}.",
        call = call
      )
    }
  }

  built <- map_fn(sequences, function(seq_i) {
    stats <- preprocess_pool_sequence(
      sub_models,
      seq_i$data,
      control_prep = control_prep,
      call = call
    )
    list(
      data = seq_i$data,
      stats = stats,
      reference = seq_i$reference,
      attachment = seq_i$attachment
    )
  })

  structure(
    list(
      sub_models = sub_models,
      sequences = built,
      theta_length = theta_length,
      control_prep = control_prep,
      map_fn = map_fn
    ),
    class = c("goldfishDynesPool", "list")
  )
}

# The pool's reference records, in sequence order -- the per-sequence
# (theta_ref, loglik_ref, log_proposal) triples the E-step weighting reads.
pool_references <- function(pool) {
  lapply(pool$sequences, function(s) s$reference)
}
