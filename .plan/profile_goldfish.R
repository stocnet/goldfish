# profile_goldfish.R --------------------------------------------------------
#
# Cross-version profiling harness for goldfish. Install a version listed in
# `.plan/goldfish_versions.csv`, then run `profile_goldfish()` for it. Each
# result row carries `version`, `engine`, and `model`, so results from
# different versions/engines can be row-bound and compared.
#
# Two user interfaces are supported (the CSV `interface` column says which a
# version uses):
#   * "old"  (CRAN, <= 1.6.x): estimate(formula, model=, subModel=),
#            defineNetwork()/defineNodes()/defineDependentEvents(). The data
#            object is NOT a function argument -- the formula objects are
#            resolved from the calling environment, so `object` may be NULL.
#   * "new"  (develop, refactor/rate_prep): estimate_dynam()/estimate_rem()
#            with `sub_model`, `data = <make_data() object>`, and an engine
#            selected via set_estimation_opt(engine = ).
#
# Because two goldfish builds cannot be loaded in one R session, the workflow
# is one fresh session per version:
#
#   source(".plan/profile_goldfish.R")
#   install_goldfish_version("1.8.0")   # then RESTART R
#   # --- fresh session ---
#   library(goldfish); source(".plan/profile_goldfish.R")
#   <build data + formulas for this interface; see the examples at the bottom>
#   res <- profile_goldfish(object = socialEvData, formulas = formulas_new)
#   saveRDS(res, sprintf(".plan/profvis/profile_%s.rds", res$summary$version[1]))
#
# After collecting one RDS per version, bind and compare (see the very end).
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a)) b else a

# ---- version registry -----------------------------------------------------

#' Read the version -> commit registry used to install builds with remotes.
read_goldfish_versions <- function(
  path = ".plan/goldfish_versions.csv"
) {
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Install one goldfish build named in the registry. Run in a session with no
#' goldfish loaded; restart R afterwards before profiling. CRAN rows install
#' the pinned release; github rows install the recorded ref (tag or commit).
install_goldfish_version <- function(
  version,
  versions = read_goldfish_versions(),
  upgrade = "never",
  ...
) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("Package 'remotes' is required to install versions.")
  }
  row <- versions[versions$version == version, , drop = FALSE]
  if (nrow(row) == 0L) {
    stop("version '", version, "' not found in the registry.")
  }
  row <- row[1, ]
  pushed <- if ("pushed" %in% names(row)) {
    isTRUE(as.logical(row$pushed))
  } else TRUE
  if (identical(row$source, "cran")) {
    remotes::install_version(
      "goldfish", version = sub("^v", "", row$ref %||% version),
      upgrade = upgrade, ...
    )
  } else {
    if (!pushed) {
      stop("version '", version, "' points at commit ", row$ref,
           " which is marked pushed = FALSE (local-only). Push the branch to ",
           row$repo, " first, or build/install it locally from a checkout.")
    }
    remotes::install_github(row$repo, ref = row$ref, upgrade = upgrade, ...)
  }
  message("Installed goldfish ", version, " (", row$interface, " interface). ",
          "Restart R before profiling.")
  invisible(row)
}

# ---- model registry -------------------------------------------------------

#' Map a formula key -> (model, sub_model). This is the single source of truth
#' for how each named formula is estimated; override it to change the mapping.
#'
#' NOTE on choice_coordination: in goldfish it is a DyNAM sub_model
#' (DyNAM-MM / coordination), estimated with `estimate_dynam(sub_model =
#' "choice_coordination")` (old: `estimate(subModel = "choice_coordination")`).
#' REM only supports rate / rate_ordered, so coordination is mapped to DyNAM
#' here. Edit this table if your setup defines it differently.
#'
#' "ordered" keys map to the same model/sub_model as their base counterpart;
#' the ordering is expressed by the formula you supply (no time intercept),
#' which works across every version. Set `use_ordered_submodel = TRUE` in
#' profile_goldfish() to instead pass sub_model = "rate_ordered" on builds
#' (>= 1.7.4) that expose it.
goldfish_default_model_map <- function() {
  list(
    rate                = list(model = "DyNAM", sub_model = "rate"),
    rate_ordered        = list(model = "DyNAM", sub_model = "rate"),
    choice              = list(model = "DyNAM", sub_model = "choice"),
    choice_coordination = list(model = "DyNAM", sub_model = "choice_coordination"),
    rem                 = list(model = "REM",   sub_model = "rate"),
    rem_ordered         = list(model = "REM",   sub_model = "rate")
  )
}

# Detect which interface the loaded goldfish exposes, unless told explicitly.
detect_interface <- function() {
  exports <- tryCatch(getNamespaceExports("goldfish"), error = function(e) character())
  if ("estimate_dynam" %in% exports) "new" else "old"
}

# ---- estimation primitives ------------------------------------------------

# Run a thunk once, capturing elapsed time, status, and any warning text.
time_call <- function(thunk) {
  status <- "ok"
  msg <- ""
  timing <- tryCatch(
    system.time(value <- withCallingHandlers(
      thunk(),
      warning = function(w) {
        msg <<- paste(msg, conditionMessage(w), sep = if (nzchar(msg)) " | " else "")
        invokeRestart("muffleWarning")
      }
    )),
    error = function(e) {
      status <<- "error"
      msg <<- conditionMessage(e)
      NULL
    }
  )
  if (status == "error") {
    return(list(value = NULL, elapsed = NA_real_, status = status, message = msg))
  }
  list(value = value, elapsed = unname(timing["elapsed"]), status = status, message = msg)
}

resolve_sub_model <- function(map_entry, interface, ordered, use_ordered_submodel) {
  sm <- map_entry$sub_model
  if (ordered && use_ordered_submodel && interface == "new") sm <- "rate_ordered"
  sm
}

build_control_new <- function(engine, is_coord, control_coord) {
  if (is_coord) {
    goldfish::set_estimation_opt(
      engine = engine,
      initial_damping = control_coord$initial_damping,
      max_iterations = control_coord$max_iterations
    )
  } else {
    goldfish::set_estimation_opt(engine = engine)
  }
}

# Preprocess one model ONCE (engine-independent). Returns the preprocessed
# object, its wall time, in-memory size, and the mode-1 actor count. The
# preprocessed object is reused across engines so preprocessing is not repeated.
goldfish_preprocess <- function(
  formula, map_entry, object, interface, ordered, use_ordered_submodel
) {
  model <- map_entry$model
  sub_model <- resolve_sub_model(map_entry, interface, ordered, use_ordered_submodel)
  thunk <- function() {
    if (interface == "new") {
      if (model == "REM") {
        goldfish::estimate_rem(formula, data = object, preprocessing_only = TRUE)
      } else {
        goldfish::estimate_dynam(
          formula, sub_model = sub_model, data = object, preprocessing_only = TRUE
        )
      }
    } else {
      do.call(goldfish::estimate,
              list(formula, model = model, subModel = sub_model, preprocessingOnly = TRUE))
    }
  }
  r <- time_call(thunk)
  prep <- r$value
  size <- if (!is.null(prep)) as.numeric(utils::object.size(prep)) else NA_real_
  na <- if (!is.null(prep) && !is.null(prep$initialStats)) {
    dim(prep$initialStats)[1]
  } else NA_integer_
  list(prep = prep, t_preprocess = r$elapsed, object_size_bytes = size,
       n_actors = na, status = r$status, message = r$message)
}

# Estimate one model x engine. When a preprocessed object is available (new
# interface) it is fed via preprocessing_init, so the timed call is the pure
# estimation step and preprocessing is not repeated. Falls back to a full run.
# Build a zero-arg thunk for one estimation call in the given `mode`
# ("from_prep" reuses the preprocessed object; "full" preprocesses + estimates).
# Reused for the system.time run AND the bench run so both measure the same call.
make_estimate_thunk <- function(
  formula, map_entry, object, interface, engine, prep,
  control_coord, ordered, use_ordered_submodel, mode
) {
  model <- map_entry$model
  sub_model <- resolve_sub_model(map_entry, interface, ordered, use_ordered_submodel)
  is_coord <- identical(sub_model, "choice_coordination")

  if (mode == "from_prep") {
    ctrl <- build_control_new(engine, is_coord, control_coord)
    return(function() {
      if (model == "REM") {
        goldfish::estimate_rem(formula, data = object, preprocessing_init = prep,
                               control_estimation = ctrl)
      } else {
        goldfish::estimate_dynam(formula, sub_model = sub_model, data = object,
                                 preprocessing_init = prep, control_estimation = ctrl)
      }
    })
  }
  function() {
    if (interface == "new") {
      ctrl <- build_control_new(engine, is_coord, control_coord)
      if (model == "REM") {
        goldfish::estimate_rem(formula, data = object, control_estimation = ctrl)
      } else {
        goldfish::estimate_dynam(formula, sub_model = sub_model, data = object,
                                 control_estimation = ctrl)
      }
    } else {
      args <- list(formula, model = model, subModel = sub_model)
      if (is_coord) {
        args$estimationInit <- list(initialDamping = control_coord$initial_damping,
                                    maxIterations = control_coord$max_iterations)
      }
      do.call(goldfish::estimate, args)
    }
  }
}

goldfish_estimate <- function(
  formula, map_entry, object, interface, engine, prep,
  control_coord, ordered, use_ordered_submodel
) {
  args <- list(formula, map_entry, object, interface, engine, prep,
               control_coord, ordered, use_ordered_submodel)
  if (interface == "new" && !is.null(prep)) {
    r <- time_call(do.call(make_estimate_thunk, c(args, list(mode = "from_prep"))))
    if (r$status == "ok") return(c(r, list(mode = "from_prep")))
    # else fall through to a full run
  }
  c(time_call(do.call(make_estimate_thunk, c(args, list(mode = "full")))),
    list(mode = "full"))
}

# Optional memory measures. Each RE-RUNS the estimation, so they are off by
# default; profmem in particular makes the run much slower.
measure_peak_ram <- function(thunk) {
  if (!requireNamespace("peakRAM", quietly = TRUE)) return(NA_real_)
  out <- tryCatch(peakRAM::peakRAM(thunk()), error = function(e) NULL)
  if (is.null(out)) NA_real_ else out$Peak_RAM_Used_MiB[1]
}
measure_alloc <- function(thunk) {
  if (!requireNamespace("profmem", quietly = TRUE)) return(NA_real_)
  out <- tryCatch(profmem::profmem(thunk()), error = function(e) NULL)
  if (is.null(out)) NA_real_ else sum(out$bytes, na.rm = TRUE)
}

# Separate measure via bench::mark(): high-resolution min/median time, total R
# allocations (mem_alloc), and GC count over `iterations` runs. This is run
# independently of the system.time measure (time_call), since bench's allocation
# tracking can inflate timing. `iterations` is supplied from profile_goldfish().
empty_bench <- function() {
  list(bench_min_sec = NA_real_, bench_median_sec = NA_real_,
       mem_alloc_bytes = NA_real_, n_gc = NA_integer_, bench_n_itr = NA_integer_)
}
measure_bench <- function(thunk, iterations) {
  if (is.null(iterations) || iterations < 1L ||
      !requireNamespace("bench", quietly = TRUE)) {
    return(empty_bench())
  }
  out <- tryCatch(
    bench::mark(thunk(), iterations = iterations, filter_gc = FALSE,
                memory = TRUE, check = FALSE),
    error = function(e) NULL
  )
  if (is.null(out)) return(empty_bench())
  list(
    bench_min_sec = as.numeric(out$min[1]),
    bench_median_sec = as.numeric(out$median[1]),
    mem_alloc_bytes = as.numeric(out$mem_alloc[1]),
    n_gc = as.integer(out$n_gc[1]),
    bench_n_itr = as.integer(out$n_itr[1])
  )
}

# ---- coefficient / fit extraction -----------------------------------------

extract_coef <- function(fit, version, model_key, engine, interface) {
  if (is.null(fit)) return(NULL)
  if (requireNamespace("broom", quietly = TRUE)) {
    tb <- tryCatch(
      broom::tidy(fit, conf.int = TRUE),
      error = function(e) tryCatch(broom::tidy(fit), error = function(e2) NULL)
    )
    if (!is.null(tb)) {
      tb <- as.data.frame(lapply(tb, function(col) if (is.list(col)) unlist(col) else col))
    }
  } else {
    cf <- tryCatch(stats::coef(fit), error = function(e) NULL)
    se <- tryCatch(sqrt(diag(stats::vcov(fit))), error = function(e) NULL)
    tb <- if (is.null(cf)) NULL else data.frame(
      term = names(cf), estimate = unname(cf),
      std.error = if (is.null(se)) NA_real_ else unname(se)
    )
  }
  if (is.null(tb) || nrow(tb) == 0L) return(NULL)
  tb$pos <- seq_len(nrow(tb))
  cbind(version = version, model = model_key, engine = engine,
        interface = interface, tb, stringsAsFactors = FALSE)
}

# Fit summary, taking n_params / n_events / logLik / AIC / BIC from the goldfish
# glance() tibble (df = n_params, nobs = n_events), with fit-field fallbacks.
extract_glance <- function(fit) {
  out <- list(n_params = NA_integer_, n_events = NA_integer_,
              logLik = NA_real_, AIC = NA_real_, BIC = NA_real_)
  if (is.null(fit)) return(out)
  if (requireNamespace("broom", quietly = TRUE)) {
    g <- tryCatch(as.list(broom::glance(fit)), error = function(e) NULL)
    if (!is.null(g)) {
      out$logLik   <- g$logLik %||% out$logLik
      out$AIC      <- g$AIC %||% out$AIC
      out$BIC      <- g$BIC %||% out$BIC
      out$n_params <- g$df %||% out$n_params
      out$n_events <- g$nobs %||% out$n_events
    }
  }
  if (is.na(out$n_params) && !is.null(fit$nParams)) out$n_params <- fit$nParams[1]
  if (is.na(out$n_events) && !is.null(fit$nEvents)) out$n_events <- fit$nEvents[1]
  if (is.na(out$logLik) && !is.null(fit$logLikelihood)) {
    out$logLik <- as.numeric(fit$logLikelihood)[1]
  }
  out
}

# Convergence diagnostics from the fitted object. goldfish stores
# fit$convergence = c(converged, return_code, final_score) and fit$nIterations.
extract_convergence <- function(fit) {
  out <- list(n_iterations = NA_integer_, return_code = NA_real_,
              converged = NA, final_score = NA_real_)
  if (is.null(fit)) return(out)
  for (nm in c("nIterations", "n_iterations", "iterations")) {
    if (!is.null(fit[[nm]])) { out$n_iterations <- as.integer(fit[[nm]][1]); break }
  }
  conv <- fit[["convergence"]]
  if (!is.null(conv)) {
    cv <- suppressWarnings(as.numeric(conv))
    if (length(cv) >= 1L) out$converged   <- as.logical(cv[1])
    if (length(cv) >= 2L) out$return_code  <- cv[2]
    if (length(cv) >= 3L) out$final_score  <- cv[3]
  }
  if (is.na(out$final_score) && !is.null(fit[["finalScore"]])) {
    out$final_score <- max(abs(as.numeric(fit[["finalScore"]])))
  }
  out
}

# ---- main entry point -----------------------------------------------------

#' Profile a set of models on the currently loaded goldfish build.
#'
#' @param formulas Named list of formulas keyed by model key (any of
#'   `rate`, `rate_ordered`, `choice`, `choice_coordination`, `rem`,
#'   `rem_ordered`, or any custom key present in `model_map`). Keys may be
#'   omitted -- only the supplied formulas are run, so a data object that does
#'   not support a given model simply leaves that key out.
#' @param object Data object from `make_data()` (new interface). May be NULL
#'   for the old interface, where formula objects resolve from the calling
#'   environment.
#' @param version Label stored in every result row. Defaults to the installed
#'   package version.
#' @param interface "new" or "old"; auto-detected from the loaded namespace.
#' @param engines Engines to iterate over. New interface default is
#'   c("default_c", "gather_compute"); the old interface has no engine choice,
#'   so it is collapsed to a single NA run.
#' @param model_map Key -> (model, sub_model) mapping; see
#'   `goldfish_default_model_map()`.
#' @param n_actors Optional override for the mode-1 actor count (used as-is for
#'   every row). When NULL it is derived from the preprocessed object (`split`).
#' @param split When TRUE (default for the new interface), preprocess each model
#'   once and estimate each engine from that preprocessed object. This yields
#'   the preprocessing/estimation time split and `object_size_bytes` WITHOUT
#'   repeating preprocessing, so it costs about one full run per engine -- no
#'   replicate runs are taken (timings are single-shot; mind GC/OS noise on
#'   small models).
#' @param ram,alloc Optionally also measure peak RAM (peakRAM) / total
#'   allocations (profmem). Each RE-RUNS the estimation once more, so both are
#'   FALSE by default; profmem is slow on large data.
#' @param bench_iterations Number of `bench::mark()` iterations for an
#'   additional high-resolution time + allocation + GC-count measure, run
#'   separately from the `system.time` measure (`time_call`). `0` (default)
#'   disables it; set `1` to add a single instrumented run per model x engine,
#'   or a few iterations on small data for a stable `bench_min_sec`. Each
#'   iteration is one extra estimation run, so raise it cautiously as data grows.
#'
#' @return list(summary = <one row per model x engine>, coef = <tidy coefs>).
profile_goldfish <- function(
  formulas,
  object = NULL,
  version = as.character(utils::packageVersion("goldfish")),
  interface = detect_interface(),
  engines = NULL,
  model_map = goldfish_default_model_map(),
  control_coord = list(initial_damping = 40, max_iterations = 30),
  use_ordered_submodel = FALSE,
  n_actors = NULL,
  split = NULL,
  ram = FALSE,
  alloc = FALSE,
  bench_iterations = 0L
) {
  stopifnot(is.list(formulas), length(formulas) > 0L)
  interface <- match.arg(interface, c("new", "old"))
  if (is.null(engines)) {
    engines <- if (interface == "new") c("default_c", "gather_compute") else NA_character_
  }
  if (is.null(split)) split <- (interface == "new")

  keys <- names(formulas)
  unknown <- setdiff(keys, names(model_map))
  if (length(unknown) > 0L) {
    warning("Skipping formula keys not in model_map: ", paste(unknown, collapse = ", "))
  }
  keys <- intersect(keys, names(model_map))
  keys <- keys[!vapply(formulas[keys], is.null, logical(1))]

  summary_rows <- list()
  coef_rows <- list()

  for (key in keys) {
    formula <- formulas[[key]]
    map_entry <- model_map[[key]]
    ordered <- grepl("ordered", key, fixed = TRUE)

    pre <- if (split) {
      goldfish_preprocess(formula, map_entry, object, interface, ordered, use_ordered_submodel)
    } else {
      list(prep = NULL, t_preprocess = NA_real_, object_size_bytes = NA_real_,
           n_actors = NA_integer_, status = "skipped", message = "")
    }
    na <- n_actors %||% pre$n_actors

    for (engine in engines) {
      label <- sprintf("%s / %s / %s", version, key, engine %||% "cran")
      message("Running ", label, " ...")
      est <- goldfish_estimate(
        formula, map_entry, object, interface, engine, pre$prep,
        control_coord, ordered, use_ordered_submodel
      )
      fit <- est$value
      gl <- extract_glance(fit)
      cv <- extract_convergence(fit)

      t_pre <- pre$t_preprocess
      if (identical(est$mode, "from_prep")) {
        t_est <- est$elapsed
        t_tot <- if (is.na(t_pre)) t_est else t_pre + t_est
      } else {
        t_tot <- est$elapsed
        t_est <- if (!is.na(t_pre) && !is.na(t_tot)) t_tot - t_pre else NA_real_
      }

      alloc_bytes <- NA_real_
      peak_ram <- NA_real_
      bm <- empty_bench()
      if (est$status == "ok" && (alloc || ram || bench_iterations >= 1L)) {
        est_thunk <- make_estimate_thunk(
          formula, map_entry, object, interface, engine, pre$prep,
          control_coord, ordered, use_ordered_submodel,
          mode = est$mode %||% "full"
        )
        if (alloc) alloc_bytes <- measure_alloc(est_thunk)
        if (ram) peak_ram <- measure_peak_ram(est_thunk)
        if (bench_iterations >= 1L) bm <- measure_bench(est_thunk, bench_iterations)
      }

      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        version = version, interface = interface, model = key,
        model_type = map_entry$model,
        sub_model = resolve_sub_model(map_entry, interface, ordered, use_ordered_submodel),
        engine = engine %||% "cran",
        n_actors = na, n_events = gl$n_events, n_params = gl$n_params,
        logLik = gl$logLik, AIC = gl$AIC, BIC = gl$BIC,
        n_iterations = cv$n_iterations, return_code = cv$return_code,
        converged = cv$converged, final_score = cv$final_score,
        t_preprocess_sec = t_pre, t_estimate_sec = t_est, elapsed_sec = t_tot,
        bench_min_sec = bm$bench_min_sec, bench_median_sec = bm$bench_median_sec,
        bench_n_itr = bm$bench_n_itr, n_gc = bm$n_gc,
        object_size_bytes = pre$object_size_bytes,
        mem_alloc_bytes = bm$mem_alloc_bytes,
        alloc_bytes = alloc_bytes, peak_ram_mib = peak_ram,
        fit_mode = est$mode %||% NA_character_, status = est$status,
        message = substr(paste(c(pre$message, est$message), collapse = " "), 1, 300),
        stringsAsFactors = FALSE
      )
      coef_rows[[length(coef_rows) + 1L]] <-
        extract_coef(fit, version, key, engine %||% "cran", interface)
    }
  }

  summary <- do.call(rbind, summary_rows)
  coef <- do.call(rbind, c(Filter(Negate(is.null), coef_rows),
                           make.row.names = FALSE))
  list(summary = summary, coef = coef)
}

# ---- correctness comparison (no extra runs) -------------------------------

#' Coefficient/SE/logLik deltas of every run vs a reference run.
#'
#' Operates on the bound `coef` and `summary` tables from one or more
#' `profile_goldfish()` results -- no estimation is re-run. Aligns coefficients
#' to the reference `by = "position"` (default; robust to term-name changes
#' between the old and new interfaces) or `by = "term"`.
#'
#' @return data frame: one row per version x engine x model with
#'   `max_abs_d_estimate`, `max_abs_d_se`, `d_logLik` relative to the reference.
compare_to_reference <- function(
  coef, summary, ref_version, ref_engine = NULL, by = c("position", "term")
) {
  by <- match.arg(by)
  join_key <- if (by == "position") c("model", "pos") else c("model", "term")
  refmask <- coef$version == ref_version &
    (is.null(ref_engine) | coef$engine %in% (ref_engine %||% coef$engine))
  ref <- coef[refmask, c(join_key, "estimate", "std.error"), drop = FALSE]
  names(ref)[match(c("estimate", "std.error"), names(ref))] <- c("estimate_ref", "se_ref")

  m <- merge(coef, ref, by = join_key, all.x = TRUE)
  m$abs_d_estimate <- abs(m$estimate - m$estimate_ref)
  m$abs_d_se <- abs(m$std.error - m$se_ref)
  agg <- stats::aggregate(
    cbind(max_abs_d_estimate = abs_d_estimate, max_abs_d_se = abs_d_se) ~
      version + engine + model,
    data = m, FUN = max, na.rm = TRUE, na.action = stats::na.pass
  )

  ref_ll <- summary[summary$version == ref_version &
    (is.null(ref_engine) | summary$engine %in% (ref_engine %||% summary$engine)),
    c("model", "logLik"), drop = FALSE]
  names(ref_ll)[2] <- "logLik_ref"
  sll <- merge(summary[, c("version", "engine", "model", "logLik")], ref_ll,
               by = "model", all.x = TRUE)
  sll$d_logLik <- sll$logLik - sll$logLik_ref
  agg <- merge(agg, sll[, c("version", "engine", "model", "d_logLik")],
               by = c("version", "engine", "model"), all.x = TRUE)
  agg[order(agg$model, agg$version, agg$engine), ]
}


# ===========================================================================
# EXAMPLE USAGE (run the block matching the installed interface)
# ===========================================================================
if (FALSE) {

  ## --- NEW interface (develop, refactor/rate_prep / 1.8.0) ---------------
  library(goldfish)
  data("Social_Evolution")
  callNetwork <- make_network(nodes = actors, directed = TRUE)
  callNetwork <- link_events(callNetwork, change_event = calls, nodes = actors)
  callsDependent <- make_dependent_events(
    events = calls, nodes = actors, default_network = callNetwork
  )
  friendshipNetwork <- make_network(nodes = actors, directed = TRUE) |>
    link_events(change_events = friendship, nodes = actors)
  socialEvData <- make_data(callsDependent, callNetwork, calls, actors,
                            friendshipNetwork)

  data("Fisheries_Treaties_6070")
  states <- make_nodes(states)
  states <- link_events(states, sovchanges, attribute = "present")
  states <- link_events(states, regchanges, attribute = "regime")
  states <- link_events(states, gdpchanges, attribute = "gdp")
  bilatnet <- make_network(bilatnet, nodes = states, directed = FALSE) |>
    link_events(bilatchanges, nodes = states)
  contignet <- make_network(contignet, nodes = states, directed = FALSE) |>
    link_events(contigchanges, nodes = states)
  createBilat <- make_dependent_events(
    events = bilatchanges[bilatchanges$increment == 1, ],
    nodes = states, default_network = bilatnet
  )
  fisheriesData <- make_data(createBilat, bilatnet, contignet, states,
                             bilatchanges, contigchanges)

  # One named list per data object. Omit keys that do not apply to the data.
  formulas_social <- list(
    rate         = callsDependent ~ 1 + indeg + outdeg + indeg(friendshipNetwork),
    rate_ordered = callsDependent ~ indeg + outdeg + indeg(friendshipNetwork),
    choice       = callsDependent ~ inertia + recip + trans + tie(friendshipNetwork),
    rem          = callsDependent ~ 1 + indeg + outdeg + trans(callNetwork),
    rem_ordered  = callsDependent ~ indeg + outdeg + trans(callNetwork)
    # no choice_coordination here: needs the Fisheries (undirected) data
  )
  formulas_fisheries <- list(
    choice_coordination = createBilat ~ inertia(bilatnet) + trans(bilatnet) +
      tie(contignet) + alter(states$regime) + diff(states$gdp)
  )

  # Default: single-shot system.time timings, preprocessing once per model,
  # split timing + object.size for free. Optional extra runs:
  #   bench_iterations = n  -> bench::mark() high-res min/median time +
  #                            mem_alloc + GC count (n extra runs; keep small
  #                            for large data, a few for stable small-data time)
  #   alloc = TRUE          -> profmem total allocations (1 extra, slow run)
  #   ram = TRUE            -> peakRAM peak (1 extra run; noisy, R-heap only)
  res_social <- profile_goldfish(formulas_social, object = socialEvData,
                                 bench_iterations = 1)
  res_fish   <- profile_goldfish(formulas_fisheries, object = fisheriesData,
                                 bench_iterations = 1)

  ver <- as.character(packageVersion("goldfish"))
  dir.create(".plan/profvis", showWarnings = FALSE)
  saveRDS(list(summary = rbind(res_social$summary, res_fish$summary),
               coef = rbind(res_social$coef, res_fish$coef)),
          sprintf(".plan/profvis/profile_%s.rds", ver))

  ## --- OLD interface (CRAN 1.6.12) ---------------------------------------
  # Build objects with the old constructors; `object` stays NULL because the
  # formula resolves networks/nodes from the calling environment.
  library(goldfish)
  data("Social_Evolution")
  callNetwork <- defineNetwork(nodes = actors, directed = TRUE)
  callNetwork <- linkEvents(callNetwork, changeEvent = calls, nodes = actors)
  callsDependent <- defineDependentEvents(
    events = calls, nodes = actors, defaultNetwork = callNetwork
  )
  formulas_old <- list(
    rate         = callsDependent ~ 1 + indeg + outdeg,
    rate_ordered = callsDependent ~ indeg + outdeg,
    choice       = callsDependent ~ inertia + recip + trans,
    rem          = callsDependent ~ 1 + indeg + outdeg + trans(callNetwork),
    rem_ordered  = callsDependent ~ indeg + outdeg + trans(callNetwork)
  )
  res_old <- profile_goldfish(formulas_old, object = NULL, interface = "old")
  saveRDS(res_old, sprintf(".plan/profvis/profile_%s.rds",
                           as.character(packageVersion("goldfish"))))

  ## --- COMPARE across versions -------------------------------------------
  files <- list.files(".plan/profvis", pattern = "^profile_.*\\.rds$", full.names = TRUE)
  all <- lapply(files, readRDS)
  summary_all <- do.call(rbind, lapply(all, `[[`, "summary"))
  coef_all    <- do.call(rbind, lapply(all, `[[`, "coef"))

  # Performance: preprocessing vs estimation time and memory by version/engine.
  summary_all[, c("version", "engine", "model", "n_actors", "n_events",
                  "t_preprocess_sec", "t_estimate_sec", "elapsed_sec",
                  "object_size_bytes", "n_iterations")]
  # normalize, e.g. time per event:
  #   summary_all$us_per_event <- 1e6 * summary_all$elapsed_sec / summary_all$n_events

  # Correctness: coefficients/SEs/logLik vs the pre-refactor baseline
  # (1.7.3-prechange is the new interface, so its engine label is "default_c").
  # Aligns by position, so old-interface term names (nodeTrans vs node_trans) do
  # not matter. Expect ~1e-6 or smaller if the refactor preserved estimates.
  compare_to_reference(coef_all, summary_all, ref_version = "1.7.3-prechange",
                       ref_engine = "default_c", by = "position")
}
