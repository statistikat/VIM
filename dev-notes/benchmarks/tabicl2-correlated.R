# Small, reproducible diagnostic experiment. Source this file, then call
# tabicl_correlated_run(); sourcing never initializes Python or trains models.
tabicl_correlated_data <- function(n = 160L, seed = 20261016L) {
  stopifnot(length(n) == 1L, is.finite(n), n >= 40L, n == as.integer(n))
  set.seed(seed)
  X <- stats::rnorm(n)
  A <- factor(sample(LETTERS[1:2], n, replace = TRUE), levels = LETTERS[1:2])
  B <- factor(sample(LETTERS[2:3], n, replace = TRUE), levels = LETTERS[2:3])
  means <- c("A:B" = -8, "A:C" = -2, "B:B" = 2, "B:C" = 10)
  mu <- unname(means[paste(A, B, sep = ":")])
  # Y | X,A,B ~ N(mu[A,B] + 4*X, 0.5^2).
  # Thus Y | A,B is also normal, with variance 4^2 + 0.5^2.
  data.frame(X, Y = mu + 4 * X + stats::rnorm(n, sd = .5), A, B)
}

tabicl_correlated_run <- function(n = 160L, prop = .2, seed = 20261016L,
                                  config = NULL, backend = NULL, output = NULL,
                                  epochs = 20L) {
  if (!is.numeric(epochs) || length(epochs) != 1L || !is.finite(epochs) ||
      epochs < 1 || epochs != floor(epochs) || epochs > .Machine$integer.max)
    stop("epochs must be a positive integer")
  bench <- new.env(parent = environment())
  paths <- file.path(c(".", "..", "../.."), "dev-notes", "benchmarks", "tabicl2.R")
  paths <- paths[file.exists(paths)]
  if (!length(paths)) stop("Run this development experiment from the VIM source tree")
  sys.source(paths[1L], bench)
  if (is.null(config)) config <- bench$uci_config()
  config$tabicl2_finetune$epochs <- as.integer(epochs)
  config$n <- n
  config$methods <- c("vimpute_tabicl2", "vimpute_tabicl2_finetune",
                     "vimpute_tabicl2_finetune_seq10")
  truth <- tabicl_correlated_data(n, seed)
  targets <- "Y"
  incomplete <- VIM::makeMissing(truth, mechanism = "MCAR", vars = targets,
                                 prop = prop, seed = seed + 1L)
  mask <- attr(incomplete, "where")
  if (is.null(backend)) backend <- VIM:::.tabicl_import()
  calls <- list()
  wrap <- function(kind) {
    force(kind)
    function(...) {
      args <- list(...)
      calls[[length(calls) + 1L]] <<- list(kind = kind, args = args)
      do.call(backend[[kind]], args)
    }
  }
  constructors <- c("TabICLRegressor", "TabICLClassifier",
                    "FinetunedTabICLRegressor", "FinetunedTabICLClassifier")
  config$tabicl2$tabicl_module <- setNames(lapply(constructors, wrap), constructors)
  methods <- bench$uci_methods(config)
  completed <- metrics <- fits <- list()
  for (method in names(methods)) {
    message("Correlated MCAR: ", method)
    calls <- list()
    started <- proc.time()[["elapsed"]]
    ans <- methods[[method]](incomplete, targets, seed + 2L)
    elapsed <- proc.time()[["elapsed"]] - started
    stopifnot(identical(names(ans), names(truth)), identical(dim(ans), dim(truth)),
              !anyNA(ans))
    for (v in names(truth)) {
      observed <- !mask[, v]
      stopifnot(identical(ans[[v]][observed], truth[[v]][observed]))
      numeric <- is.numeric(truth[[v]])
      if (numeric) stopifnot(all(is.finite(ans[[v]])))
      else stopifnot(identical(levels(ans[[v]]), levels(truth[[v]])))
    }
    completed[[method]] <- ans
    fits[[method]] <- calls
    metrics[[method]] <- do.call(rbind, lapply(targets, function(v) {
      take <- mask[, v]; a <- truth[[v]][take]; b <- ans[[v]][take]
      numeric <- is.numeric(a)
      data.frame(method, variable = v, n_masked = sum(take),
        rmse = if (numeric) sqrt(mean((b - a)^2)) else NA_real_,
        mae = if (numeric) mean(abs(b - a)) else NA_real_,
        nrmse = if (numeric) sqrt(mean((b - a)^2)) / stats::sd(a) else NA_real_,
        accuracy = if (!numeric) mean(b == a) else NA_real_,
        elapsed, model_fits = length(calls),
        sweeps = length(calls) / length(targets))
    }))
  }
  pairs <- utils::combn(names(completed), 2, simplify = FALSE)
  differences <- do.call(rbind, lapply(pairs, function(pair) {
    do.call(rbind, lapply(targets, function(v) {
      take <- mask[, v]
      a <- completed[[pair[1]]][[v]][take]; b <- completed[[pair[2]]][[v]][take]
      numeric <- is.numeric(a)
      data.frame(first = pair[1], second = pair[2], variable = v,
        fraction_changed = if (numeric) mean(abs(b-a) > 1e-8) else mean(b != a),
        mean_absolute_change = if (numeric) mean(abs(b-a)) else NA_real_,
        max_absolute_change = if (numeric) max(abs(b-a)) else NA_real_)
    }))
  }))
  # Do not serialize the wrapped backend or Python estimator objects.
  config$tabicl2$tabicl_module <- NULL
  result <- list(truth = truth, incomplete = incomplete, mask = mask,
    completed = completed, metrics = do.call(rbind, metrics),
    differences = differences, fit_calls = fits, config = config,
    design = list(n = n, prop = prop, seed = seed, missing_variables = targets,
      model = "Y | X,A,B ~ N(mu[A,B] + 4*X, 0.5^2)",
      group_means = c("A:B" = -8, "A:C" = -2, "B:B" = 2, "B:C" = 10)))
  if (!is.null(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, file.path(output, "experiment.rds"))
    utils::write.csv(result$metrics, file.path(output, "metrics.csv"), row.names = FALSE)
    utils::write.csv(result$differences, file.path(output, "differences.csv"), row.names = FALSE)
  }
  result
}

# Independent datasets/masks, paired methods, and atomic per-repetition caches.
# rep_ids permits disjoint workers; all workers must use the same specification.
tabicl_correlated_repetitions <- function(output, repetitions = 50L,
    rep_ids = seq_len(repetitions), n = 160L, prop = .2, seed = 20261016L,
    epochs = 20L, config = NULL, backend = NULL) {
  valid_integer <- function(z) is.numeric(z) && length(z) == 1L &&
    is.finite(z) && z >= 1 && z <= .Machine$integer.max && z == floor(z)
  if (!valid_integer(repetitions) || !valid_integer(epochs) ||
      !valid_integer(seed) || seed + 100 * (repetitions - 1) + 2 > .Machine$integer.max)
    stop("repetitions, epochs and seed must be valid positive integers")
  if (!is.numeric(rep_ids) || anyNA(rep_ids) || any(!is.finite(rep_ids)) ||
      any(rep_ids != floor(rep_ids) | rep_ids < 1 | rep_ids > repetitions) ||
      anyDuplicated(rep_ids)) stop("Invalid repetition IDs")
  paths <- file.path(c(".", "..", "../.."), "dev-notes", "benchmarks", "tabicl2.R")
  paths <- paths[file.exists(paths)]
  if (!length(paths)) stop("Run this development experiment from the VIM source tree")
  directory <- dirname(normalizePath(paths[1L]))
  root <- dirname(dirname(directory))
  bench <- new.env(parent = environment()); sys.source(paths[1L], bench)
  if (is.null(config)) config <- bench$uci_config()
  config$n <- as.integer(n)
  config$repetitions <- as.integer(repetitions)
  config$methods <- c("vimpute_tabicl2", "vimpute_tabicl2_finetune",
                     "vimpute_tabicl2_finetune_seq10")
  config$tabicl2_finetune$epochs <- as.integer(epochs)
  files <- c("R/tabiclImpute.R", "R/helper_vimpute.R",
             "dev-notes/benchmarks/tabicl2.R", "dev-notes/benchmarks/tabicl2-correlated.R")
  hashes <- setNames(unname(tools::md5sum(file.path(root, files))), files)
  spec <- list(protocol = "tabicl-correlated-y-only-mcar-repetitions-v1",
    repetitions = as.integer(repetitions), n = as.integer(n), prop = prop,
    seed = as.integer(seed), seed_stride = 100L, epochs = as.integer(epochs),
    config = config, code_hashes = hashes,
    backend = if (is.null(backend)) "python" else "supplied")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(output, "config.rds")
  if (file.exists(path)) {
    if (!identical(spec, readRDS(path))) stop("Cannot resume with a different experiment specification")
  } else {
    # Initialize once with rep_ids=integer(0) before starting parallel workers.
    saveRDS(spec, path)
  }
  for (id in rep_ids) {
    folder <- file.path(output, sprintf("rep%03d", id))
    path <- file.path(folder, "experiment.rds")
    run_seed <- as.integer(seed + 100L * (id - 1L))
    if (file.exists(path)) {
      saved <- readRDS(path)
      if (!identical(saved$design$seed, run_seed) || nrow(saved$metrics) != 3L)
        stop("Invalid cached repetition: ", id)
      next
    }
    message("Repetition ", id, "/", repetitions, " (seed ", run_seed, ")")
    # Errors remain visible and do not create a completed-repetition marker.
    result <- tabicl_correlated_run(n = n, prop = prop, seed = run_seed,
      config = config, backend = backend, epochs = epochs)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, paste0(path, ".tmp"))
    if (!file.rename(paste0(path, ".tmp"), path)) stop("Checkpoint write failed")
  }
  invisible(output)
}

# Monte Carlo summaries across independent repetitions, with paired RMSE changes.
# Negative second-minus-first differences favor the second method.
tabicl_correlated_summary <- function(output, allow_incomplete = FALSE) {
  spec <- readRDS(file.path(output, "config.rds"))
  paths <- file.path(output, sprintf("rep%03d", seq_len(spec$repetitions)), "experiment.rds")
  if (!allow_incomplete && any(!file.exists(paths))) stop("Experiment is incomplete")
  ids <- which(file.exists(paths))
  if (!length(ids)) stop("No completed repetitions")
  results <- lapply(paths[ids], readRDS)
  metrics <- do.call(rbind, lapply(seq_along(results), function(j)
    cbind(repetition = ids[j], results[[j]]$metrics)))
  interval <- function(z) {
    if (length(z) < 2L) return(c(NA_real_, NA_real_))
    mean(z) + c(-1, 1) * stats::qt(.975, length(z) - 1L) * stats::sd(z) / sqrt(length(z))
  }
  methods <- spec$config$methods
  summary <- do.call(rbind, lapply(methods, function(method) {
    z <- metrics[metrics$method == method, ]
    ci <- interval(z$rmse)
    data.frame(method, repetitions = nrow(z), mean_rmse = mean(z$rmse),
      sd_rmse = stats::sd(z$rmse), mcse_rmse = stats::sd(z$rmse) / sqrt(nrow(z)),
      mean_rmse_ci_low = ci[1L], mean_rmse_ci_high = ci[2L],
      mean_mae = mean(z$mae), mean_nrmse = mean(z$nrmse),
      mean_seconds = mean(z$elapsed), mean_sweeps = mean(z$sweeps))
  }))
  paired <- do.call(rbind, lapply(utils::combn(methods, 2, simplify = FALSE), function(pair) {
    a <- metrics[metrics$method == pair[1L], c("repetition", "rmse")]
    b <- metrics[metrics$method == pair[2L], c("repetition", "rmse")]
    both <- merge(a, b, by = "repetition", suffixes = c("_first", "_second"))
    delta <- both$rmse_second - both$rmse_first
    ci <- interval(delta)
    data.frame(first = pair[1L], second = pair[2L], repetitions = length(delta),
      mean_rmse_difference = mean(delta), sd_rmse_difference = stats::sd(delta),
      ci_low = ci[1L], ci_high = ci[2L],
      second_wins = sum(delta < -1e-8), ties = sum(abs(delta) <= 1e-8),
      first_wins = sum(delta > 1e-8))
  }))
  list(config = spec, metrics = metrics, summary = summary, paired = paired)
}
