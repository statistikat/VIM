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
                                  config = NULL, backend = NULL, output = NULL) {
  bench <- new.env(parent = environment())
  sys.source(system.file("benchmarks", "tabicl2.R", package = "VIM"), bench)
  if (is.null(config)) config <- bench$uci_config()
  config$n <- n
  config$methods <- c("vimpute_tabicl2", "vimpute_tabicl2_finetune",
                     "vimpute_tabicl2_finetune_seq10")
  truth <- tabicl_correlated_data(n, seed)
  incomplete <- VIM::makeMissing(truth, mechanism = "MCAR", vars = names(truth),
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
    ans <- methods[[method]](incomplete, names(truth), seed + 2L)
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
    metrics[[method]] <- do.call(rbind, lapply(names(truth), function(v) {
      take <- mask[, v]; a <- truth[[v]][take]; b <- ans[[v]][take]
      numeric <- is.numeric(a)
      data.frame(method, variable = v, n_masked = sum(take),
        rmse = if (numeric) sqrt(mean((b - a)^2)) else NA_real_,
        mae = if (numeric) mean(abs(b - a)) else NA_real_,
        nrmse = if (numeric) sqrt(mean((b - a)^2)) / stats::sd(a) else NA_real_,
        accuracy = if (!numeric) mean(b == a) else NA_real_,
        elapsed, model_fits = length(calls),
        sweeps = length(calls) / ncol(truth))
    }))
  }
  pairs <- utils::combn(names(completed), 2, simplify = FALSE)
  differences <- do.call(rbind, lapply(pairs, function(pair) {
    do.call(rbind, lapply(names(truth), function(v) {
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
    design = list(n = n, prop = prop, seed = seed,
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
