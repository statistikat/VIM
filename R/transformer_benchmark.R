# Internal benchmark helpers; never run at package load or during normal checks.
.transformer_seed <- function(seed, code) {
  if (is.null(seed)) return(force(code))
  if (length(seed) != 1L || !is.numeric(seed) || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed > .Machine$integer.max || seed != floor(seed))
    stop("seed must be NULL or a non-negative integer")
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had) assign(".Random.seed", old, .GlobalEnv) else
    if (exists(".Random.seed", .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  force(code)
}

.transformer_benchmark_data <- function(n = 200L, seed = 1L) {
  .transformer_seed(seed, {
    x <- rnorm(n)
    group <- factor(ifelse(x + rnorm(n) > 0, "A", "B"))
    data.frame(x = x, y = 2 * x + as.numeric(group) + rnorm(n, sd = .3),
               skew = exp(.4 * x + rnorm(n, sd = .2)), group = group)
  })
}

.transformer_benchmark_mask <- function(data, rate = .2,
                                        mechanism = c("MCAR", "MAR", "MNAR", "block"),
                                        seed = 1L) {
  mechanism <- match.arg(mechanism)
  if (anyNA(data) || nrow(data) < 3L || ncol(data) < 2L)
    stop("Benchmark truth must be complete with at least three rows and two columns")
  if (length(rate) != 1L || !is.finite(rate) || rate <= 0 || rate >= 1)
    stop("rate must lie strictly between zero and one")
  .transformer_seed(seed, {
    mask <- matrix(FALSE, nrow(data), ncol(data), dimnames = list(NULL, names(data)))
    # First column remains observed and supplies the MAR covariate.
    for (j in seq.int(2L, ncol(data))) {
      z <- if (mechanism == "MAR") data[[1L]] else data[[j]]
      z <- as.numeric(z)
      weights <- if (mechanism %in% c("MAR", "MNAR"))
        exp(2 * rank(z) / length(z)) else rep(1, length(z))
      count <- min(nrow(data) - 2L, max(1L, round(nrow(data) * rate)))
      rows <- if (mechanism == "block" && j > 2L) which(mask[, 2L]) else
        sample.int(nrow(data), count, prob = weights)
      mask[rows, j] <- TRUE
    }
    mask
  })
}

.transformer_benchmark_score <- function(truth, completed, mask) {
  completed <- as.data.frame(completed)
  if (!identical(dim(truth), dim(completed)) || !identical(names(truth), names(completed)))
    stop("Completion changed the table schema")
  rows <- lapply(seq_along(truth), function(j) {
    observed <- !mask[, j]
    if (!identical(truth[[j]][observed], completed[[j]][observed]))
      stop("Completion changed observed values or their types: ", names(truth)[j])
    take <- mask[, j]
    if (!any(take)) return(NULL)
    a <- truth[[j]][take]; b <- completed[[j]][take]
    if (anyNA(b) || (is.numeric(b) && any(!is.finite(b))))
      stop("Completion contains missing/non-finite predictions")
    numeric <- is.numeric(a)
    scale <- if (numeric) stats::sd(truth[[j]][observed]) else NA_real_
    data.frame(variable = names(truth)[j], n = sum(take),
      mae = if (numeric) mean(abs(a - b)) else NA_real_,
      rmse = if (numeric) sqrt(mean((a - b)^2)) else NA_real_,
      nrmse = if (numeric && scale > 0) sqrt(mean((a - b)^2)) / scale else NA_real_,
      categorical_error = if (!numeric) mean(as.character(a) != as.character(b)) else NA_real_,
      mean_shift = if (numeric) mean(completed[[j]]) - mean(truth[[j]]) else NA_real_,
      sd_ratio = if (numeric) stats::sd(completed[[j]]) / stats::sd(truth[[j]]) else NA_real_)
  })
  do.call(rbind, rows)
}

.transformer_benchmark <- function(truth, methods, seeds = 1:3, rates = .2,
                                    mechanisms = c("MCAR", "MAR", "MNAR", "block"),
                                    output = NULL) {
  if (!is.list(methods) || !length(methods) || is.null(names(methods)) ||
      any(!nzchar(names(methods))) || anyDuplicated(names(methods)))
    stop("methods must be a uniquely named list of functions or fit/predict specifications")
  results <- list(); artifacts <- list(); i <- 0L
  for (seed in seeds) for (rate in rates) for (mechanism in mechanisms) {
    mask <- .transformer_benchmark_mask(truth, rate, mechanism, seed)
    incomplete <- truth
    for (j in seq_along(incomplete)) incomplete[[j]][mask[, j]] <- NA
    artifacts[[paste(seed, rate, mechanism, sep = "/")]] <- mask
    for (method in names(methods)) {
      i <- i + 1L; fit_seconds <- predict_seconds <- NA_real_
      start <- proc.time()[["elapsed"]]
      outcome <- tryCatch(.transformer_seed(seed, {
        spec <- methods[[method]]
        if (is.function(spec)) {
          prediction <- spec(incomplete)
        } else {
          if (!is.function(spec$fit) || !is.function(spec$predict)) stop("Invalid method specification")
          t <- proc.time()[["elapsed"]]; fitted <- spec$fit(incomplete)
          fit_seconds <- proc.time()[["elapsed"]] - t
          t <- proc.time()[["elapsed"]]; prediction <- spec$predict(fitted, incomplete)
          predict_seconds <- proc.time()[["elapsed"]] - t
        }
        .transformer_benchmark_score(truth, prediction, mask)
      }), error = identity)
      elapsed <- proc.time()[["elapsed"]] - start
      failed <- inherits(outcome, "error")
      scores <- if (failed) data.frame(variable = NA_character_, n = NA_integer_,
        mae = NA_real_, rmse = NA_real_, nrmse = NA_real_, categorical_error = NA_real_,
        mean_shift = NA_real_, sd_ratio = NA_real_) else outcome
      results[[i]] <- cbind(data.frame(method, seed, rate, mechanism,
        status = if (failed) "failed" else "ok",
        error = if (failed) conditionMessage(outcome) else "",
        elapsed, fit_seconds, predict_seconds, peak_memory_bytes = NA_real_), scores)
    }
  }
  report <- list(results = do.call(rbind, results), masks = artifacts, truth = truth,
    protocol = "Transductive completion; first column observed; no tuning on held-out truth",
    metadata = list(session = utils::sessionInfo(), system = Sys.info(),
      threads = Sys.getenv(c("OMP_NUM_THREADS", "TF_NUM_INTRAOP_THREADS")),
      memory_note = "Peak memory not measured; NA must not be interpreted as zero"))
  if (!is.null(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    saveRDS(report, file.path(output, "benchmark.rds"))
    utils::write.csv(report$results, file.path(output, "results.csv"), row.names = FALSE)
  }
  report
}
