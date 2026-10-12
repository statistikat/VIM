# Pure-R protocol checks: no network, Python, training or optional learners.
e <- new.env(parent = baseenv())
# The script intentionally uses stats/utils explicitly except standard base calls.
sys.source(system.file('benchmarks', 'uci-transformer.R', package = 'VIM'), envir = e)
set.seed(81)
d <- data.frame(age = seq_len(100), value = rnorm(100),
                group = factor(rep(c('no', 'yes'), 50)))
a <- VIM::makeMissing(d, prop = .2, mechanism = 'MAR',
                      vars = c('value', 'group'), weights = c(age = 1), seed = 12)
b <- VIM::makeMissing(d, prop = .2, mechanism = 'MAR',
                      vars = c('value', 'group'), weights = c(age = 1), seed = 12)
mask <- attr(a, 'where')
expect_identical(a, b)
expect_false(any(mask[, 'age']))
expect_equal(unname(colSums(mask)), c(0, 20, 20))
expect_identical(is.na(a), mask)
d2 <- d; d2$value <- rev(d2$value); d2$group <- rev(d2$group)
expect_identical(mask, attr(VIM::makeMissing(d2, prop = .2, mechanism = 'MAR',
  vars = c('value', 'group'), weights = c(age = 1), seed = 12), 'where'))
perfect <- e$uci_score(d, d, mask, c('value', 'group'))
expect_equal(perfect$rmse[1], 0)
expect_equal(perfect$accuracy[2], 1)
expect_equal(perfect$marginal_distance, c(0, 0))
completed <- d
completed$value[mask[, 'value']] <- completed$value[mask[, 'value']] + 2
completed$group[mask[, 'group']] <- ifelse(d$group[mask[, 'group']] == 'yes', 'no', 'yes')
s <- e$uci_score(d, completed, mask, c('value', 'group'))
expect_equal(s$rmse[1], 2)
expect_equal(s$mae[1], 2)
expect_equal(s$nrmse[1], 2 / sd(d$value[mask[, 'value']]))
expect_equal(s$nrmse_reference[1], 2 / sd(d$value))
expect_equal(s$nrmse[1], VIM::nrmse(d$value, completed$value, mask[, 'value']))
expect_equal(s$accuracy[2], 0)
expect_equal(s$misclassification[2], 1)
expect_equal(s$mean_difference[1], 2 * mean(mask[, 'value']))
completed$age[1] <- -1
expect_error(e$uci_score(d, completed, mask, c('value', 'group')), 'Changed observed')
completed <- d; completed$value[which(mask[, 'value'])[1]] <- NA
expect_error(e$uci_score(d, completed, mask, c('value', 'group')), 'Invalid completion')
expect_error(e$uci_prepare(tempfile(), download = FALSE), 'Missing archive')

# VIM's masked-reference NRMSE is undefined for a constant masked truth.
flat <- d; flat$value[mask[, 'value']] <- 1
out <- flat; out$value[mask[, 'value']] <- 2
flat_score <- e$uci_score(flat, out, mask, c('value', 'group'))
expect_true(is.na(flat_score$nrmse[1]))
expect_equal(flat_score$rmse[1], 1)
expect_true(is.finite(flat_score$nrmse_reference[1]))

# Size validation and collection must not pool otherwise identical scenarios.
expect_identical(e$uci_config()$n, c(240L, 500L, 1000L, 2000L, 5000L))
for (bad in list(c(240, 240), c(240, NA), 49, 240.5, Inf, numeric(), "240"))
  expect_error(e$uci_validate_sizes(bad), "distinct integer")

# Exercise the runner and its checkpoints with deterministic, cheap stand-ins.
runner <- new.env(parent = globalenv())
sys.source(system.file("benchmarks", "uci-transformer.R", package = "VIM"),
           envir = runner)
runner$uci_prepare <- function(...) list(
  data = list(Adult = data.frame(age = seq_len(100),
    hours_per_week = seq_len(100), income = factor(rep(c("low", "high"), 50))),
    Bank = data.frame(age = seq_len(100), balance = seq_len(100),
                     y = factor(rep(c("no", "yes"), 50)))),
  audit = data.frame(dataset = c("Adult", "Bank"), rows = 100),
  sources = data.frame(url = "test fixture", md5 = "test fixture"))
calls <- 0L
runner$uci_methods <- function(config) list(kNN = function(d, targets, seed) {
  calls <<- calls + 1L
  for (v in targets) {
    missing <- is.na(d[[v]])
    d[[v]][missing] <- if (is.numeric(d[[v]])) median(d[[v]], na.rm = TRUE) else
      levels(d[[v]])[1L]
  }
  d
})
config <- runner$uci_config()
config$n <- c(50L, 60L); config$methods <- "kNN"
config$repetitions <- 1L; config$mechanisms <- "MAR"; config$rates <- .2
directory <- tempfile()
runner$uci_run(config, output = directory)
runs <- read.csv(file.path(directory, "runs.csv"))
metrics <- read.csv(file.path(directory, "metrics.csv"))
expect_identical(calls, 4L)
expect_equal(as.integer(table(runs$n)), c(2L, 2L))
expect_true(all(runs$status == "ok"))
expect_equal(metrics$n_masked, metrics$n * .2)
expect_equal(sort(unique(read.csv(file.path(directory, "distributions.csv"))$n)),
             c(50, 60))
runner$uci_run(config, output = directory, resume = TRUE)
expect_identical(calls, 4L)
expect_identical(read.csv(file.path(directory, "runs.csv")), runs)
# A size interrupted after its last scenario resumes from the scenario checkpoint.
unlink(file.path(directory, "n60", "complete.rds"))
runner$uci_run(config, output = directory, resume = TRUE)
expect_identical(calls, 4L)
expect_identical(read.csv(file.path(directory, "runs.csv")), runs)
changed <- config; changed$rates <- .4
expect_error(runner$uci_run(changed, output = directory, resume = TRUE),
             "different configuration")
expect_error(runner$uci_run(config, output = directory), "resume=TRUE")
for (nr in config$n) {
  artifacts <- readRDS(file.path(directory, paste0("n", nr), "artifacts.rds"))
  expect_true(all(vapply(artifacts, function(a) nrow(a$truth) == nr, logical(1))))
}
# Adding a method must extend each size without replacing existing scores.
original_methods <- runner$uci_methods
runner$uci_methods <- function(config) {
  f <- original_methods(config)[[1L]]
  setNames(rep(list(f), length(config$methods)), config$methods)
}
runner$uci_extend(directory, "ranger")
extended <- read.csv(file.path(directory, "runs.csv"))
expect_identical(calls, 8L)
expect_equal(as.integer(table(extended$n, extended$method)), rep(2L, 4L))
original <- extended[extended$method == "kNN", names(runs)]
rownames(original) <- NULL
expect_identical(original, runs)
config$methods <- c(config$methods, "ranger")
runner$uci_run(config, output = directory, resume = TRUE)
expect_identical(calls, 8L)
# An interrupted extension reuses completed method/scenario checkpoints.
xgb_calls <- 0L
runner$uci_methods <- function(config) list(xgboost = function(d, targets, seed) {
  xgb_calls <<- xgb_calls + 1L
  if (xgb_calls == 2L) stop(structure(list(message = "test interruption", call = NULL),
                                    class = c("interrupt", "condition")))
  original_methods(config)[[1L]](d, targets, seed)
})
interrupted <- tryCatch(runner$uci_extend(directory, "xgboost"),
                       interrupt = function(e) TRUE)
expect_true(isTRUE(interrupted))
runner$uci_extend(directory, "xgboost")
expect_identical(xgb_calls, 5L)
resumed <- read.csv(file.path(directory, "runs.csv"))
expect_equal(nrow(resumed), 12L)
expect_false(anyDuplicated(resumed[c("n", "dataset", "repetition", "mechanism",
                                     "rate", "method")]) > 0L)
expect_true(all(resumed$status == "ok"))
expect_true(all(c("tabimpute", "tabiclImpute", "tabpfnImpute") %in%
                  e$uci_config(foundation = TRUE)$methods))
# Reduced extensions add only requested scenarios, while preserving all core runs.
runner$uci_methods <- function(config)
  setNames(rep(list(original_methods(config)[[1L]]), length(config$methods)), config$methods)
expect_error(runner$uci_extend(directory, "shared", sample_sizes = 75L),
             "existing sample sizes")
runner$uci_extend(directory, "shared", sample_sizes = 50L, rates = .2)
scoped <- read.csv(file.path(directory, "runs.csv"))
expect_equal(nrow(scoped), 14L)
expect_equal(scoped$n[scoped$method == "shared"], c(50, 50))
scoped_config <- readRDS(file.path(directory, "config.rds"))
expect_identical(scoped_config$extensions[[1L]]$n, 50L)
runner$uci_run(scoped_config, output = directory, resume = TRUE)
expect_identical(read.csv(file.path(directory, "runs.csv")), scoped)
# Restarting during final output writes also avoids duplicate scores or fits.
original_summary <- runner$uci_distribution_summary
stop_summary <- TRUE
runner$uci_distribution_summary <- function(...) {
  if (stop_summary) {
    stop_summary <<- FALSE
    stop(structure(list(message = "output interruption", call = NULL),
                   class = c("interrupt", "condition")))
  }
  original_summary(...)
}
prior_calls <- calls
interrupted <- tryCatch(runner$uci_extend(directory, "ranger_seq10"),
                       interrupt = function(e) TRUE)
expect_true(isTRUE(interrupted))
runner$uci_extend(directory, "ranger_seq10")
expect_identical(calls, prior_calls + 4L)
committed <- read.csv(file.path(directory, "runs.csv"))
expect_equal(nrow(committed), 18L)
expect_false(anyDuplicated(committed[c("n", "dataset", "repetition", "mechanism",
                                       "rate", "method")]) > 0L)
unlink(directory, recursive = TRUE)
