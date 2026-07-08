library(VIM)

## Wave 2 (audit P2.63 tail): vimpute_tune_control() exposes the tuning
## budget, resampling folds, tuner and batch size that were hard-coded in
## vimpute()'s tuning block; vimpute_search_space() exports the built-in
## per-learner search spaces (the starting point for search_space hooks in
## register_vimpute_method()). NULL budget/folds keep vimpute's data-size
## heuristics, so defaults are unchanged.

## --- constructor validates its inputs -------------------------------------------
tc <- vimpute_tune_control(budget = 2, folds = 3)
expect_true(inherits(tc, "vimpute_tune_control"))
expect_equal(tc$budget, 2L)
expect_equal(tc$folds, 3L)
expect_equal(tc$tuner, "random_search")
expect_equal(tc$batch_size, 1L)

expect_error(vimpute_tune_control(budget = 0))
expect_error(vimpute_tune_control(folds = 1))
expect_error(vimpute_tune_control(tuner = 42))
expect_error(vimpute_tune_control(batch_size = -1))

## defaults: NULL budget/folds mean "keep vimpute's heuristics"
tc0 <- vimpute_tune_control()
expect_null(tc0$budget)
expect_null(tc0$folds)

## --- built-in search spaces are exported ----------------------------------------
task <- mlr3::TaskRegr$new("t", backend = data.frame(y = rnorm(30), x = rnorm(30)),
                           target = "y")
ss <- vimpute_search_space("regr.ranger", task)
expect_true(inherits(ss$space, "ParamSet"))
expect_true(is.numeric(ss$n_evals))
ss_unknown <- vimpute_search_space("regr.nope", task)
expect_null(ss_unknown$space)

## --- wired through vimpute(): budget/folds respected and logged -----------------
set.seed(9)
n <- 40
dd <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
dd$y <- dd$x1 + rnorm(n, sd = 0.3)
dd$y[sample(n, 7)] <- NA

res <- vimpute(dd, method = "ranger", tune = TRUE, sequential = FALSE,
               uncert = "none", imp_var = FALSE, verbose = FALSE, seed = 5,
               tune_control = vimpute_tune_control(budget = 2, folds = 3))
tl <- attr(res, "tuning_log")
expect_true(length(tl) >= 1L)
entry <- tl[[length(tl)]]
expect_equal(entry$n_evals, 2L,
             info = "tuning_log must record the evaluation budget used")
expect_equal(entry$folds, 3L,
             info = "tuning_log must record the CV folds used")
expect_true(!is.null(entry$params))

## invalid tune_control errors early with a helpful message
expect_error(
  vimpute(dd, method = "ranger", tune = TRUE,
          tune_control = list(budget = 2)),
  pattern = "vimpute_tune_control")

## --- m > 1: the control is forwarded to the (single) tuning run -----------------
mi <- vimpute(dd, method = "ranger", tune = TRUE, m = 2,
              sequential = FALSE, imp_var = FALSE, verbose = FALSE, seed = 5,
              tune_control = vimpute_tune_control(budget = 2, folds = 3))
expect_true(inherits(mi, "vimmi"))
tl_mi <- mi$tuning_log
expect_equal(tl_mi[[length(tl_mi)]]$n_evals, 2L)
