# The "restricted" method's contract is that imputed values satisfy the
# validation rules. Value-level uncertainty draws (uncert = "pmm" default,
# "normalerror", "resid") would replace the solver's rule-satisfying value
# with a donor or noisy value that knows nothing about the rules -- on the
# rebased engine the 7.3.0 uncert = "pmm" default drew donor y = 2 over the
# constrained solution y = 4 under the rule y >= 4 (CI-red on all
# platforms since the restrictionRegression merge). The registry therefore
# pins uncert to "none" for restricted variables: silently for the default,
# with a warning when the user explicitly requested a draw mechanism.
suppressMessages(library(VIM))

if (!requireNamespace("validate", quietly = TRUE) ||
    !requireNamespace("ECOSolveR", quietly = TRUE) ||
    !requireNamespace("mlr3", quietly = TRUE)) {
  exit_file("restricted backend not available")
}

test_df <- data.frame(y = c(1, 2, NA), x = c(1, 2, 3))
rules <- validate::validator(y >= 4)
lp <- list(restricted = list(rules = rules))

## --- default uncert: forced to "none" silently, rules hold ------------------
w_def <- character(0)
imp_def <- withCallingHandlers(
  vimpute(test_df, method = list(y = "restricted"), pmm = FALSE,
          sequential = FALSE, learner_params = lp),
  warning = function(c) { w_def <<- c(w_def, conditionMessage(c)); invokeRestart("muffleWarning") }
)
expect_true(imp_def$y[3] >= 4 - 1e-6,
            info = sprintf("default uncert broke the rule: y[3] = %g", imp_def$y[3]))
expect_false(any(grepl("uncert", w_def)),
             info = "forcing uncert for the DEFAULT must be silent")

## --- explicit draw mechanism: still forced, but with a warning ---------------
expect_warning(
  imp_expl <- vimpute(test_df, method = list(y = "restricted"), pmm = FALSE,
                      uncert = "pmm",
                      sequential = FALSE, learner_params = lp),
  "restricted",
  info = "explicitly requested draw uncert on a restricted variable must warn"
)
expect_true(imp_expl$y[3] >= 4 - 1e-6,
            info = sprintf("explicit uncert broke the rule: y[3] = %g", imp_expl$y[3]))

## --- the solver runs exactly once per restricted variable --------------------
## (no re-solve for the model-quality report, none for PMM scoring)
imp_cnt <- vimpute(test_df, method = list(y = "restricted"), pmm = FALSE,
                   sequential = FALSE,
                   learner_params = list(restricted = list(
                     rules = rules, save_optimization_problem = TRUE)))
probs <- attr(imp_cnt, "restricted_optimization_problems", exact = TRUE)
expect_equal(length(probs$y), 1L,
             info = sprintf("expected 1 solve, got %d", length(probs$y)))

## --- mixed methods: the pin is per-variable, ranger keeps its draws ----------
set.seed(7)
mix <- data.frame(y = c(1, 2, NA), z = c(NA, rnorm(2)), x = c(1, 2, 3))
w_mix <- character(0)
imp_mix <- withCallingHandlers(
  vimpute(mix, method = list(y = "restricted", z = "ranger"), pmm = FALSE,
          sequential = FALSE, learner_params = lp),
  warning = function(c) { w_mix <<- c(w_mix, conditionMessage(c)); invokeRestart("muffleWarning") }
)
expect_true(imp_mix$y[3] >= 4 - 1e-6)
expect_false(anyNA(imp_mix$z))
