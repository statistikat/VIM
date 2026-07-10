library(VIM)

## Regression tests for MI-properness warnings (Wave 1, audit 2026-07-02).
## Policy: warn-only. Defaults and imputed values are unchanged; vimpute must
## warn loudly whenever an m > 1 configuration cannot yield proper
## between-imputation variability, so Rubin's rules are not applied silently to
## degenerate or variance-underestimating imputations.

## helper: collect the warning messages emitted while evaluating expr
.mi_warns <- function(expr) {
  w <- character(0)
  withCallingHandlers(
    expr,
    warning = function(c) {
      w[[length(w) + 1L]] <<- conditionMessage(c)
      invokeRestart("muffleWarning")
    }
  )
  w
}

## --- P0.1: deterministic default PMM (pmm_k = 1, pmm_k_method = "mean") ------
## with m > 1 and no bootstrap yields identical imputations. Previously silent
## because has_any_pmm bypassed the guard; must now warn. (boot = FALSE is set
## explicitly since 7.3.0's adaptive default -- with the m > 1 default
## boot = TRUE, the bootstrap refits vary the predictions and the deterministic
## donor mapping no longer collapses the imputations, so no warning is due.)
set.seed(1)
w_pmm <- .mi_warns(
  vimpute(sleep, method = "ranger", pmm = TRUE, m = 2, boot = FALSE,
          sequential = FALSE)
)
expect_true(any(grepl("identical", w_pmm)))

## --- Guard: the same deterministic PMM under the m > 1 default (boot = TRUE)
## must NOT warn -- bootstrap refits supply the between-imputation variability.
set.seed(1)
w_pmm_boot <- .mi_warns(
  vimpute(sleep, method = "ranger", pmm = TRUE, m = 2, sequential = FALSE)
)
expect_false(any(grepl("identical|underestimate", w_pmm_boot)))

## --- P0.2: bootstrap-only MI (boot = TRUE, uncert = "none", no PMM) ----------
## imputes conditional means, so between-imputation variance is underestimated.
## Previously silent because the guard required !boot; must now warn.
## (uncert = "none" is set explicitly since 7.3.0, when the default became
## "pmm" -- the default configuration is no longer improper.)
set.seed(1)
w_boot <- .mi_warns(
  vimpute(sleep, method = "ranger", boot = TRUE, uncert = "none",
          m = 2, sequential = FALSE)
)
expect_true(any(grepl("underestimate", w_boot)))

## --- Guard: a genuinely stochastic PMM (k >= 2, random draw) must NOT warn ---
set.seed(1)
w_ok_pmm <- .mi_warns(
  vimpute(sleep, method = "ranger", pmm = TRUE, pmm_k = 5,
          pmm_k_method = "random", m = 2, sequential = FALSE)
)
expect_false(any(grepl("identical|underestimate", w_ok_pmm)))

## --- Guard: a proper noise mechanism (uncert) must NOT warn ------------------
set.seed(1)
w_ok_uncert <- .mi_warns(
  vimpute(sleep, method = "ranger", uncert = "normalerror", m = 2,
          sequential = FALSE)
)
expect_false(any(grepl("identical|underestimate", w_ok_uncert)))
