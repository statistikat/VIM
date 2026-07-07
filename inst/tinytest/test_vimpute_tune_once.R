library(VIM)

## Wave 2: tuning architecture (audit P1.30 / P2 "tuning rewire").
## Previously the m > 1 wrapper recursed with tune = tune, so every one of the
## m runs re-tuned independently -- m x the tuning cost, and each imputation
## could pick different hyperparameters, conflating tuner noise with
## missing-data uncertainty (statistically wrong for Rubin pooling; mice tunes
## once). Now: run 1 tunes, its chosen parameters are passed to runs 2..m via
## the new `tuned_params` argument, and the vimmi carries run 1's tuning_log.

data(sleep, package = "VIM")
d <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]

## --- mechanism: tuned_params pre-seeds the cache and is applied -------------
## With tune = FALSE the tuning gate cannot fire, so a difference in the result
## can only come from the supplied parameters being applied to the learner.
p_small <- list(Sleep = list(num.trees = 5L, min.node.size = 10L,
                             sample.fraction = 0.8))
out_default <- vimpute(d, method = "ranger", sequential = FALSE,
                       seed = 42, imp_var = FALSE, verbose = FALSE)
out_params <- vimpute(d, method = "ranger", sequential = FALSE,
                      seed = 42, imp_var = FALSE, verbose = FALSE,
                      tuned_params = p_small)
expect_false(identical(out_default$Sleep, out_params$Sleep),
             info = "tuned_params had no effect on the imputation")
## reproducible with the same seed + params
out_params2 <- vimpute(d, method = "ranger", sequential = FALSE,
                       seed = 42, imp_var = FALSE, verbose = FALSE,
                       tuned_params = p_small)
expect_identical(out_params, out_params2)

## unknown variable names in tuned_params must error (typo protection)
expect_error(
  vimpute(d, method = "ranger", sequential = FALSE, verbose = FALSE,
          tuned_params = list(Sleeep = list(num.trees = 5L))),
  "Unknown variable"
)

## --- integration: m > 1 with tune = TRUE tunes once, surfaces the report ----
set.seed(1)
r <- vimpute(d, method = "ranger", sequential = FALSE, m = 2,
             boot = TRUE, uncert = "normalerror", tune = TRUE,
             seed = 1, imp_var = FALSE, verbose = FALSE)
expect_inherits(r, "vimmi")
expect_true(!is.null(r$tuning_log) && length(r$tuning_log) > 0,
            info = "vimmi lost run 1's tuning report")
## every logged entry carries the parameters that all m runs then share
vars_logged <- vapply(r$tuning_log, function(e) e$variable, character(1))
expect_true(all(c("Sleep", "Dream", "Span") %in% vars_logged))
has_params <- vapply(r$tuning_log, function(e) !is.null(e$params), logical(1))
expect_true(any(has_params), info = "tuning_log entries carry no params")
## the m imputations still differ (uncertainty is preserved)
expect_false(identical(r$imp[["Sleep"]][[1]], r$imp[["Sleep"]][[2]]))

## --- tune-once wall-clock guard (dev machines only; skipped on CRAN) --------
if (at_home()) {
  t1 <- system.time(
    vimpute(d, method = "ranger", sequential = FALSE, m = 1, tune = TRUE,
            seed = 3, imp_var = FALSE, verbose = FALSE)
  )["elapsed"]
  t2 <- system.time(
    vimpute(d, method = "ranger", sequential = FALSE, m = 2, boot = TRUE,
            uncert = "normalerror", tune = TRUE, seed = 3,
            imp_var = FALSE, verbose = FALSE)
  )["elapsed"]
  ## re-tuning per run would give t2 ~ 2 x t1; tune-once gives ~1.0-1.3 x
  expect_true(t2 < 1.75 * t1 + 1,
              info = sprintf("m=2 tuned run took %.1fs vs %.1fs for m=1 (re-tuning?)", t2, t1))
}
