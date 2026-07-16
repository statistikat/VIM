library(VIM)

## The scale and residual pool behind uncert = "normalerror" / "resid" must be
## honest estimates of the LEARNER'S PREDICTIVE spread.
##
## extract_model_info() has no ranger branch, so ranger fits fall through to
## complete_model_info(), which derives both from learner$predict(<training
## task>). In-sample forest predictions are near-interpolating, so their SD
## understates the true predictive SD roughly twofold (0.57 vs 1.16 on the DGP
## below). normalerror then draws N(0, sigma_hat) at half the right scale and
## pooled intervals under-cover -- 0.83 / 0.73 against a nominal 0.95 in the
## R Journal paper's coverage study, where lmrob/gam arms (which expose proper
## scale estimates) are unaffected.
##
## ranger stores out-of-bag predictions on the fitted object for free; those
## track the true predictive SD to ~1%. The same pool also feeds the
## "stratified"/"residual" bootstrap strategies via bootstrap_resample().

if (!requireNamespace("mlr3", quietly = TRUE) ||
    !requireNamespace("mlr3learners", quietly = TRUE) ||
    !requireNamespace("ranger", quietly = TRUE)) {
  exit_file("mlr3 stack not available")
}

suppressMessages({
  library(mlr3)
  library(mlr3learners)
})
lgr::get_logger("mlr3")$set_threshold("warn")

set.seed(1)
mk <- function(n) {
  x <- rnorm(n); z <- rnorm(n)
  data.frame(x = x, z = z, y = 1 + 2 * x - z + rnorm(n))
}
train <- mk(400L)
holdout <- mk(400L)

task <- TaskRegr$new("train", backend = train, target = "y")
learner <- lrn("regr.ranger", num.trees = 500L, num.threads = 1L)
learner$train(task)

## Ground truth: the forest's real predictive spread, measured on fresh data.
sd_holdout <- sd(holdout$y - learner$predict_newdata(holdout)$response)
## What the in-sample path yields (the defect under test).
sd_insample <- sd(train$y - learner$predict(task)$response)

## Guard the DGP itself: if in-sample residuals were not badly optimistic here,
## the test could pass for the wrong reason.
expect_true(sd_insample < 0.75 * sd_holdout,
            info = sprintf("test DGP is wrong: in-sample SD %.3f is not optimistic vs holdout %.3f",
                           sd_insample, sd_holdout))

info <- VIM:::complete_model_info(
  VIM:::extract_model_info(learner, method = "ranger"),
  learner = learner, task = task)

## --- scale: what uncert = "normalerror" draws with ---------------------------
expect_true(is.numeric(info$scale) && length(info$scale) == 1L &&
              is.finite(info$scale),
            info = "no usable scale returned for a ranger fit")
expect_true(abs(info$scale - sd_holdout) < 0.2 * sd_holdout,
            info = sprintf("scale %.3f is not an honest predictive SD (holdout %.3f, in-sample %.3f)",
                           info$scale, sd_holdout, sd_insample))

## --- residual pool: what uncert = "resid" samples from -----------------------
expect_true(is.numeric(info$residuals) && length(info$residuals) > 0L,
            info = "no usable residual pool returned for a ranger fit")
expect_true(abs(sd(info$residuals) - sd_holdout) < 0.2 * sd_holdout,
            info = sprintf("residual pool SD %.3f is not an honest predictive SD (holdout %.3f)",
                           sd(info$residuals), sd_holdout))

## --- learners exposing a proper scale must keep it ---------------------------
## lm reports the model's own sigma; the OOB path must not hijack it.
task_lm <- TaskRegr$new("lm", backend = train, target = "y")
learner_lm <- lrn("regr.lm")
learner_lm$train(task_lm)
info_lm <- VIM:::complete_model_info(
  VIM:::extract_model_info(learner_lm, method = "lm"),
  learner = learner_lm, task = task_lm)
expect_true(abs(info_lm$scale - 1) < 0.15,
            info = sprintf("lm scale %.3f drifted from the true residual sigma of 1",
                           info_lm$scale))
