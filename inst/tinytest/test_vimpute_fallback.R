library(VIM)

## Wave 2: per-variable learner fallback (audit P2.6 / P2.66).
## learner$train() ran unguarded, so a learner failure on ONE variable aborted
## the entire imputation with a raw error that did not even name the variable
## (e.g. ranger: "User interrupt or internal error."), losing all completed
## work. Now the failure is caught per variable: a warning names the variable,
## and a featureless learner (mean / class-frequency prediction) imputes that
## variable so the run completes -- mice's never-dies-wholesale behaviour.
##
## Failure is injected deterministically through the public API: mtry = 999
## passes the paradox bounds check but makes ranger::ranger() error at train.

set.seed(1)
n <- 40
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), y_bad = rnorm(n), y_ok = rnorm(n))
d$y_ok[sample(n, 6)] <- NA
d$y_bad[sample(n, 6)] <- NA

## --- numeric target: warn + fall back + complete -----------------------------
warned <- FALSE
res <- NULL
ok <- tryCatch({
  res <- withCallingHandlers(
    vimpute(d, method = "ranger", sequential = FALSE, imp_var = FALSE,
            verbose = FALSE, seed = 2,
            tuned_params = list(y_bad = list(mtry = 999L))),
    warning = function(cond) {
      if (grepl("y_bad", conditionMessage(cond))) warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  TRUE
}, error = function(e) FALSE)
expect_true(ok, info = "a single failing variable still aborts the whole run")
expect_true(warned, info = "fallback warning does not name the failing variable")
if (ok) {
  expect_equal(sum(is.na(res$y_bad)), 0L)
  expect_equal(sum(is.na(res$y_ok)), 0L)
}

## --- factor target: classif fallback path ------------------------------------
set.seed(2)
d2 <- d
d2$y_bad <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
d2$y_bad[sample(n, 6)] <- NA
res2 <- tryCatch(
  suppressWarnings(
    vimpute(d2, method = "ranger", sequential = FALSE, imp_var = FALSE,
            verbose = FALSE, seed = 3,
            tuned_params = list(y_bad = list(mtry = 999L)))
  ),
  error = function(e) NULL
)
expect_true(!is.null(res2), info = "factor-target fallback aborted")
if (!is.null(res2)) {
  expect_equal(sum(is.na(res2$y_bad)), 0L)
  expect_true(all(na.omit(as.character(res2$y_bad)) %in% c("a", "b", "c")))
}

## --- m > 1: the MI path survives a failing variable too ----------------------
res3 <- tryCatch(
  suppressWarnings(
    vimpute(d, method = "ranger", sequential = FALSE, imp_var = FALSE,
            verbose = FALSE, seed = 4, m = 2, boot = TRUE, uncert = "normalerror",
            tuned_params = list(y_bad = list(mtry = 999L)))
  ),
  error = function(e) NULL
)
expect_inherits(res3, "vimmi")
if (inherits(res3, "vimmi")) {
  expect_equal(sum(is.na(complete(res3, 1)$y_bad)), 0L)
}
