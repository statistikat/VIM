## VIM suggests the mlr3 stack rather than importing it: it backs vimpute()
## alone, and a hard dependency would put every reverse dependency of VIM at
## the mercy of the mlr3 chain. This file exercises that path, so it cannot
## run when the stack is absent -- e.g. under _R_CHECK_DEPENDS_ONLY_=true.
if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                  "paradox", "R6", "future"),
                requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed (VIM only suggests it)")

## Skipped on CRAN (check-time budget): runs locally and on CI with NOT_CRAN=true
## (devtools::check()/test(), GitHub Actions) -- see tests/tinytest.R.
if (!at_home()) exit_file("skipped on CRAN: long-running (run with NOT_CRAN=true)")

library(VIM)

## Regression test for tune = TRUE (Wave 1, audit P1).
## Tuning fired only when i == round(nseq / 2). With sequential = FALSE, nseq is
## forced to 1 and round(1/2) == 0, but i starts at 1, so tune = TRUE was a
## silent no-op. Tuning must now execute, observable via tuning_log[[k]]$tuned.

set.seed(1)
res <- vimpute(sleep, method = "ranger", tune = TRUE, sequential = FALSE)

## type-stable contract: the tuning report is an attribute on the data
tl <- attr(res, "tuning_log")
expect_true(is.list(tl) && length(tl) > 0)
expect_true(
  any(vapply(tl, function(e) isTRUE(e$tuned), logical(1))),
  info = "tune = TRUE with sequential = FALSE tuned no variable"
)
