library(VIM)

## Regression test for tune = TRUE (Wave 1, audit P1).
## Tuning fired only when i == round(nseq / 2). With sequential = FALSE, nseq is
## forced to 1 and round(1/2) == 0, but i starts at 1, so tune = TRUE was a
## silent no-op. Tuning must now execute, observable via tuning_log[[k]]$tuned.

set.seed(1)
res <- vimpute(sleep, method = "ranger", tune = TRUE, sequential = FALSE)

expect_true(is.list(res$tuning_log) && length(res$tuning_log) > 0)
expect_true(
  any(vapply(res$tuning_log, function(e) isTRUE(e$tuned), logical(1))),
  info = "tune = TRUE with sequential = FALSE tuned no variable"
)
