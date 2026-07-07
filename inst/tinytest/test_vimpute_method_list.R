library(VIM)

## Regression test for method-list handling (Wave 1, audit P1).
## A length-1 NAMED method list (e.g. list(Dream = "gam")) matched the
## "single global method" branch, which discarded the name and applied the
## method to ALL variables -- so a misspelled variable name passed silently.
## A named list must route to per-variable handling, which validates names.

## a typo'd variable name in a length-1 named list must now error, not be
## silently reinterpreted as a global method
expect_error(
  vimpute(sleep, method = list(Dreammm = "ranger"), sequential = FALSE),
  "Unknown variable"
)

## a correct length-1 named list must be accepted (per-variable) and impute
set.seed(1)
out <- vimpute(sleep, method = list(Dream = "ranger"), sequential = FALSE)
expect_equal(sum(is.na(as.data.frame(out)[, colnames(sleep)])), 0L)
