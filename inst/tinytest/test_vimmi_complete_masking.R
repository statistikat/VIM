library(VIM)

## Regression test for complete(vimmi) generic masking (Wave 1, audit P1).
## VIM defines its own complete() S3 generic; mice and tidyr export a generic of
## the same name. The docs tell users to load mice to pool, which masks VIM's
## generic so complete(vimmi_obj) errored with "no applicable method". The vimmi
## method is now registered on the mice/tidyr generics in .onLoad, so their
## generic dispatches to it regardless of load order.

if (!requireNamespace("mice", quietly = TRUE)) {
  exit_file("mice not available")
}

set.seed(1)
mi <- vimpute(sleep, method = "ranger", m = 3, boot = TRUE, uncert = "resid",
              sequential = FALSE)
expect_equal(class(mi)[1], "vimmi")

## mice's complete generic must dispatch to VIM's vimmi method
d1 <- mice::complete(mi, 1)
expect_true(is.data.frame(d1))
expect_equal(nrow(d1), nrow(sleep))
expect_equal(sum(is.na(d1)), 0L)

## and the same via tidyr's generic
if (requireNamespace("tidyr", quietly = TRUE)) {
  d2 <- tidyr::complete(mi, 1)
  expect_equal(nrow(d2), nrow(sleep))
  expect_equal(sum(is.na(d2)), 0L)
}
