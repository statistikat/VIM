library(VIM)

## Regression test for complete(vimmi) dispatch through the foreign generics.
## mice and tidyr both export a complete() generic. VIM deliberately exports no
## complete() generic of its own -- one would mask theirs (and be masked by
## them), and would make any package importing VIM and mice/tidyr wholesale emit
## "replacing previous import" at load time. Instead VIM exports vim_complete()
## and registers the same function as a method on mice::complete() and
## tidyr::complete(), so users of those packages keep writing complete(obj, 1).
## This test pins that dispatch.

## VIM must NOT export a complete() generic. Re-exporting one would reintroduce
## the "replacing previous import 'VIM::complete' by 'mice::complete'" warning
## that CRAN flagged in the reverse dependency MIGEE (2026-08-28).
expect_false("complete" %in% getNamespaceExports("VIM"))
expect_true("vim_complete" %in% getNamespaceExports("VIM"))

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
