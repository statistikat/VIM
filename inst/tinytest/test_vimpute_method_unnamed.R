library(VIM)

## Regression test: precheck() must map an UNNAMED per-column method list to the
## correct variables (Wave 1 tail, audit P2.7). The unnamed-list branch mapped
## via all_methods[seq_along(variables_NA)] (take-first), so a list with one
## method per COLUMN was positionally misassigned: with x1 complete and x2/x3
## missing, list("ranger", "gam", "robust") gave x2 -> "ranger" (x1's slot)
## instead of "gam", silently. The per-NA-variable form (one entry per missing
## column, aligned in order) was already correct and must stay so.
##
## precheck() only validates and maps the method (no model fitting), so this
## needs no Suggests. The m = 1 vimpute result records no method, so the mapping
## is asserted on precheck() directly.

set.seed(1)
d <- data.table::data.table(x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60))
d$x2[sample(60, 10)] <- NA
d$x3[sample(60, 8)]  <- NA

precheck_method <- function(method) {
  pc <- VIM:::precheck(
    data = data.table::copy(d), pmm = FALSE, formula = FALSE, method = method,
    sequential = TRUE, pmm_k = NULL, pmm_k_method = "mean",
    learner_params = NULL, tune = FALSE
  )
  pc$method
}

## one method per COLUMN -> mapped by the NA-variables' column positions
m1 <- precheck_method(list("ranger", "gam", "robust"))
expect_equal(m1[["x2"]], "gam")
expect_equal(m1[["x3"]], "robust")

## reversed order shifts accordingly (proves position matching, not take-first)
m2 <- precheck_method(list("robust", "ranger", "gam"))
expect_equal(m2[["x2"]], "ranger")
expect_equal(m2[["x3"]], "gam")

## one method per NA-VARIABLE -> aligned in order (unchanged; must stay correct)
m3 <- precheck_method(list("gam", "robust"))
expect_equal(m3[["x2"]], "gam")
expect_equal(m3[["x3"]], "robust")
