library(VIM)

## Regression tests for three defects reported with the colic data (2026-07):
## (1) imputeCellM() had no data.frame interface (chained equations),
## (2) the DDC weight init in imputeCellIRMI() indexed stdResid past the
##     columns DDC's checkDataSet retained ('subscript out of bounds'),
## (3) name-pasted model formulas broke with duplicated column names (as
##     produced by colnames(x) <- substr(colnames(x), 1, 15)): the response
##     resolved to the wrong column, giving multinom warnings and silent NAs.

if (!requireNamespace("robustbase", quietly = TRUE) ||
    !requireNamespace("nnet", quietly = TRUE)) {
  exit_file("robustbase/nnet not available")
}

## small mixed data set with missings in numeric and factor columns
make_mixed <- function(n = 120, seed = 11) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- 0.8 * x1 + rnorm(n, sd = 0.6)
  x3 <- -0.5 * x1 + 0.5 * x2 + rnorm(n, sd = 0.7)
  f1 <- factor(ifelse(x1 + rnorm(n, sd = 0.8) > 0, "a", "b"))
  f2 <- factor(sample(c("u", "v", "w"), n, replace = TRUE))
  d <- data.frame(x1, x2, x3, f1, f2)
  d$x2[sample(n, 15)] <- NA
  d$x3[sample(n, 12)] <- NA
  d$f1[sample(n, 10)] <- NA
  d$f2[sample(n, 10)] <- NA
  d
}

## ------------------------------------------------------------------
## 1. imputeCellM(data.frame): chained-equations interface
## ------------------------------------------------------------------
d <- make_mixed()
res <- imputeCellM(d)
expect_true(is.list(res))
expect_true(all(!is.na(res$data_imputed)))
expect_identical(dim(res$data_imputed), dim(d))
expect_identical(names(res$data_imputed), names(d))
expect_true(is.factor(res$data_imputed$f1))
expect_identical(levels(res$data_imputed$f1), levels(d$f1))
expect_true(all(res$cellweights >= 0 & res$cellweights <= 1))
expect_identical(dim(res$cellweights), c(nrow(d), ncol(d)))
expect_true(res$iterations >= 1L)
## observed cells must be untouched
obs <- !is.na(d$x2)
expect_identical(res$data_imputed$x2[obs], d$x2[obs])
obs_f <- !is.na(d$f1)
expect_identical(res$data_imputed$f1[obs_f], d$f1[obs_f])

## matrix input is coerced
dm <- as.matrix(d[, 1:3])
res_m <- imputeCellM(dm)
expect_true(all(!is.na(res_m$data_imputed)))

## the dispatcher no longer refuses cellM
res_disp <- imputeCellwise(d, method = "cellM")
expect_true(all(!is.na(res_disp$data_imputed)))

## the single-response formula interface is unchanged
res_f <- imputeCellM(x2 ~ x1 + x3, data = d)
expect_true(all(!is.na(res_f$data_imputed$x2)))
expect_identical(res_f$iterations, 1L)

## ------------------------------------------------------------------
## 2. imputeCellIRMI: DDC init with columns DDC refuses to analyse
## ------------------------------------------------------------------
## A constant column and a 3-value discrete column are dropped by DDC's
## checkDataSet, so stdResid has fewer columns than the continuous block;
## this errored with 'subscript out of bounds' (colic dataset, 2026-07).
if (requireNamespace("cellWise", quietly = TRUE)) {
  d2 <- make_mixed(seed = 22)
  d2$konst <- 2                              # zero scale: dropped by DDC
  d2$disc <- rep(0:2, length.out = nrow(d2)) # <= 3 distinct: dropped by DDC
  res2 <- imputeCellIRMI(d2, maxit = 3)
  expect_true(all(!is.na(res2$data_imputed)))
  expect_true(all(res2$cellweights >= 0 & res2$cellweights <= 1))
  expect_identical(names(res2$data_imputed), names(d2))
}

## ------------------------------------------------------------------
## 3. duplicated column names must not corrupt the imputation
## ------------------------------------------------------------------
## The factor with missings that carries a duplicated name SECOND is the
## harmful case: its pasted formula resolved the response to the first
## column of that name, so the imputed classes came from the wrong level
## set and the factor assignment silently produced NA.
d3 <- make_mixed(seed = 33)
names(d3) <- c("x1", "dup", "dup", "f", "f")

chk <- function(res) {
  expect_true(all(!is.na(res$data_imputed)))
  expect_identical(names(res$data_imputed), names(d3))
  expect_identical(levels(res$data_imputed[[4]]), levels(d3[[4]]))
  expect_identical(levels(res$data_imputed[[5]]), levels(d3[[5]]))
}

chk(imputeCellEM(d3, maxit_em = 5))
chk(imputeCellIRMI(d3, maxit = 3, init_weights = "univariate"))
chk(imputeCellM(d3, maxit = 3))
chk(imputeCellMM(d3, maxit = 3))
chk(imputeCellMCD(d3, maxit = 5))
if (requireNamespace("crmReg", quietly = TRUE)) {
  chk(imputeCellReg(d3, maxit = 3))
}
