library(VIM)

## Regression test for evaluation() (Wave 1, audit P1).
## With the default vartypes = "guess" the function previously matched neither
## the "numeric" nor the "factor" branch, so it silently returned error 0 for
## ANY imputation. It must now infer variable types and report the true error.

set.seed(1)
x <- data.frame(a = rnorm(20), b = rnorm(20))     # complete "truth"
m <- matrix(FALSE, 20, 2)
m[c(2, 5, 9), 1] <- TRUE
m[c(3, 7), 2] <- TRUE
y <- x
y[m] <- x[m] + 3                                   # 5 imputed cells, each off by 3

## default guess must recover the true numeric error: sum(3^2 * 5) / 5 = 9
res <- evaluation(x, y, m)
expect_equal(res$error, 9)
expect_true(res$err_num > 0)

## guessing must agree with explicitly supplied numeric types
res2 <- evaluation(x, y, m, vartypes = c("numeric", "numeric"))
expect_equal(res$error, res2$error)

## a perfect imputation must score 0 (guard against always-nonzero)
expect_equal(evaluation(x, x, m)$error, 0)
