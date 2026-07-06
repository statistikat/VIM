library(VIM)

## Regression test for regressionImp lm/glm dispatch (Wave 1, audit P1).
## The docs promise lm/glm, but the code used glmnet (regularized, lambda=0.01)
## for numeric responses with >= 2 predictors. Resolution (user choice): use
## lm/glm as documented, falling back to glmnet only when the design is
## rank-deficient; binary responses use glm.

set.seed(1)
n <- 80
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d$y <- 1.5 * d$x1 - 0.8 * d$x2 + rnorm(n, 0, 0.4)
mi <- c(4, 11, 19, 28, 40, 55, 70)
d$y[mi] <- NA

## multi-predictor numeric must now use lm -> matches a direct lm fit exactly
cc <- !is.na(d$y)
lm_pred <- predict(lm(y ~ x1 + x2, d[cc, ]), newdata = d[mi, ])
out <- regressionImp(y ~ x1 + x2, d)
expect_equal(as.numeric(out$y[mi]), as.numeric(lm_pred), tolerance = 1e-8)

## rank-deficient predictors -> glmnet fallback: still imputes, no NA, no error
if (requireNamespace("glmnet", quietly = TRUE)) {
  d2 <- d
  d2$x3 <- d2$x1                    # perfect collinearity -> lm rank-deficient
  out2 <- regressionImp(y ~ x1 + x2 + x3, d2)
  expect_equal(sum(is.na(out2$y)), 0L)
}

## binary response -> glm, valid factor levels
set.seed(2)
db <- data.frame(a = rnorm(n), b = rnorm(n))
db$g <- factor(ifelse(plogis(1.2 * db$a - db$b) > runif(n), "yes", "no"))
gm <- c(5, 15, 30, 50, 70)
db$g[gm] <- NA
outb <- regressionImp(g ~ a + b, db)
expect_true(all(!is.na(outb$g)))
expect_true(all(as.character(outb$g[gm]) %in% c("yes", "no")))
