# --- ranger engine ---
set.seed(42); n <- 300
X <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
y <- X$x1^2 + stats::rnorm(n)
y[5] <- y[5] + 10                                   # one gross response cell
# out-of-bag residual keeps the outlier; the in-sample residual of a fully grown forest keeps ~37 %
f1 <- VIM:::.cif_fit_ranger(y, X, aggregate = "mean", min.node.size = 1, num.trees = 300)
expect_true(y[5] - f1$oob_pred[5] > 7)
expect_true(y[5] - f1$center(X[5, , drop = FALSE]) < 6)
f1i <- VIM:::.cif_fit_ranger(y, X, aggregate = "mean", residuals = "insample",
                             min.node.size = 1, num.trees = 300)
expect_true(y[5] - f1i$oob_pred[5] < 6)              # the ablation reports the in-sample residual
# imputation uses the median over trees, detection the mean
f2 <- VIM:::.cif_fit_ranger(y, X, num.trees = 100)
expect_equal(length(f2$oob_pred), n)
expect_false(anyNA(f2$oob_pred))
expect_equal(length(f2$predict(X)), n)
expect_false(isTRUE(all.equal(f2$predict(X[1:20, ]), f2$center(X[1:20, ]))))
f3 <- VIM:::.cif_fit_ranger(y, X, num.trees = 50, quantreg = TRUE)
q3 <- stats::predict(f3$rf, data = X[1:4, ], type = "quantiles", quantiles = c(0.1, 0.9))
expect_equal(dim(q3$predictions), c(4L, 2L))
# categorical response: probability forest, levels preserved
yc <- factor(ifelse(X$x1 + stats::rnorm(n, sd = 0.3) > 0, "hi", "lo"))
f4 <- VIM:::.cif_fit_ranger(yc, X, num.trees = 100)
expect_equal(colnames(f4$oob_prob), levels(yc))
expect_true(mean(f4$predict(X) == yc) > 0.85)
expect_equal(levels(f4$predict(X[1:3, ])), levels(yc))
expect_equal(dim(f4$predict_prob(X[1:3, ])), c(3L, 2L))
