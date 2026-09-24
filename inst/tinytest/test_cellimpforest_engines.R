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

# --- xgboost engine ---
set.seed(43); n <- 300
X <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n),
                g = factor(sample(c("a", "b", "c"), n, TRUE)))
y <- X$x1^2 + (X$g == "b") + stats::rnorm(n)
y[5] <- y[5] + 10
f <- VIM:::.cif_fit_xgboost(y, X, K = 5, nrounds = 100)
expect_true(y[5] - f$oob_pred[5] > 7)                 # cross-fitted: the fold never saw the cell
expect_null(f$oob_prob)
expect_equal(length(f$predict(X[1:4, ])), 4L)
# review focus 5: a prediction set that lost a level keeps the training columns
mm <- VIM:::.cif_onehot(X)
sub <- X[X$g != "c", ]
sub$g <- droplevels(sub$g)
expect_equal(colnames(VIM:::.cif_onehot(sub, attr(mm, "template"))), colnames(mm))
expect_true(all(VIM:::.cif_onehot(sub, attr(mm, "template"))[, "gc"] == 0))
expect_equal(length(f$predict(sub[1:3, ])), 3L)
X1 <- X; X1$one <- factor("z")                         # single-level factor predictors are dropped
expect_false("onez" %in% colnames(VIM:::.cif_onehot(X1)))
# categorical response
yc <- factor(ifelse(X$x1 + stats::rnorm(n, sd = 0.3) > 0, "hi", "lo"))
fc <- VIM:::.cif_fit_xgboost(yc, X, K = 3, nrounds = 50)
expect_equal(colnames(fc$oob_prob), levels(yc))
expect_true(mean(fc$predict(X) == yc) > 0.85)
# review fix 1: a missing predictor cell must not shift row alignment
set.seed(44); n <- 60
X_na <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n),
                    g = factor(sample(c("a", "b", "c"), n, TRUE)))
y_na <- X_na$x1^2 + (X_na$g == "b") + stats::rnorm(n)
X_na$x1[3] <- NA                                       # NA in a numeric column
X_na$g[7] <- NA                                        # NA in a factor column
expect_equal(nrow(VIM:::.cif_onehot(X_na)), nrow(X_na))
f_na <- VIM:::.cif_fit_xgboost(y_na, X_na, K = 3, nrounds = 30)
expect_equal(length(f_na$oob_pred), length(y_na))
complete <- stats::complete.cases(X_na)
expect_false(anyNA(f_na$oob_pred[complete]))
expect_equal(length(f_na$predict(X_na)), nrow(X_na))
