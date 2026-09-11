# ==========================================================================
# Tests for cellGLoc: cellwise-robust estimation on a categorical mean structure
# ==========================================================================

set.seed(1)
d <- data.frame(x1 = rnorm(60), x2 = rnorm(60),
                f1 = factor(rep(c("a", "b", "c"), each = 20)),
                f2 = factor(rep(c("u", "v"), 30)))

U <- VIM:::.gloc_design(d, ~ ., c("f1", "f2"))
expect_equal(nrow(U), 60)
expect_equal(ncol(U), 4)                      # intercept + 2 (f1) + 1 (f2)
expect_true(all(U[, 1] == 1))

U1 <- VIM:::.gloc_design(d, ~ 1, c("f1", "f2"))
expect_equal(ncol(U1), 1)                     # intercept only

# a missing factor value becomes its own level, never a dropped row
d2 <- d; d2$f1[1:5] <- NA
U2 <- VIM:::.gloc_design(d2, ~ ., c("f1", "f2"))
expect_equal(nrow(U2), 60)
expect_equal(ncol(U2), 5)                     # intercept + 3 (f1 incl. NA) + 1 (f2)

# --- .gloc_update_B recovers a known mean structure when weights are all 1 ---
set.seed(2)
n <- 500
g  <- factor(sample(c("a", "b", "c"), n, TRUE))
U  <- VIM:::.gloc_design(data.frame(g = g), ~ ., "g")
Btrue <- rbind(c(0, 0), c(3, -2), c(-1, 4))          # 3 design cols x 2 responses
X  <- U %*% Btrue + matrix(rnorm(n * 2, sd = 0.1), n, 2)
Bhat <- VIM:::.gloc_update_B(X, U, matrix(1, n, 2))
expect_equal(dim(Bhat), c(3L, 2L))
expect_true(max(abs(Bhat - Btrue)) < 0.05)

# --- a zero cell weight removes that cell from its own column's fit only ---
W <- matrix(1, n, 2); W[1:50, 1] <- 0
X2 <- X; X2[1:50, 1] <- 999                          # garbage, but down-weighted to zero
Bhat2 <- VIM:::.gloc_update_B(X2, U, W)
expect_true(max(abs(Bhat2[, 1] - Btrue[, 1])) < 0.05)
expect_true(max(abs(Bhat2[, 2] - Btrue[, 2])) < 0.05)

# --- .gloc_cond_resid is standardised: unit variance on clean Gaussian data ---
set.seed(3)
S <- matrix(c(1, .6, .3, .6, 1, .4, .3, .4, 1), 3, 3)
R <- MASS::mvrnorm(4000, rep(0, 3), S)
Z <- VIM:::.gloc_cond_resid(R, S)
expect_equal(dim(Z), c(4000L, 3L))
expect_true(all(abs(apply(Z, 2, sd) - 1) < 0.05))
expect_true(all(abs(colMeans(Z)) < 0.06))
