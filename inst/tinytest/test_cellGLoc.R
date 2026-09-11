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

# --- a column with no cell of positive weight must warn (naming the column)
# and fall back deterministically -- never a silent NA ---
set.seed(5)
n3 <- 100
Uc <- matrix(1, n3, 1, dimnames = list(NULL, "(Intercept)"))
Xc <- matrix(rnorm(n3 * 2), n3, 2, dimnames = list(NULL, c("v1", "v2")))
Wc <- matrix(1, n3, 2); Wc[, 1] <- 0                 # v1 has no usable weight at all
expect_warning(Bc <- VIM:::.gloc_update_B(Xc, Uc, Wc), "v1")
expect_false(anyNA(Bc))
expect_equal(Bc[1, "v1"], stats::median(Xc[, "v1"]))

# --- .gloc_cond_resid is standardised: unit variance on clean Gaussian data ---
set.seed(3)
S <- matrix(c(1, .6, .3, .6, 1, .4, .3, .4, 1), 3, 3)
R <- MASS::mvrnorm(4000, rep(0, 3), S)
Z <- VIM:::.gloc_cond_resid(R, S)
expect_equal(dim(Z), c(4000L, 3L))
expect_true(all(abs(apply(Z, 2, sd) - 1) < 0.05))
expect_true(all(abs(colMeans(Z)) < 0.06))

# has teeth: a marginal-only Z = R / sqrt(diag(Sigma)) would retain the raw
# correlation with each peer (0.3-0.6 under S above); proper conditioning on
# the peers must remove it (measured ~0.02-0.04; 0.10 separates the two cleanly)
maxpeercor <- 0
for (j in 1:3) for (k in setdiff(1:3, j))
  maxpeercor <- max(maxpeercor, abs(cor(Z[, j], R[, k])))
expect_true(maxpeercor < 0.10)

# --- .gloc_cond_resid: when a cell's peers are entirely absent, the standardised
# residual must fall back to the MARGINAL variance (Sigma[j,j]), not the
# full-peer-set conditional variance -- the latter understates the true spread
# and inflates Z (sd ~1.25 under S above, not 1) ---
set.seed(4)
Rmiss <- MASS::mvrnorm(4000, rep(0, 3), S)
Rmiss[, 2:3] <- NA_real_                             # column 1's peers fully absent
Zmiss <- VIM:::.gloc_cond_resid(Rmiss, S)
expect_true(abs(sd(Zmiss[, 1]) - 1) < 0.05)

# ==========================================================================
# imputeCellGLoc(): the two reduction claims
#
# The cellWise-dependent blocks are wrapped in requireNamespace() rather than
# exit_file(): exit_file() aborts the WHOLE file, which would silently skip
# every test below it, including the imputation tests that need no cellWise.
# ==========================================================================

if (requireNamespace("cellWise", quietly = TRUE)) {

  # --- REDUCTION 1: design = ~1 with binary weights reproduces cellMCD ---
  set.seed(11)
  Xr <- MASS::mvrnorm(400, rep(0, 4), 0.5 * diag(4) + 0.5)
  Xr[1:8, 1] <- Xr[1:8, 1] + 8
  colnames(Xr) <- paste0("x", 1:4)
  dr <- as.data.frame(Xr)

  ref <- cellWise::cellMCD(Xr, alpha = 0.75)
  got <- VIM::imputeCellGLoc(dr, design = ~ 1, weights = "binary", alpha = 0.75)

  expect_true(max(abs(got$Sigma - ref$S)) / max(abs(ref$S)) < 0.05)
  expect_true(max(abs(as.vector(got$B[1, ]) - ref$mu)) < 0.10)
  expect_true(mean(got$W == ref$W) > 0.97)          # same cells flagged

  # --- REDUCTION 2: design = ~1, all weights 1, soft corner gives the Gaussian MLE ---
  got1 <- VIM::imputeCellGLoc(dr, design = ~ 1, weights = "soft",
                              psi_c = Inf, maxit = 1)
  expect_true(max(abs(as.vector(got1$B[1, ]) - colMeans(Xr))) < 1e-8)
  mle <- crossprod(scale(Xr, TRUE, FALSE)) / nrow(Xr)
  expect_true(max(abs(got1$Sigma - mle)) / max(abs(mle)) < 1e-6)
}

# --- the mean structure is actually used: a strong group effect is absorbed ---
set.seed(12)
n <- 600
g  <- factor(sample(c("a", "b", "c"), n, TRUE))
Xg <- MASS::mvrnorm(n, rep(0, 3), diag(3))
Xg[g == "a", ] <- Xg[g == "a", ] + 4
Xg[g == "c", ] <- Xg[g == "c", ] - 4
dg <- data.frame(Xg, g = g); names(dg)[1:3] <- paste0("x", 1:3)

with_g <- VIM::imputeCellGLoc(dg, design = ~ ., weights = "soft")
no_g   <- VIM::imputeCellGLoc(dg, design = ~ 1, weights = "soft")
# ignoring the design inflates the scatter; modelling it recovers the identity
expect_true(mean(diag(with_g$Sigma)) < mean(diag(no_g$Sigma)))
expect_true(max(abs(diag(with_g$Sigma) - 1)) < 0.35)

# --- missing continuous cells are filled by the model's conditional expectation ---
set.seed(21)
n  <- 400
g  <- factor(sample(c("a", "b"), n, TRUE))
Xi <- MASS::mvrnorm(n, rep(0, 3), 0.4 * diag(3) + 0.6)
Xi[g == "a", ] <- Xi[g == "a", ] + 3
di <- data.frame(Xi, g = g); names(di)[1:3] <- paste0("x", 1:3)
truth <- di$x1
di$x1[1:40] <- NA

res <- VIM::imputeCellGLoc(di, design = ~ ., weights = "soft")
expect_false(anyNA(res$imputed$x1))
expect_equal(nrow(res$imputed), n)
expect_equal(names(res$imputed), names(di))
expect_true(is.factor(res$imputed$g))
# imputations must respect the group structure: better than the pooled mean
rmse_model  <- sqrt(mean((res$imputed$x1[1:40] - truth[1:40])^2))
rmse_pooled <- sqrt(mean((mean(di$x1, na.rm = TRUE) - truth[1:40])^2))
expect_true(rmse_model < rmse_pooled)
