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

  # The three assertions above are, on their own, VACUOUS. At design = ~ 1 the
  # fitted mean is constant within each column, so R is a pure translation of X,
  # and cellMCD is exactly translation-equivariant in S and W. They therefore
  # hold for ANY B -- including B pinned to zero, with no iteration and no
  # weighted least squares at all. The next assertions pin the parts that
  # translation-equivariance cannot supply.

  # (a) the weighted-least-squares step itself: with design = ~ 1 and binary
  #     weights, B[1, j] IS the mean of column j's unflagged cells, exactly.
  unflagged_mean <- vapply(seq_len(ncol(Xr)),
                           function(j) mean(Xr[got$W[, j] == 1, j]), numeric(1))
  expect_true(max(abs(as.vector(got$B[1, ]) - unflagged_mean)) < 1e-12)

  # (b) the alternation actually ran, and the weights actually moved off their
  #     all-ones initialisation (the comparison array borrows got$W's dimnames
  #     so that this tests values, not attributes)
  expect_true(got$iterations >= 2)
  expect_false(identical(got$W, array(1, dim(got$W), dimnames(got$W))))

  # --- REDUCTION 1b: a NON-INTERCEPT design, which translation-equivariance
  # cannot satisfy. With three well-separated groups, any estimator that does
  # not actually fit the mean structure reads the between-group spread as
  # scatter. ---
  set.seed(13)
  ng  <- 250
  gg  <- factor(rep(c("a", "b", "c"), each = ng))
  Xg2 <- MASS::mvrnorm(3 * ng, rep(0, 3), 0.5 * diag(3) + 0.5)
  shift <- rbind(c(0, 0, 0), c(5, -5, 2), c(-4, 3, -6))
  Xg2 <- Xg2 + shift[as.integer(gg), ]
  Xg2[1:15, 1] <- Xg2[1:15, 1] + 9                  # cellwise contamination
  colnames(Xg2) <- paste0("x", 1:3)
  dgg <- data.frame(Xg2, g = gg)

  fit_g <- VIM::imputeCellGLoc(dgg, design = ~ g, weights = "binary",
                               alpha = 0.75)
  fitted_mu <- fit_g$U %*% fit_g$B

  # the fitted group means reproduce a cellMCD fitted separately in each group
  per_group_mu <- t(vapply(levels(gg), function(k)
    cellWise::cellMCD(Xg2[gg == k, , drop = FALSE], alpha = 0.75,
                      checkPars = list(silent = TRUE))$mu, numeric(3)))
  own_mu <- t(vapply(levels(gg),
                     function(k) fitted_mu[which(gg == k)[1], ], numeric(3)))
  expect_true(max(abs(own_mu - per_group_mu)) < 0.10)

  # and the single pooled scatter reproduces a cellMCD on oracle-centred data
  ref_pooled <- cellWise::cellMCD(Xg2 - shift[as.integer(gg), ], alpha = 0.75,
                                  checkPars = list(silent = TRUE))
  expect_true(max(abs(fit_g$Sigma - ref_pooled$S)) /
                max(abs(ref_pooled$S)) < 0.05)
  # an estimator that ignores the design sees diagonals of ~14 here, not ~1
  expect_true(max(diag(fit_g$Sigma)) < 2)

  # --- REDUCTION 2: design = ~1, all weights 1, soft corner gives the Gaussian MLE ---
  # start = "classical": the claim is that ONE iteration from all weights at 1
  # is the Gaussian MLE. The robust start begins from cellMCD's flags instead,
  # so its first iteration is a different (and deliberately robust) scatter.
  got1 <- VIM::imputeCellGLoc(dr, design = ~ 1, weights = "soft",
                              psi_c = Inf, maxit = 1, start = "classical")
  expect_true(max(abs(as.vector(got1$B[1, ]) - colMeans(Xr))) < 1e-8)
  mle <- crossprod(scale(Xr, TRUE, FALSE)) / nrow(Xr)
  expect_true(max(abs(got1$Sigma - mle)) / max(abs(mle)) < 1e-6)

  # Reduction 2 sets psi_c = Inf, so every weight is 1 and a scatter step that
  # ignored W entirely would also pass it to machine precision. The next block
  # runs the soft corner at the DEFAULT psi_c against real contamination, where
  # ignoring W is fatal.
  set.seed(14)
  ns <- 600; ps <- 4
  Sc <- 0.5 * diag(ps) + 0.5
  Xs <- MASS::mvrnorm(ns, rep(0, ps), Sc)
  colnames(Xs) <- paste0("x", 1:ps)
  Xclean <- Xs
  inj <- matrix(FALSE, ns, ps)
  inj[sample.int(ns * ps, round(0.03 * ns * ps))] <- TRUE
  Xs[inj] <- Xs[inj] + 8

  rs <- VIM::imputeCellGLoc(as.data.frame(Xs),     design = ~ 1, weights = "soft")
  rc <- VIM::imputeCellGLoc(as.data.frame(Xclean), design = ~ 1, weights = "soft")

  # Scatter recovery, stated as robustness: injecting 3% of cells at +8 must
  # barely move the estimate away from the fit on the same data uncontaminated.
  # This is the assertion that an estimator ignoring W cannot pass -- the
  # second line pins that, by measuring how far the non-robust Gaussian ML fit
  # on the same contaminated data is displaced (measured: 184%, vs 2.7% here).
  expect_true(max(abs(rs$Sigma - rc$Sigma)) / max(abs(rc$Sigma)) < 0.05)
  mle_contam <- crossprod(scale(Xs, TRUE, FALSE)) / nrow(Xs)
  expect_true(max(abs(mle_contam - rc$Sigma)) / max(abs(rc$Sigma)) > 1)

  # and it is the injected cells that get downweighted
  flagged <- rs$W < 0.5
  contam_row <- (rowSums(inj) > 0)[row(inj)]
  expect_true(mean(flagged[inj]) > 0.90)                     # recall
  expect_true(mean(flagged[!inj & !contam_row]) < 0.05)      # FPR in clean rows
  # Clean cells sharing a row with a contaminated one must not be dragged down
  # with it. .gloc_cond_resid() used to condition on every finite peer, which
  # put this at 0.663 against 0.0155 in clean rows; excluding downweighted
  # peers from the conditioning set brings it to 0.0408. The dedicated
  # single-column test below is the clean demonstration.
  expect_true(mean(flagged[!inj & contam_row]) < 0.08)

  # --- the soft corner must be Fisher-consistent at the Gaussian model. The
  # bisquare deflates a sum(w)-normalised weighted scatter, by a factor that is
  # kappa = E[w(Z)Z^2] / E[w(Z)] = 0.828 only when the columns are INDEPENDENT;
  # the weights act on conditional residuals, so with correlation the factor is
  # 1 - (s_j^2 / sigma_j^2)(1 - kappa). Uncorrected the fixed point is 0.786, a
  # ~21% under-estimate that inflates the standardised residuals and makes the
  # estimator over-flag. ---
  set.seed(15)
  Xf <- MASS::mvrnorm(4000, rep(0, 3), diag(3))
  colnames(Xf) <- paste0("x", 1:3)
  rf <- VIM::imputeCellGLoc(as.data.frame(Xf), design = ~ 1, weights = "soft")
  expect_true(max(abs(diag(rf$Sigma) - 1)) < 0.06)

  # the same claim at correlated Sigma, where a single scalar kappa over-corrects
  # by +8% at rho = 0.5 and +15% at rho = 0.8. Comparing against the psi_c = Inf
  # fit on the SAME data cancels sampling and isolates the downweighting bias.
  #
  # at_home() only. This is four fits at n = 4000 and, measured, 10.2 s of the
  # file's 18.9 s -- the single most expensive thing in it, and the file is
  # part of a suite that CRAN gives 10 minutes in total. It is gated rather
  # than shrunk because the 0.06 tolerance is sized for n = 4000; a smaller n
  # would keep the assertion's text and lose its meaning. Nothing load-bearing
  # is gated: both reduction blocks, the contamination-recovery block and the
  # swamping block all still run on CRAN. devtools::check() and the CI
  # workflow set NOT_CRAN=true and so run this too.
  if (at_home()) for (rho in c(0.5, 0.8)) {
    set.seed(19)
    Xk <- MASS::mvrnorm(4000, rep(0, 3), (1 - rho) * diag(3) + rho)
    colnames(Xk) <- paste0("x", 1:3)
    rk <- VIM::imputeCellGLoc(as.data.frame(Xk), design = ~ 1, weights = "soft")
    # the reference is the one-iteration Gaussian MLE, which only the classical
    # start (all weights at 1) produces; see REDUCTION 2
    mk <- VIM::imputeCellGLoc(as.data.frame(Xk), design = ~ 1, weights = "soft",
                              psi_c = Inf, maxit = 1, start = "classical")
    expect_true(max(abs(diag(rk$Sigma) / diag(mk$Sigma) - 1)) < 0.06)
    # Assert CONVERGENCE, not just the value. High correlation is exactly where
    # the peer-inclusion decision cycles -- every damping failure in the sweep
    # behind .gloc_damp_schedule is at rho >= 0.5, and none at rho = 0 -- so a
    # block that checks only the diagonal ratio is blind to the failure mode
    # that this configuration is most likely to hit.
    expect_true(rk$converged)
  }

  # kappa itself: 1 at no downweighting, and the published value at the default
  expect_equal(VIM:::.gloc_consistency(Inf), 1)
  expect_true(abs(VIM:::.gloc_consistency(4.685) - 0.828073) < 1e-5)
  # and the matrix-aware correction reduces to the scalar one under independence
  expect_true(max(abs(VIM:::.gloc_correct_scatter(diag(3) * 0.828073, 0.828073) -
                        diag(3))) < 1e-8)
  expect_equal(VIM:::.gloc_correct_scatter(diag(3), 1), diag(3))
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
# tightened from 0.35: that tolerance had been sized around the ~21% scatter
# deflation of the uncorrected bisquare, which the consistency factor removes
expect_true(max(abs(diag(with_g$Sigma) - 1)) < 0.10)

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

# ==========================================================================
# Convergence, degraded paths and edge cases
# ==========================================================================

# --- convergence must not be declared while the weights are still moving.
# The criterion divides the change in the FITTED MEANS by the scatter. A
# criterion that divided a coefficient change by a LOCATION (max|B_old|) would
# loosen 200,000-fold when the data is shifted by +1000, stopping after one
# iteration with hundreds of weights still moving and returning a scatter
# several times too large. Shifting must leave the fit equivariant. ---
set.seed(16)
Xe <- MASS::mvrnorm(300, rep(0, 3), 0.5 * diag(3) + 0.5)
Xe[1:10, 2] <- Xe[1:10, 2] + 9
colnames(Xe) <- paste0("x", 1:3)

e0 <- VIM::imputeCellGLoc(as.data.frame(Xe),        design = ~ 1, weights = "soft")
e1 <- VIM::imputeCellGLoc(as.data.frame(Xe + 1000), design = ~ 1, weights = "soft")

expect_true(max(abs(e1$Sigma - e0$Sigma)) / max(abs(e0$Sigma)) < 1e-6)
expect_true(max(abs(e1$W - e0$W)) < 1e-6)
expect_true(max(abs((as.vector(e1$B[1, ]) - as.vector(e0$B[1, ])) - 1000)) < 1e-6)
expect_equal(e1$iterations, e0$iterations)
expect_true(e0$converged)

# --- the degraded scatter path honours the cell weights and is never silent.
# cellWise is in Suggests, so running without it is supported, not an edge
# case; have_cw = FALSE exercises exactly that path. ---
set.seed(17)
Rw <- matrix(rnorm(300), 100, 3, dimnames = list(NULL, paste0("x", 1:3)))
Ww <- matrix(1, 100, 3)
Mw <- matrix(FALSE, 100, 3)
Rw[1:5, 1] <- 50; Ww[1:5, 1] <- 0        # garbage cells, driven to weight zero

expect_warning(Sw <- VIM:::.gloc_scatter_soft(Rw, Ww, Mw, have_cw = FALSE),
               "cellWise")
# a covariance that discarded W would put ~125 here, not ~1
expect_true(Sw[1, 1] < 2)
expect_true(min(eigen(Sw, symmetric = TRUE)$values) > 0)   # repaired to p.d.
# and it really is weighted: zeroing more cells changes the answer
Ww2 <- Ww; Ww2[6:40, 2] <- 0
Sw2 <- suppressWarnings(VIM:::.gloc_scatter_soft(Rw, Ww2, Mw, have_cw = FALSE))
expect_false(isTRUE(all.equal(Sw, Sw2)))

# --- maxit = 0 returns the starting fit instead of erroring ---
z0 <- VIM::imputeCellGLoc(dg, design = ~ ., weights = "soft", maxit = 0)
expect_equal(z0$iterations, 0L)
expect_false(z0$converged)
expect_true(all(is.finite(z0$Sigma)))
expect_false(anyNA(z0$imputed))
expect_equal(names(z0$criterion),
             c("means", "scatter", "weights", "scatter_spread"))
expect_true(all(is.na(z0$criterion)))          # nothing was computed at all

# --- $criterion reports the stopping residuals, so a caller can test them
# rather than parse a warning string. A NON-converged fit returns whatever the
# last iteration produced, and from Sigma alone there is no way to see whether
# that is a point on a settled cycle -- where the answer depends on which phase
# maxit stopped in -- or a value still drifting. scatter_spread measures that
# dependence; it is deliberately NA on a converged fit, where the trailing
# window still holds the approach and a number there would invite the wrong
# reading. ---
zk <- VIM::imputeCellGLoc(dg, design = ~ 1, weights = "soft")
expect_true(zk$converged)
expect_equal(names(zk$criterion),
             c("means", "scatter", "weights", "scatter_spread"))
expect_true(all(zk$criterion[c("means", "scatter", "weights")] < 5e-3))
expect_true(is.na(zk$criterion[["scatter_spread"]]))

expect_warning(zm <- VIM::imputeCellGLoc(dg, design = ~ 1, weights = "soft",
                                         maxit = 2))
expect_false(zm$converged)
expect_true(is.finite(zm$criterion[["scatter_spread"]]))
expect_true(zm$criterion[["scatter_spread"]] > 0)
# it really is a spread over the window, not a copy of the one-step residual:
# at two iterations the window holds two scatters, so the two coincide
expect_equal(zm$criterion[["scatter_spread"]], zm$criterion[["scatter"]])

# --- integer columns keep their class through imputation ---
set.seed(18)
di2 <- data.frame(a = as.integer(round(rnorm(200, 50, 10))),
                  b = rnorm(200),
                  g = factor(rep(c("u", "v"), 100)))
di2$a[1:20] <- NA_integer_
zi <- VIM::imputeCellGLoc(di2, design = ~ ., weights = "soft")
expect_true(is.integer(zi$imputed$a))
expect_false(anyNA(zi$imputed$a))
expect_true(is.double(zi$imputed$b))

# --- a non-finite value that is not NA is imputed, but never silently ---
di3 <- dg; di3$x1[3] <- Inf
expect_warning(zf <- VIM::imputeCellGLoc(di3, design = ~ ., weights = "soft"),
               "non-finite")
expect_false(anyNA(zf$imputed$x1))
expect_true(all(is.finite(zf$imputed$x1)))

# ==========================================================================
# Outlier propagation within a row (the "swamping" test)
#
# A cell is conditioned only on peers that are themselves still clean, so a
# contaminated cell is treated exactly like an absent one. Conditioning on
# every finite peer instead lets one bad cell inflate the conditional residual
# of every other cell in its row, and they are all flagged -- the precise
# failure the cellwise literature exists to prevent, and a divergence from
# cellMCD, which predicts a flagged cell from the clean cells in its row.
# ==========================================================================

set.seed(24)
np <- 1000; ncont <- 50
Xp <- MASS::mvrnorm(np, rep(0, 3), 0.5 * diag(3) + 0.5)
colnames(Xp) <- paste0("x", 1:3)
Xp[seq_len(ncont), 1] <- Xp[seq_len(ncont), 1] + 8    # ONLY x1, ONLY these rows
dp <- as.data.frame(Xp)
ci <- seq_len(ncont); cl <- (ncont + 1):np

fixed <- VIM::imputeCellGLoc(dp, design = ~ 1, weights = "soft")
ff <- fixed$W < 0.5
clean_rate <- mean(ff[cl, 2])                          # clean rows, clean column

# the contaminated cells are still all caught
expect_true(mean(ff[ci, 1]) > 0.95)
# and their clean row-mates are flagged at the clean-row rate, not above it
# (measured 0.0200 and 0.0200 against a clean-row rate of 0.0189)
expect_true(mean(ff[ci, 2]) < 2 * clean_rate)
expect_true(mean(ff[ci, 3]) < 2 * clean_rate)
expect_true(mean(ff[ci, 2]) < 0.05)
expect_true(mean(ff[ci, 3]) < 0.05)
expect_true(fixed$converged)

# Teeth: a negative peer_w_min conditions on every finite peer, which is the
# old behaviour, and the propagation comes straight back (0.82 and 0.66).
# It has to be negative, not 0. The damped weight update multiplies a weight
# by (1 - d) each time the bisquare sends it to zero, so a contaminated weight
# decays geometrically without ever reaching zero -- measured ~4e-6 at
# convergence, with no cell exactly 0. A threshold of 0 therefore readmits
# every one of those cells at full influence and reproduces the propagation
# rather than preventing it.
prop <- VIM::imputeCellGLoc(dp, design = ~ 1, weights = "soft", peer_w_min = -1)
fp <- prop$W < 0.5
expect_true(mean(fp[ci, 2]) > 0.5)
expect_true(mean(fp[ci, 3]) > 0.5)
expect_true(mean(fp[cl, 2]) < 0.05)      # clean rows unaffected either way

# --- .gloc_cond_resid honours the weights, and ignores them when W is NULL ---
set.seed(25)
Sw3 <- 0.5 * diag(3) + 0.5
Rw3 <- MASS::mvrnorm(500, rep(0, 3), Sw3)
colnames(Rw3) <- paste0("x", 1:3)
Rw3[1:50, 1] <- Rw3[1:50, 1] + 10                     # a grossly bad peer
Ww3 <- matrix(1, 500, 3); Ww3[1:50, 1] <- 0           # correctly downweighted

Z_all  <- VIM:::.gloc_cond_resid(Rw3, Sw3)                 # conditions on it
Z_wgt  <- VIM:::.gloc_cond_resid(Rw3, Sw3, W = Ww3)        # treats it as absent
# the bad peer drags its row-mates' standardised residuals far out ...
expect_true(mean(abs(Z_all[1:50, 2])) > 3 * mean(abs(Z_all[51:500, 2])))
# ... and excluding it puts them back on the same scale as everyone else
expect_true(mean(abs(Z_wgt[1:50, 2])) < 1.5 * mean(abs(Z_wgt[51:500, 2])))
# W = NULL must reproduce the old behaviour exactly
expect_equal(Z_all, VIM:::.gloc_cond_resid(Rw3, Sw3, W = NULL))
# and an all-clean W must agree with no W at all
expect_equal(Z_all, VIM:::.gloc_cond_resid(Rw3, Sw3, W = matrix(1, 500, 3)))

# ==========================================================================
# The two performance levers
#
# The scatter step is about 95% of an iteration (Rprof, n = 1000, p = 10, five
# draws spanning 94.3-95.6%; it was quoted as 97.5% here and 98.8% in the
# roxygen until 2026-09-12, when it was re-measured), so both
# levers aim there: cw_crit loosens the EM inside it, and the adaptive
# relaxation schedule needs fewer of them. Neither may move where the
# iteration lands, which is what these assertions are for.
# ==========================================================================

if (requireNamespace("cellWise", quietly = TRUE)) {

  set.seed(41)
  Xv <- MASS::mvrnorm(400, rep(0, 4), 0.5 * diag(4) + 0.5)
  Xv[1:12, 2] <- Xv[1:12, 2] + 8
  colnames(Xv) <- paste0("x", 1:4)
  dv <- as.data.frame(Xv)

  # damp = 0.25 is the relaxation factor of releases before 7.4.0. It is not
  # bit-identical to them: they compared the undivided weight step against
  # eps, so a relaxed run stopped at four times the fixed-point residual eps
  # asked for. That is now divided out, which is what makes the two schedules
  # comparable at all.
  ad  <- VIM::imputeCellGLoc(dv, design = ~ 1, weights = "soft")
  ref <- VIM::imputeCellGLoc(dv, design = ~ 1, weights = "soft",
                             damp = 0.25, cw_crit = 1e-12)
  expect_true(ad$converged)
  expect_true(ref$converged)
  # The fixed point does not move. The tolerance is not machine precision
  # because both schedules stop at a residual of eps rather than at 0, and
  # they stop at different points inside that slack. Across the
  # 36-configuration sweep the median relative difference in Sigma is 8.6e-5
  # and the worst 4.2e-3, and 29 of 34 configurations produce an IDENTICAL
  # flagged set (8 cells differ in total, worst case 3 of 3200). Tightening
  # eps shrinks all of it, which is how we know it is tolerance slack: at
  # eps = 1e-5 the flagged sets agree in 44 of 54 detection runs against 33
  # at the default, and the residual recall difference loses its sign
  # (paired p = 0.24 against 0.022).
  expect_true(max(abs(ad$Sigma - ref$Sigma)) / max(abs(ref$Sigma)) < 0.02)
  expect_true(max(abs(ad$B - ref$B)) < 0.01)
  # ... and it is reached in strictly fewer iterations
  expect_true(ad$iterations < ref$iterations)

  # Lever 1 alone: five orders of magnitude on cwLocScat's inner EM must not
  # be visible in the scatter it returns. 1e-12 is cwLocScat's own default and
  # bought a 1.5e-9 difference for 1.46x the time.
  Rv <- scale(Xv, TRUE, FALSE)
  Wv <- matrix(1, nrow(Xv), ncol(Xv)); Mv <- matrix(FALSE, nrow(Xv), ncol(Xv))
  s8  <- VIM:::.gloc_scatter_soft(Rv, Wv, Mv, crit = 1e-8)
  s12 <- VIM:::.gloc_scatter_soft(Rv, Wv, Mv, crit = 1e-12)
  expect_true(max(abs(s8 - s12)) < 1e-6)

  # Lever 2 alone: uncorrelated data never cycles -- every convergence failure
  # in the sweep behind .gloc_damp_schedule is at rho >= 0.5, and none is at
  # rho = 0, not even undamped -- so the schedule must not pay for relaxation
  # it does not need. A fixed 0.25 costs several times the iterations here.
  set.seed(42)
  Xu <- MASS::mvrnorm(400, rep(0, 4), diag(4))
  colnames(Xu) <- paste0("x", 1:4)
  du <- as.data.frame(Xu)
  au <- VIM::imputeCellGLoc(du, design = ~ 1, weights = "soft")
  fu <- VIM::imputeCellGLoc(du, design = ~ 1, weights = "soft", damp = 0.25)
  expect_true(au$converged)
  expect_true(2 * au$iterations < fu$iterations)
}

# --- damp, cw_crit and peer_band are validated rather than silently accepted ---
expect_error(VIM::imputeCellGLoc(dg, design = ~ 1, damp = 0))
expect_error(VIM::imputeCellGLoc(dg, design = ~ 1, damp = 1.5))
expect_error(VIM::imputeCellGLoc(dg, design = ~ 1, cw_crit = 0))
expect_error(VIM::imputeCellGLoc(dg, design = ~ 1, peer_band = -0.1))
expect_error(VIM::imputeCellGLoc(dg, design = ~ 1, peer_band = c(0.05, 0.05)))

# --- the non-convergence warning tells the two failure modes apart: an
# exhausted limit is cured by raising maxit, a limit cycle is not cured by
# raising anything. ---
expect_warning(VIM::imputeCellGLoc(dg, design = ~ 1, weights = "soft", maxit = 2),
               "raise maxit")

# ==========================================================================
# The peer band
#
# "Condition on a peer iff its weight exceeds w_min" makes the weight map
# DISCONTINUOUS, and a discontinuous self-map of [0,1]^(n x p) need not have a
# fixed point at all. An iteration told to drive max|f(W) - W| below a
# tolerance can then be asked for a state that does not exist, and on the
# design = ~ . arm it usually was: 15 of 40 pilot fits converged. The
# threshold is therefore a band. Peers that are confidently clean (r = 1) or
# confidently flagged (r = 0) are treated exactly as before; only the fringe
# between is interpolated, as a peer observed with measurement error.
# ==========================================================================

set.seed(51)
Sb <- 0.5 * diag(4) + 0.5
colnames(Sb) <- rownames(Sb) <- paste0("x", 1:4)
# Z %*% chol(Sb), not MASS::mvrnorm. Sb has a triple eigenvalue, so the
# eigenbasis mvrnorm uses is not unique and depends on the BLAS: the same seed
# gave different data on different platforms, and the discontinuity check
# below failed on 3 of 6 GitHub check platforms until 2026-09-15. The Cholesky
# factor is unique. Both draw 300 * 4 normals, so the runif() stream after
# this line is unchanged.
Rb <- matrix(rnorm(300 * 4), 300) %*% chol(Sb)
colnames(Rb) <- paste0("x", 1:4)
Rb[1:10, 2] <- NA_real_                       # missing peers in the mix too

# (a) THE ENDPOINTS ARE UNCHANGED. The band may not move the estimator for any
# cell that is not in it, or "the fixed point does not move" is empty talk.
Wsharp <- matrix(ifelse(runif(300 * 4) < 0.1, 0.05, 0.95), 300, 4)
Wsharp[is.na(Rb)] <- 0
expect_equal(VIM:::.gloc_cond_resid(Rb, Sb, W = Wsharp),
             VIM:::.gloc_cond_resid(Rb, Sb, W = Wsharp, band = 0))
# ... and the three callers that never had a threshold are untouched
expect_equal(VIM:::.gloc_cond_resid(Rb, Sb),
             VIM:::.gloc_cond_resid(Rb, Sb, W = NULL, band = 0))
expect_equal(VIM:::.gloc_cond_resid(Rb, Sb, W = matrix(1, 300, 4)),
             VIM:::.gloc_cond_resid(Rb, Sb))
Wmix <- matrix(runif(300 * 4), 300, 4); Wmix[is.na(Rb)] <- 0
expect_equal(VIM:::.gloc_cond_resid(Rb, Sb, W = Wmix, w_min = -1),
             VIM:::.gloc_cond_resid(Rb, Sb, W = Wmix, w_min = -1, band = 0))

# (b) CONTINUITY. Sweep one cell's weight across the threshold and watch a
# row-mate's standardised residual. The hard cut moves it in a single step;
# the band spreads the same total excursion over the sweep. Measured on these
# data: largest one-step change 0.225 hard against 0.0071 banded, a factor of
# 32. (The 0.110 and 0.0035 recorded here before 2026-09-15 matched neither
# the old data nor these.)
Wc <- matrix(0.95, 300, 4); Wc[is.na(Rb)] <- 0
sweep_z <- function(band) vapply(seq(0.30, 0.70, by = 0.002), function(w) {
  Wx <- Wc; Wx[20, 3] <- w
  VIM:::.gloc_cond_resid(Rb, Sb, W = Wx, w_min = 0.5, band = band)[20, 1]
}, numeric(1))
z_hard <- sweep_z(0)
z_band <- sweep_z(VIM:::.gloc_peer_band)
expect_true(max(abs(diff(z_hard))) > 0.05)            # the discontinuity is real
expect_true(max(abs(diff(z_band))) < 0.01)            # and the band removes it
expect_true(max(abs(diff(z_band))) < max(abs(diff(z_hard))) / 10)
# the two agree wherever the swept cell is outside the band, so the band
# interpolates between the hard rule's two answers rather than replacing them.
# Two grid points lie on the band edges to within 4e-17, where rounding alone
# decides the side; the 1e-8 margin keeps them out of the comparison.
outside <- abs(seq(0.30, 0.70, by = 0.002) - 0.5) > VIM:::.gloc_peer_band + 1e-8
expect_equal(z_hard[outside], z_band[outside])

if (at_home()) {
  # A configuration where the hard cut (peer_band = 0) cycles and warns
  # "cycling" while the band converges to what that cycle orbits, which is the
  # cause-and-effect this block exists to pin. n = 800, p = 4, and the
  # hard-cut arm runs to maxit, so at_home() only.
  #
  # The hard cut does not cycle on every draw. At this configuration it cycled
  # for 3 of the first 12 seeds, and seed 4 is used because its cycling
  # survived three relative jitters of 1e-9 on the data. Until 2026-09-15 the
  # block used set.seed(5) with MASS::mvrnorm. That draw cycled only on the
  # author's machine: the matrix has a triple eigenvalue, mvrnorm's eigenbasis
  # depends on the BLAS, and on all 6 GitHub check platforms the hard cut
  # converged. The Cholesky factor below is unique.
  set.seed(4)
  Xz <- matrix(rnorm(800 * 4), 800) %*% chol(0.2 * diag(4) + 0.8)
  inj <- matrix(FALSE, 800, 4)
  inj[sample.int(800 * 4, 160)] <- TRUE
  Xz[inj] <- Xz[inj] + 8
  colnames(Xz) <- paste0("x", 1:4)

  # Both fits use start = "classical": this block pins a property of the peer
  # rule, and the start must not be allowed to change which state the hard cut
  # reaches. On this machine the robust start (7.4.1) happened to converge on
  # these data, but that is one draw on one BLAS and is not asserted.
  # (Corrected: this comment first said the robust start "avoids this cycle on
  # its own", a claim no test checks.)
  expect_warning(zh <- VIM::imputeCellGLoc(as.data.frame(Xz), design = ~ 1,
                                           weights = "soft", maxit = 200,
                                           peer_band = 0, start = "classical"),
                 "cycling")
  expect_false(zh$converged)

  # the hard-cut arm is exactly the case $criterion exists for: it returns a
  # scatter that is still moving, and says so in a number
  expect_true(is.finite(zh$criterion[["scatter_spread"]]))
  expect_true(zh$criterion[["scatter_spread"]] > 0)

  zc <- VIM::imputeCellGLoc(as.data.frame(Xz), design = ~ 1, weights = "soft",
                            maxit = 200, start = "classical")
  expect_true(zc$converged)
  expect_true(zc$iterations < 50)
  expect_true(is.na(zc$criterion[["scatter_spread"]]))
  # and it converges to what the cycling run was orbiting, not somewhere else:
  # the fix must remove the oscillation, not relocate the estimate
  expect_true(max(abs(zc$Sigma - zh$Sigma)) / max(abs(zh$Sigma)) < 0.05)
  expect_true(max(abs(zc$B - zh$B)) < 0.05)
  expect_true(mean((zc$W < 0.5) == (zh$W < 0.5)) > 0.98)
}

# ==========================================================================
# The deprecated alias
# ==========================================================================

# --- the old name still works and warns once ---
set.seed(31)
do <- data.frame(x1 = rnorm(120), x2 = rnorm(120),
                 f = factor(sample(c("a", "b"), 120, TRUE)))
do$x1[1:10] <- NA
expect_warning(old <- VIM::imputeCellMCD(do), pattern = "deprecated")
expect_false(anyNA(old$x1) && anyNA(old$x2))

# ==========================================================================
# The robust start (7.4.1)
#
# Until 7.4.0 the soft corner started from a classical fit: every observed cell
# at weight 1, B by ordinary least squares, the scatter from cwLocScat at unit
# weights. A redescending weight function started there can settle on a masked
# solution. The robust start fits B by MM regression of each continuous column
# on the categorical design alone, where no predictor cell can be contaminated,
# and takes the starting flags from cellMCD on those residuals.
# ==========================================================================

set.seed(61)
n  <- 450
g  <- factor(rep(c("a", "b", "c"), each = 150))
Us <- VIM:::.gloc_design(data.frame(g = g), ~ ., "g")
Btrue <- rbind(c(1, -1), c(3, -2), c(-2, 4))       # 3 design cols x 2 responses
Xs0 <- Us %*% Btrue + matrix(rnorm(n * 2), n, 2)
colnames(Xs0) <- c("x1", "x2")
bad <- matrix(FALSE, n, 2)
bad[sample.int(n * 2, round(0.2 * n * 2))] <- TRUE
Xsb <- Xs0; Xsb[bad] <- Xsb[bad] + 10              # 20% of response cells at +10
Ms  <- matrix(FALSE, n, 2)

if (requireNamespace("cellWise", quietly = TRUE)) {
  st <- VIM:::.gloc_start_robust(Xsb, Us, Ms)
  # Tolerances fixed before the first run. Treatment coding puts the whole +10
  # shift of 20% of cells into the intercept row, so least squares is off by
  # about 2 there, while the MM fit should stay within sampling error.
  expect_true(max(abs(st$B - Btrue)) < 0.4)
  expect_true(max(abs(VIM:::.gloc_update_B(Xsb, Us, matrix(1, n, 2)) - Btrue)) > 1.5)
  # the starting flags are cellMCD's: the injected cells, and few others
  expect_true(mean(st$W[bad] == 0) > 0.90)
  expect_true(mean(st$W[!bad] == 0) < 0.05)

  # missing cells start at weight 0 and do not break the fit
  Mm <- matrix(runif(n * 2) < 0.1, n, 2)
  Xsm <- Xsb; Xsm[Mm] <- NA
  stm <- VIM:::.gloc_start_robust(Xsm, Us, Mm)
  expect_true(all(stm$W[Mm] == 0))
  expect_false(anyNA(stm$B))

  # deterministic, and it does not consume the caller's random numbers: lmrob's
  # S-step subsamples, and a start that drew from the global stream would
  # desynchronise paired simulation arms (a lesson from Paper A4)
  set.seed(7); before <- .Random.seed
  st2 <- VIM:::.gloc_start_robust(Xsb, Us, Ms)
  expect_identical(.Random.seed, before)
  expect_identical(st2, st)

  # --- fallbacks: each degraded path warns, none is silent ---
  # too few observed rows for a design column -> median intercept, zero contrasts
  g_rare <- factor(c(rep("a", 100), rep("b", 98), rep("c", 2)))
  U_rare <- VIM:::.gloc_design(data.frame(g = g_rare), ~ ., "g")
  X_rare <- cbind(x1 = rnorm(200), x2 = rnorm(200))
  expect_warning(st_rare <- VIM:::.gloc_start_robust(X_rare, U_rare,
                                                     matrix(FALSE, 200, 2)),
                 "too few")
  expect_true(all(st_rare$B[-1, ] == 0))
  expect_equal(as.vector(st_rare$B[1, ]), unname(apply(X_rare, 2, stats::median)))

  # lmrob error
  expect_warning(VIM:::.gloc_start_robust(Xsb, Us, Ms, control = "not a control"),
                 "failed")
  # lmrob non-convergence
  expect_warning(VIM:::.gloc_start_robust(
    Xsb, Us, Ms, control = robustbase::lmrob.control(max.it = 1, k.max = 1,
                                                     maxit.scale = 1)),
    "converge")
}

# cellWise unavailable -> hard threshold on |residual| / MAD
expect_warning(st_nocw <- VIM:::.gloc_start_robust(Xsb, Us, Ms, have_cw = FALSE),
               "cellWise")
expect_true(mean(st_nocw$W[bad] == 0) > 0.90)
expect_true(mean(st_nocw$W[!bad] == 0) < 0.05)

if (requireNamespace("cellWise", quietly = TRUE)) {
  dsb <- data.frame(Xsb, g = g)
  # the default is the robust start, and it converges on contaminated data
  expect_equal(eval(formals(VIM::imputeCellGLoc)$start)[1], "robust")
  fr <- VIM::imputeCellGLoc(dsb, design = ~ ., weights = "soft")
  expect_true(fr$converged)
  expect_error(VIM::imputeCellGLoc(dsb, design = ~ ., start = "median"))
  # start has no effect on the binary corner, which already begins with cellMCD
  keep_b <- c("B", "Sigma", "W", "converged", "iterations")
  expect_identical(
    VIM::imputeCellGLoc(dsb, design = ~ ., weights = "binary", start = "robust")[keep_b],
    VIM::imputeCellGLoc(dsb, design = ~ ., weights = "binary", start = "classical")[keep_b])
}

# --- start = "classical" reproduces VIM 7.4.0's ESTIMATES. The reference was
# written by 7.4.0 (commit e8f204c) on fixed data, with Apple's Accelerate BLAS.
# Since 7.4.1 a missing cell is imputed from the unflagged peers in its row
# only, by the detection peer rule. B, Sigma and W do not depend on the
# imputation, so they must still match. $imputed must NOT match in the rows
# where a peer is flagged. The 7.4.0 imputations are asserted to be the
# all-peers rule applied to the same estimates, so the whole difference is
# attributable to the peer rule.
#
# Correction, recorded rather than deleted: until 2026-09-15 this block
# compared B, Sigma, W, the criterion and the all-peers imputations with
# expect_identical() under at_home(), and said bit-identity was "a statement
# about this code on one platform". But tests/tinytest.R sets at_home from
# NOT_CRAN, which the GitHub workflow sets on all 6 platforms, so the
# comparison ran everywhere. Under R's reference BLAS it failed 6 times, with
# differences up to 4.4e-15 and equal iteration counts. The bitwise
# comparisons now run only with VIM_BITREF=true. Everywhere else the
# iteration count and convergence flag are exact and B, Sigma, W and the
# all-peers imputations agree to 1e-10, a tolerance fixed before the first run
# and far above the measured cross-BLAS gap. ---
if (at_home() && requireNamespace("cellWise", quietly = TRUE)) {
  ref740 <- readRDS("gloc_classical_ref_740.rds")
  bitref <- identical(Sys.getenv("VIM_BITREF"), "true")
  keep_num <- c("B", "Sigma", "W")
  cases <- list(soft_dot = list(design = ~ ., weights = "soft"),
                soft_one = list(design = ~ 1, weights = "soft"),
                bin_dot  = list(design = ~ ., weights = "binary"))
  for (nm in names(cases)) {
    ref <- ref740$fits[[nm]]
    fit <- suppressWarnings(VIM::imputeCellGLoc(ref740$data,
                                                design = cases[[nm]]$design,
                                                weights = cases[[nm]]$weights,
                                                start = "classical"))
    expect_identical(fit[c("U", "converged", "iterations")],
                     ref[c("U", "converged", "iterations")], info = nm)
    for (k in keep_num)
      expect_equal(fit[[k]], ref[[k]], tolerance = 1e-10, info = paste(nm, k))
    if (bitref)
      expect_identical(fit[c(keep_num, "criterion")], ref[c(keep_num, "criterion")],
                       info = nm)
    cont <- colnames(fit$W)
    Xr <- as.matrix(ref740$data[, cont]); Mr <- is.na(Xr)
    old_imp <- as.matrix(ref$imputed[, cont])
    new_imp <- as.matrix(fit$imputed[, cont])
    all_peers <- unname(VIM:::.gloc_impute(Xr, fit$U, fit$B, fit$Sigma, Mr)[Mr])
    expect_equal(all_peers, unname(old_imp[Mr]), tolerance = 1e-10, info = nm)
    if (bitref) expect_identical(all_peers, unname(old_imp[Mr]), info = nm)
    rel <- VIM:::.gloc_peer_rel(!Mr, fit$W)
    hit <- rowSums(Mr) > 0 & rowSums(!Mr & rel < 1) > 0
    cells_hit  <- Mr & hit
    cells_same <- Mr & !hit
    expect_true(sum(cells_hit) > 0, info = nm)
    expect_true(max(abs(new_imp[cells_hit] - old_imp[cells_hit])) > 1e-6, info = nm)
    expect_equal(new_imp[cells_same], old_imp[cells_same], tolerance = 1e-12, info = nm)
    expect_identical(new_imp[!Mr], old_imp[!Mr], info = nm)
  }
}

# ==========================================================================
# The robust start's own cellMCD tolerance, and warnings that must not leak
# ==========================================================================

# cellMCD refuses any column whose marginal outliers plus NAs exceed 1 - alpha.
# At the user-facing default alpha = 0.75 that is 25%, which 20% missingness
# plus a few percent of shifted cells already exceeds, so in the 7.4.1 pilot the
# robust start fell back to MAD flags in 74 of its 180 fits, all at 10-20%
# contamination, 67 of them with shifts of 6 or 10 and 7 with a shift of 3.
# (Corrected: this comment first said 74 of 360 fits.) The start therefore runs
# cellMCD at its own alpha, and the binary corner keeps the user's.
set.seed(62)
n_a <- 400
Xa <- matrix(rnorm(n_a * 4), n_a) %*% chol(0.5 * diag(4) + 0.5)
colnames(Xa) <- paste0("x", 1:4)
out_a <- matrix(runif(n_a * 4) < 0.10, n_a)
Xa[out_a] <- Xa[out_a] + 10
Xa[matrix(runif(n_a * 4) < 0.20, n_a)] <- NA
da <- as.data.frame(Xa)

collect_warnings <- function(expr) {
  w <- character(0)
  withCallingHandlers(expr, warning = function(cond) {
    w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning")
  })
  w
}

if (requireNamespace("cellWise", quietly = TRUE)) {
  expect_true(exists(".gloc_start_alpha", envir = asNamespace("VIM"), inherits = FALSE) &&
                identical(get(".gloc_start_alpha", envir = asNamespace("VIM")), 0.5))
  w_soft <- collect_warnings(VIM::imputeCellGLoc(da, design = ~ 1, weights = "soft",
                                                 alpha = 0.75))
  expect_false(any(grepl("robust start: cellWise::cellMCD", w_soft)))
  # the user's alpha still governs the binary corner, which refuses these data
  w_bin <- collect_warnings(VIM::imputeCellGLoc(da, design = ~ 1, weights = "binary",
                                                alpha = 0.75))
  expect_true(any(grepl("cellMCD\\(\\) failed", w_bin)))

  # cellWise's cwLocScat drops rows whose weights are all zero and warns that it
  # did. A row of missing or flagged cells carries no weight in the likelihood,
  # so dropping it changes nothing, and the message must not reach users.
  dz <- da; dz[1:3, ] <- NA
  for (st in c("robust", "classical")) {
    w_z <- collect_warnings(VIM::imputeCellGLoc(dz, design = ~ 1, weights = "soft",
                                                start = st))
    expect_false(any(grepl("only zero weights", w_z)), info = st)
  }

  # cellWise::cellMCD() stops with "mean(): object has no elements" as soon as
  # one row has no observed cell, at any alpha, which silently sent the robust
  # start to its MAD fallback. Such a row carries no flag information, so the
  # start hands cellMCD only rows with an observed cell.
  Xna <- Xa[!apply(is.na(Xa), 1, all), ]
  Xna[1, ] <- NA
  w_na <- collect_warnings(st_na <- VIM:::.gloc_start_robust(
    Xna, matrix(1, nrow(Xna), 1, dimnames = list(NULL, "(Intercept)")), is.na(Xna)))
  expect_false(any(grepl("cellMCD", w_na)))
  expect_true(all(st_na$W[1, ] == 0))
}

# --- lmrob's default S-estimator start does not converge on many purely
# categorical designs (S refinements run out of k.max, or "initial estim. not
# converged"), and each such column used to start from the median with zero
# contrasts, i.e. without its group structure. It happened in 42 of 90 pilot
# fits with design = ~ ., at eps = 0 as often as under contamination, and for
# 54 of 960 column fits on data generated as below. These seeds are ones where
# it did. Data use Z %*% chol(S), so they are the same on every platform.
make_start_data <- function(seed, eps, n = 200, p = 6) {
  set.seed(seed)
  f <- data.frame(f1 = factor(sample(letters[1:3], n, TRUE)),
                  f2 = factor(sample(letters[1:3], n, TRUE)),
                  f3 = factor(sample(letters[1:2], n, TRUE)),
                  f4 = factor(sample(letters[1:4], n, TRUE)))
  U <- model.matrix(~ ., f)
  Bt <- matrix(rnorm(ncol(U) * p, sd = 1.5), ncol(U), p)
  X <- U %*% Bt + matrix(rnorm(n * p), n) %*% chol(0.5 * diag(p) + 0.5)
  if (eps > 0) { k <- matrix(runif(n * p) < eps, n); X[k] <- X[k] + 10 }
  X[matrix(runif(n * p) < 0.2, n)] <- NA
  colnames(X) <- paste0("x", 1:p)
  list(X = X, U = U)
}
if (requireNamespace("cellWise", quietly = TRUE)) {
  for (cs in list(c(9, 0), c(11, 0), c(18, 0), c(2, 0.1), c(3, 0.1), c(11, 0.1))) {
    dd <- make_start_data(cs[1], cs[2])
    w_l <- collect_warnings(st_l <- VIM:::.gloc_start_robust(dd$X, dd$U, is.na(dd$X)))
    lab <- sprintf("seed %d, eps %.1f", cs[1], cs[2])
    expect_false(any(grepl("lmrob", w_l)), info = lab)
    # no column fell back to zero contrasts
    expect_true(all(colSums(abs(st_l$B[-1, , drop = FALSE])) > 0), info = lab)
  }
}

# ==========================================================================
# Imputation conditions on unflagged peers only (7.4.1)
#
# Until 7.4.0 a missing cell was imputed from every observed cell in its row,
# flagged ones included, so a grossly contaminated peer went straight into the
# imputation. In the 7.4.1 pilot this made the robust start look worse on
# imputation MSE (design = ~ 1, eps = 0.20, delta = 10: +2.91 over the
# classical start, 10 of 10 reps) although its estimates were better; imputed
# from unflagged peers only, it was better (3.61 against 4.34). Imputation now
# uses the detection peer rule: peer_w_min with the peer band.
# ==========================================================================

set.seed(71)
S4 <- 0.5 * diag(4) + 0.5
colnames(S4) <- rownames(S4) <- paste0("x", 1:4)
n4 <- 200
X4 <- matrix(rnorm(n4 * 4), n4) %*% chol(S4)
colnames(X4) <- paste0("x", 1:4)
X4[1, 1] <- NA; X4[1, 2] <- X4[1, 2] + 50    # row 1: x1 missing, x2 grossly contaminated
X4[2, 1] <- NA                                # row 2: x1 missing, clean peers
X4[3, 3] <- NA                                # row 3: x3 missing, every peer flagged below
X4[sample(4:n4, 30), 2] <- NA                 # other missingness in the mix
M4 <- is.na(X4)
U4 <- matrix(1, n4, 1, dimnames = list(NULL, "(Intercept)"))
B4 <- matrix(0, 1, 4, dimnames = list("(Intercept)", colnames(X4)))
W4 <- matrix(1, n4, 4); W4[M4] <- 0
W4[1, 2] <- 0                                  # the contaminated peer is flagged
W4[3, c(1, 2, 4)] <- 0                         # every peer of row 3 is flagged

# (a) a flagged peer is exactly an absent one
X4na <- X4; X4na[1, 2] <- NA
imp_flag <- VIM:::.gloc_impute(X4, U4, B4, S4, M4, W = W4)
imp_na   <- VIM:::.gloc_impute(X4na, U4, B4, S4, is.na(X4na))
expect_equal(imp_flag[1, 1], imp_na[1, 1], tolerance = 1e-12)
# the 7.4.0 rule pulled that imputation towards the +50 cell, by about 12
expect_true(abs(VIM:::.gloc_impute(X4, U4, B4, S4, M4)[1, 1] - imp_na[1, 1]) > 5)

# (b) no flagged peer: exactly the 7.4.0 answer
W4c <- matrix(1, n4, 4); W4c[M4] <- 0
expect_identical(VIM:::.gloc_impute(X4, U4, B4, S4, M4, W = W4c),
                 VIM:::.gloc_impute(X4, U4, B4, S4, M4))
# ... and with flagged peers elsewhere, the rows without one are unchanged
clean_rows <- rowSums(M4) > 0 & rowSums(!M4 & W4 < 1) == 0
old4 <- VIM:::.gloc_impute(X4, U4, B4, S4, M4)
expect_true(sum(clean_rows) > 20)
expect_equal(imp_flag[clean_rows, ][M4[clean_rows, ]],
             old4[clean_rows, ][M4[clean_rows, ]], tolerance = 1e-12)

# (c) the band: sweeping one peer's weight across peer_w_min moves the
# imputation continuously where the hard cut jumps. Row 5: x1 missing, x2 = 5.
X5 <- X4; X5[5, 1] <- NA; X5[5, 2] <- 5
M5 <- is.na(X5)
W5 <- matrix(1, n4, 4); W5[M5] <- 0
grid5 <- seq(0.30, 0.70, by = 0.002)
sweep_imp <- function(band) vapply(grid5, function(w) {
  Wx <- W5; Wx[5, 2] <- w
  VIM:::.gloc_impute(X5, U4, B4, S4, M5, W = Wx, w_min = 0.5, band = band)[5, 1]
}, numeric(1))
i_hard <- sweep_imp(0)
i_band <- sweep_imp(VIM:::.gloc_peer_band)
expect_true(max(abs(diff(i_hard))) > 0.5)          # the hard cut jumps, by about 1.2
expect_true(max(abs(diff(i_band))) < max(abs(diff(i_hard))) / 10)
outside5 <- abs(grid5 - 0.5) > VIM:::.gloc_peer_band + 1e-8
expect_equal(i_hard[outside5], i_band[outside5])

# (d) the conditional covariance returned for draws uses the same peers.
# Analytic values for Sigma = 0.5 I + 0.5: Var(x1 | x2, x3, x4) = 0.625,
# Var(x1 | x3, x4) = 2/3, and with no usable peer the marginal variance 1.
ic <- VIM:::.gloc_impute(X4, U4, B4, S4, M4, W = W4, cov = TRUE)
expect_identical(ic$X, imp_flag)
expect_equal(ic$cond_cov[["2"]][1, 1], 0.625, tolerance = 1e-12)
expect_equal(ic$cond_cov[["1"]][1, 1], 2 / 3, tolerance = 1e-12)
icna <- VIM:::.gloc_impute(X4na, U4, B4, S4, is.na(X4na), cov = TRUE)
expect_equal(ic$cond_cov[["1"]][1, 1], icna$cond_cov[["1"]]["x1", "x1"],
             tolerance = 1e-12)
expect_equal(ic$cond_cov[["3"]], S4[3, 3, drop = FALSE])  # every peer flagged
# ... and the fitted mean. unname(): a single cell of a matrix with column
# names comes back as a named scalar, which is not what is being compared.
expect_equal(unname(ic$X[3, 3]), 0)

# (e) end to end, for both corners and both starts: the fit passes its own
# weights, so the +50 cell is not used to impute its row-mate
if (requireNamespace("cellWise", quietly = TRUE)) {
  d4 <- as.data.frame(X4)
  for (cs in list(c("soft", "robust"), c("soft", "classical"),
                  c("binary", "classical"))) {
    lab <- paste(cs, collapse = "/")
    f4 <- suppressWarnings(VIM::imputeCellGLoc(d4, design = ~ 1, weights = cs[1],
                                               start = cs[2]))
    expect_true(f4$W[1, 2] < 0.5 - VIM:::.gloc_peer_band, info = lab)
    ref_na <- VIM:::.gloc_impute(X4na, f4$U, f4$B, f4$Sigma, is.na(X4na), W = f4$W)
    expect_equal(f4$imputed$x1[1], unname(ref_na[1, 1]), tolerance = 1e-10, info = lab)
  }
}

# ==========================================================================
# Robust start on a design without an intercept column (7.4.1 review round)
#
# The start centres each response by its median and used to add the median
# back to B[1, j] only, i.e. to the first design column, assumed to be the
# intercept. With design = ~ f - 1 that column is the first level's dummy: the
# start's fitted means came out 10.25 / 0.37 / 10.16 against a truth of
# 10 / 20 / 30, and the final fit flagged 210 cells (172 of them clean) against
# 55 (17 clean) under the classical start, with no warning. The classical
# start's 17 clean flags, and 19 for the robust start with ~ f, were measured
# before the thresholds below were fixed.
# ==========================================================================
set.seed(301)
n_ni <- 300
f_ni <- factor(sample(c("a", "b", "c"), n_ni, TRUE))
mu_ni <- c(a = 10, b = 20, c = 30)
X_ni <- matrix(rnorm(n_ni * 3), n_ni) %*% chol(0.5 * diag(3) + 0.5) +
  mu_ni[as.character(f_ni)]
colnames(X_ni) <- paste0("x", 1:3)
X_ni[matrix(runif(n_ni * 3) < 0.1, n_ni)] <- NA
inj_ni <- matrix(runif(n_ni * 3) < 0.05, n_ni) & !is.na(X_ni)
X_ni[inj_ni] <- X_ni[inj_ni] + 8
d_ni <- data.frame(X_ni, f = f_ni)
first_ni <- match(levels(f_ni), f_ni)
if (requireNamespace("cellWise", quietly = TRUE)) {
  for (des in list(~ f, ~ f - 1)) {
    lab <- deparse(des)
    U_ni <- VIM:::.gloc_design(d_ni, des, "f")
    st_ni <- suppressWarnings(VIM:::.gloc_start_robust(X_ni, U_ni, is.na(X_ni)))
    expect_true(all(abs((U_ni %*% st_ni$B)[first_ni, 1] - mu_ni) < 0.5), info = lab)
    w_ni <- collect_warnings(fit_ni <- VIM::imputeCellGLoc(d_ni, design = des,
                                                           weights = "soft"))
    expect_false(any(grepl("robust start", w_ni)), info = lab)
    fl_ni <- fit_ni$W < 0.5 & !is.na(X_ni)
    expect_true(mean(fl_ni[inj_ni]) >= 0.95, info = lab)
    expect_true(sum(fl_ni & !inj_ni) <= 25, info = lab)
  }
}

# ==========================================================================
# The caller's random-number state (7.4.1 review round)
#
# cellWise::cellMCD, with DDC and estLocScale inside it, creates .Random.seed in
# a session that has none; cwLocScat and the start's L1 path do not. So the
# default soft fit, through the robust start, and the binary corner left a
# .Random.seed behind, and two fresh sessions then drew identical "random"
# numbers after one call. An existing stream must not be advanced either.
# ==========================================================================
if (requireNamespace("cellWise", quietly = TRUE)) {
  has_seed <- function() exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  saved_seed <- if (has_seed()) get(".Random.seed", envir = globalenv()) else NULL
  d_rng <- data.frame(Xsb, g = g)
  for (wt in c("soft", "binary")) {
    if (has_seed()) rm(".Random.seed", envir = globalenv())
    invisible(suppressWarnings(VIM::imputeCellGLoc(d_rng, design = ~ ., weights = wt)))
    expect_false(has_seed(), info = wt)
  }
  set.seed(11); before_rng <- .Random.seed
  invisible(suppressWarnings(VIM::imputeCellGLoc(d_rng, design = ~ ., weights = "soft")))
  expect_identical(.Random.seed, before_rng)
  if (is.null(saved_seed)) {
    if (has_seed()) rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", saved_seed, envir = globalenv())
  }
}

# --- the start's cellMCD failure fallback. A column with more than half of its
# cells missing makes cellMCD refuse at the start's alpha. The start warns,
# flags by |residual| / MAD instead, and prints nothing: until this round
# cellMCD printed six console lines per refusal. The existing "failed" test
# above reaches lmrob's failure path, not this one. ---
if (requireNamespace("cellWise", quietly = TRUE)) {
  set.seed(91)
  X_mf <- matrix(rnorm(300 * 4), 300) %*% chol(0.5 * diag(4) + 0.5)
  colnames(X_mf) <- paste0("x", 1:4)
  X_mf[sample(300, 180), 2] <- NA
  k_mf <- matrix(FALSE, 300, 4); k_mf[sample(300 * 4, 60)] <- TRUE
  k_mf <- k_mf & !is.na(X_mf)
  X_mf[k_mf] <- X_mf[k_mf] + 8
  U_mf <- matrix(1, 300, 1, dimnames = list(NULL, "(Intercept)"))
  con_mf <- capture.output(
    w_mf <- collect_warnings(st_mf <- VIM:::.gloc_start_robust(X_mf, U_mf, is.na(X_mf))))
  expect_true(any(grepl("robust start: cellWise::cellMCD() failed", w_mf, fixed = TRUE)))
  expect_equal(length(con_mf), 0L)
  expect_true(mean(st_mf$W[k_mf] == 0) > 0.90)
  expect_true(mean(st_mf$W[!k_mf & !is.na(X_mf)] == 0) < 0.05)
}

# --- a rank-deficient design says so. With an aliased factor (f2 a copy of
# f1) the start used to warn that "robustbase::lmrob did not converge", which
# names the symptom, not the cause. ---
if (requireNamespace("cellWise", quietly = TRUE)) {
  set.seed(93)
  f1_rd <- factor(sample(c("a", "b", "c"), 240, TRUE))
  X_rd <- matrix(rnorm(240 * 3), 240) %*% chol(0.5 * diag(3) + 0.5)
  colnames(X_rd) <- paste0("x", 1:3)
  U_rd <- VIM:::.gloc_design(data.frame(f1 = f1_rd, f2 = f1_rd), ~ ., c("f1", "f2"))
  w_rd <- collect_warnings(st_rd <- VIM:::.gloc_start_robust(X_rd, U_rd, is.na(X_rd)))
  expect_true(any(grepl("rank deficient", w_rd, fixed = TRUE)))
  expect_false(any(grepl("did not converge", w_rd, fixed = TRUE)))
  expect_false(anyNA(st_rd$B))
}

# --- start comes after trace, so positional calls that pass trace keep working ---
expect_identical(tail(names(formals(VIM::imputeCellGLoc)), 2), c("trace", "start"))
