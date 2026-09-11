#' Cellwise-robust estimation on a categorical mean structure
#'
#' Estimates the mean structure \eqn{B}, scatter \eqn{\Sigma} and cell weights
#' \eqn{W} of the model \eqn{x_i | u_i \sim N(B' u_i, \Sigma)}, where \eqn{u_i}
#' is a design row built from the categorical variables. Detection uses the
#' conditional residual of a cell given the other continuous cells in its row
#' \emph{and} the row's categorical pattern.
#'
#' With \code{design = ~ 1} the estimator reduces to the published continuous-only
#' estimators: to the cellwise MCD of Raymaekers and Rousseeuw (2024) with
#' \code{weights = "binary"}, and to the cellwise weighted maximum likelihood
#' estimator of Rousseeuw (2026) with \code{weights = "soft"}.
#'
#' The soft corner rescales the weighted scatter so that it is
#' Fisher-consistent at the Gaussian model. Without it the bisquare
#' downweighting deflates the scatter by about 21\% at the default tuning,
#' which would inflate the standardised residuals and make the estimator
#' over-flag. The correction is per column and depends on \eqn{\Sigma}, not a
#' single constant: the weights act on \emph{conditional} residuals, so they
#' shrink only the unpredictable part of each cell. See
#' \code{.gloc_correct_scatter}. It is exact for the scale at any correlation
#' and vanishes at \code{psi_c = Inf}. The correlations themselves stay mildly
#' biased upward; see that function's note.
#'
#' Iteration stops when both the fitted means and the cell weights have settled:
#' \eqn{\max|U(B - B_{old})| / \sqrt{\max \mathrm{diag}(\Sigma)} < eps} and
#' \eqn{\max|W - W_{old}| < eps}. The first term is a change in the fitted
#' values measured in units of the scatter, not a change in a coefficient
#' measured against a location, so the criterion is invariant to shifting the
#' data. The soft corner's weight update is relaxed (see \code{.gloc_damp}),
#' because peer inclusion is a discrete decision and cells near the threshold
#' would otherwise oscillate forever.
#'
#' Continuous columns that are \code{integer} in \code{data} stay
#' \code{integer} in \code{$imputed}; their conditional expectations are
#' rounded.
#'
#' @param data a \code{data.frame} with continuous and categorical columns.
#' @param design one-sided formula for the categorical mean structure.
#'   \code{~ .} (default) is main effects over all categorical columns,
#'   \code{~ .^2} adds interactions, \code{~ 1} is intercept only.
#' @param weights \code{"soft"} for redescending weights in \[0, 1\],
#'   \code{"binary"} for the penalised cellwise MCD objective.
#' @param maxit maximum number of outer iterations. \code{0} returns the
#'   starting fit. The default is 200 rather than 50 because the damped
#'   iteration needed up to 51 steps across the sweep in \code{.gloc_damp};
#'   converged fits leave the loop early, so the cap costs nothing. Failing to
#'   converge within \code{maxit} warns.
#' @param eps convergence tolerance, applied to the scaled change in the
#'   fitted means and to the change in the cell weights.
#' @param alpha minimum fraction of unflagged cells per column (binary corner).
#' @param psi_c tuning constant of the Tukey bisquare (soft corner).
#'   \code{Inf} disables downweighting.
#' @param peer_w_min a cell is conditioned on only when its weight exceeds
#'   this, so that a downweighted peer is treated as absent rather than as
#'   evidence. The default 0.5 is the conventional 1\% flagging rule. Raising
#'   it discards more peers. A threshold of 0 does \emph{not} disable peer
#'   filtering: the damped weight update multiplies a weight by
#'   \eqn{1 - d} each time the bisquare sends it to zero, so a contaminated
#'   weight decays geometrically towards zero without ever attaining it
#'   (measured around 4e-6 at convergence, with no cell exactly 0), and a
#'   zero threshold readmits those cells at full influence. Use a negative
#'   value to condition on every finite peer, which is useful only for
#'   demonstrating what the filtering buys.
#' @param trace print progress.
#' @return a list with \code{B}, \code{Sigma}, \code{W}, \code{U},
#'   \code{imputed}, \code{converged} and \code{iterations}.
#' @references
#' Raymaekers, J. and Rousseeuw, P. J. (2024). The cellwise minimum covariance
#' determinant estimator. \emph{JASA} 119(548), 2610-2621.
#' \doi{10.1080/01621459.2023.2267777}
#'
#' Rousseeuw, P. J. (2026). Analyzing cellwise weighted data.
#' \emph{Econometrics and Statistics} 38, 31-41.
#' \doi{10.1016/j.ecosta.2023.01.007}
#' @export
imputeCellGLoc <- function(data, design = ~ ., weights = c("soft", "binary"),
                           maxit = 200, eps = 5e-3, alpha = 0.75,
                           psi_c = 4.685, peer_w_min = 0.5, trace = FALSE) {
  weights <- match.arg(weights)
  stopifnot(is.data.frame(data))
  is_cat <- vapply(data, function(x) is.factor(x) || is.character(x) ||
                     is.logical(x), logical(1))
  cont_vars <- names(data)[!is_cat]
  cat_vars  <- names(data)[is_cat]
  if (!length(cont_vars))
    stop("imputeCellGLoc() needs at least one continuous variable.")

  X <- as.matrix(data[, cont_vars, drop = FALSE])
  storage.mode(X) <- "double"
  U <- .gloc_design(data, design, cat_vars)
  n <- nrow(X); p <- ncol(X)

  # Inf / NaN are treated as missing and imputed, which is a real decision
  # about the user's data, so say so rather than doing it silently.
  odd <- !is.finite(X) & !is.na(X)
  if (any(odd)) {
    warning(sprintf(paste("cellGLoc: %d non-finite value(s) that are not NA",
                          "(Inf, -Inf or NaN) were treated as missing and will",
                          "be imputed; check whether that is intended."),
                    sum(odd)), call. = FALSE)
    X[odd] <- NA_real_
  }

  M <- !is.finite(X)                       # missing mask
  W <- matrix(1, n, p, dimnames = dimnames(X))
  W[M] <- 0
  B <- .gloc_update_B(X, U, W)
  Sigma <- NULL
  converged <- FALSE
  iter_count <- 0L

  kappa_soft <- .gloc_consistency(psi_c, "bisquare")
  hard_q     <- sqrt(stats::qchisq(0.99, df = 1))
  kappa_hard <- .gloc_consistency(hard_q, "hard")

  # A degraded scatter path must never be taken silently, but neither should it
  # shout once per iteration: report each distinct reason exactly once per call.
  seen <- character(0)
  dedup <- function(w) {
    m <- conditionMessage(w)
    if (startsWith(m, "cellGLoc: ")) {
      if (m %in% seen) invokeRestart("muffleWarning") else seen <<- c(seen, m)
    }
  }

  withCallingHandlers({
    for (it in seq_len(maxit)) {
      iter_count <- it
      B_old <- B; W_old <- W
      R <- X - U %*% B

      if (weights == "binary") {
        if (!requireNamespace("cellWise", quietly = TRUE))
          stop('weights = "binary" requires the cellWise package.')
        Rf <- R; Rf[M] <- NA_real_
        # cellMCD keeps its own (free) centre here on purpose. That centre is a
        # nuisance parameter, profiled out inside cellMCD and discarded: B alone
        # carries the mean structure. Passing fixedCenter = TRUE would instead
        # switch cellMCD's *preliminary* standardisation to
        # estLocScale(., center = FALSE), and since cellMCD rebuilds its scatter
        # as diag(rscales) %*% cov2cor(.) %*% diag(rscales), that preliminary
        # scale becomes sqrt(diag(S)) verbatim. Residuals that are not exactly
        # centred would then inflate the scale, and the reduction to cellMCD
        # would only hold to about 6% instead of to machine precision.
        fit <- tryCatch(cellWise::cellMCD(Rf, alpha = alpha,
                                          checkPars = list(coreOnly = TRUE,
                                                           silent = TRUE)),
                        error = function(e) NULL)
        if (is.null(fit)) {
          warning(paste("cellGLoc: cellWise::cellMCD() failed; falling back to",
                        "a weighted covariance with a hard threshold on the",
                        "conditional residuals. This is a cruder estimator than",
                        "the cellwise MCD and the result is not the published",
                        "cellMCD."), call. = FALSE)
          Sigma <- .gloc_scatter_soft(R, W, M)          # working scatter
          Z <- .gloc_cond_resid(R, Sigma, W = W, w_min = peer_w_min)
          # recompute W rather than carrying the previous iteration's stale one
          W <- matrix(as.numeric(is.finite(Z) & abs(Z) <= hard_q), n, p,
                      dimnames = dimnames(X))
          W[M] <- 0
          Sigma <- .gloc_scatter_soft(R, W, M, kappa = kappa_hard)
        } else {
          Sigma <- fit$S; W <- fit$W
        }
      } else {
        Sigma <- .gloc_scatter_soft(R, W, M, kappa = kappa_soft)
        # Condition each cell only on peers that are themselves still clean.
        # Conditioning on every finite peer propagates a single bad cell to its
        # whole row: contaminating only x1 flagged 88-94% of the clean x2 and
        # x3 cells in those same rows, against 1.7% in clean rows. cellMCD does
        # not have this problem because a flagged cell leaves the conditioning
        # set, and the reduction claim requires the soft corner to match.
        Z <- .gloc_cond_resid(R, Sigma, W = W, w_min = peer_w_min)
        # Damped weight update. Peer inclusion is a DISCRETE decision, so the
        # undamped map is discontinuous and a cell whose weight sits near
        # w_min flips in and out forever: an exact period-2 limit cycle in
        # which B was stable to 4e-4 and Sigma to 9e-4, yet 7 cells of 4500
        # kept swinging |dW| = 0.325 and convergence was never declared.
        # Damping leaves every genuine fixed point untouched (W = f(W) implies
        # W = (1 - d) W + d f(W)) and collapses the cycle to its average.
        W <- (1 - .gloc_damp) * W + .gloc_damp * .gloc_bisquare(Z, psi_c)
      }
      W[M] <- 0
      B <- .gloc_update_B(X, U, W)

      # Scale the change in the FITTED MEANS by the scatter. Normalising a
      # coefficient change by a location (max|B_old|) would make the tolerance
      # scale with the data's offset: shifting the data by +1000 loosened it
      # 200,000-fold and stopped the iteration with the weights still moving.
      sd_ref <- sqrt(max(diag(Sigma)))
      if (!is.finite(sd_ref) || sd_ref <= 0) sd_ref <- 1
      dB <- max(abs(U %*% (B - B_old))) / sd_ref
      dW <- max(abs(W - W_old))
      if (trace) message(sprintf(
        "  iter %d: scaled change in fitted means = %.3g, max |dW| = %.3g",
        it, dB, dW))
      if (dB < eps && dW < eps) { converged <- TRUE; break }
    }

    # Non-convergence is now the likeliest degraded path, and it was the only
    # silent one: every other degraded path in this function warns.
    if (maxit >= 1L && !converged)
      warning(sprintf(paste("cellGLoc: did not converge in %d iteration(s)",
                            "(scaled change in fitted means %.3g, max |dW|",
                            "%.3g, tolerance %.3g). The cell weights are still",
                            "moving, so B, Sigma and W are only whatever the",
                            "last iteration produced. Raise maxit; if max |dW|",
                            "has stalled at a constant, the peer-inclusion",
                            "decision is cycling and a smaller",
                            "VIM:::.gloc_damp is what helps."),
                      maxit, dB, dW, eps), call. = FALSE)

    if (is.null(Sigma))                                  # maxit = 0
      Sigma <- .gloc_scatter_soft(X - U %*% B, W, M,
                                  kappa = if (weights == "binary") 1
                                          else kappa_soft)
  }, warning = dedup)

  Ximp <- .gloc_impute(X, U, B, Sigma, M)
  out <- data
  for (v in cont_vars) out[[v]] <- .gloc_restore_class(Ximp[, v], data[[v]], v)

  list(B = B, Sigma = Sigma, W = W, U = U, imputed = out,
       converged = converged, iterations = iter_count)
}

#' Relaxation factor for the soft corner's weight update
#'
#' Peer inclusion in \code{.gloc_cond_resid} is a discrete decision, which
#' makes the undamped weight map discontinuous and lets cells sitting near the
#' inclusion threshold oscillate indefinitely. Relaxation removes that without
#' moving any genuine fixed point, since \eqn{W = f(W)} implies
#' \eqn{W = (1 - d) W + d f(W)}.
#'
#' The update has the same form as the damping in \code{imputeCellEM} and
#' \code{imputeCellwise}, which ramp \eqn{\lambda} adaptively rather than
#' holding it fixed.
#'
#' The value was chosen by sweeping 36 configurations -- 6 seeds x
#' \eqn{\rho \in \{0, 0.5, 0.8\}} x (clean, 5\% contaminated), \eqn{n = 800},
#' \code{maxit = 200}:
#'
#' \tabular{lrrr}{
#'   damping \tab converged \tab median iters \tab max iters \cr
#'   1.00 (none) \tab 21/36 \tab  6 \tab   9 \cr
#'   0.75        \tab 25/36 \tab 10 \tab  16 \cr
#'   0.50        \tab 33/36 \tab 17 \tab  26 \cr
#'   0.25        \tab 36/36 \tab 34 \tab  51 \cr
#'   0.10        \tab 36/36 \tab 66 \tab 126
#' }
#'
#' Every failure, at every damping level, is at \eqn{\rho \ge 0.5}; nothing
#' ever fails at \eqn{\rho = 0}, even undamped. That is the shape of the
#' boundary: the cycle is driven by correlation, because when the columns are
#' strongly correlated dropping one peer moves the conditional variance a long
#' way, so a cell near the inclusion threshold swings far enough to flip the
#' discrete decision back.
#'
#' @format a length-one numeric.
#'
#' @keywords internal
.gloc_damp <- 0.25

#' Gaussian consistency factor of a cell-weight function
#'
#' Returns \eqn{E[w(Z) Z^2] / E[w(Z)]} for \eqn{Z \sim N(0, 1)}, the factor by
#' which a \eqn{\sum w}-normalised weighted covariance under-states the scatter
#' at the Gaussian model. Dividing by it makes the weighted scatter
#' Fisher-consistent: at \eqn{\Sigma = \Sigma_0} the standardised conditional
#' residuals are exactly standard normal, the weighted scatter has expectation
#' \eqn{\kappa \Sigma_0}, and the corrected map therefore has \eqn{\Sigma_0} as
#' its fixed point.
#'
#' @param c tuning constant; \code{Inf} returns 1 (no downweighting).
#' @param type \code{"bisquare"} for Tukey weights, \code{"hard"} for a 0/1
#'   cut-off at \code{c}.
#' @return a scalar in (0, 1].
#' @seealso \code{.gloc_correct_scatter}, which is what applies it, and which
#'   must account for correlation between the columns.
#' @keywords internal
.gloc_consistency <- function(c, type = c("bisquare", "hard")) {
  type <- match.arg(type)
  if (!is.finite(c) || c <= 0) return(1)
  wf <- if (type == "bisquare") function(z) (1 - (z / c)^2)^2
        else function(z) rep(1, length(z))
  num <- try(stats::integrate(function(z) wf(z) * z^2 * stats::dnorm(z),
                              -c, c)$value, silent = TRUE)
  den <- try(stats::integrate(function(z) wf(z) * stats::dnorm(z),
                              -c, c)$value, silent = TRUE)
  if (inherits(num, "try-error") || inherits(den, "try-error") ||
      !is.finite(num) || !is.finite(den) || den <= 0) return(1)
  num / den
}

#' Make a weighted scatter Fisher-consistent at the Gaussian model
#'
#' Dividing the whole matrix by \code{kappa} is correct only when the columns
#' are independent. The weights are functions of the \emph{conditional}
#' residual, and a residual splits as \eqn{R_j = m_j + s_j Z_j} with the
#' predictable part \eqn{m_j} independent of \eqn{Z_j}. A weight
#' \eqn{w(Z_j)} therefore downweights only the \eqn{s_j Z_j} part, so
#'
#' \deqn{E[w R_j^2] / E[w] = (\sigma_j^2 - s_j^2) + \kappa s_j^2
#'       = \sigma_j^2 \{1 - (s_j^2/\sigma_j^2)(1 - \kappa)\},}
#'
#' where \eqn{s_j^2 = 1 / (\Sigma^{-1})_{jj}} is the conditional variance. The
#' per-column factor \eqn{\kappa_j = 1 - (s_j^2/\sigma_j^2)(1 - \kappa)}
#' reduces to \eqn{\kappa} under independence (\eqn{s_j^2 = \sigma_j^2}) and to
#' 1 when there is no downweighting. Because \eqn{\kappa_j} depends on
#' \eqn{\Sigma}, it is solved for by a short fixed-point iteration.
#'
#' The scaling is symmetric, \eqn{\Sigma \mapsto D \Sigma D} with
#' \eqn{D = \mathrm{diag}(\kappa_j^{-1/2})}, so it fixes the \emph{scale} and
#' leaves the correlation matrix alone. The correlations are themselves mildly
#' biased upward by conditional-residual downweighting (cells inconsistent with
#' their peers are exactly the ones removed); that bias is not corrected here.
#'
#' @param S_raw the uncorrected weighted scatter.
#' @param kappa the scalar consistency factor from \code{.gloc_consistency}.
#' @return the corrected scatter.
#' @keywords internal
.gloc_correct_scatter <- function(S_raw, kappa) {
  if (!is.finite(kappa) || kappa >= 1) return(S_raw)
  p <- ncol(S_raw)
  if (p == 1L) return(S_raw / kappa)
  S <- S_raw / kappa                       # independence-case starting value
  for (i in seq_len(50L)) {
    Sinv <- tryCatch(chol2inv(chol(S)), error = function(e) NULL)
    if (is.null(Sinv)) return(S_raw / kappa)
    ratio <- 1 / (diag(Sinv) * diag(S))    # s_j^2 / sigma_j^2, in [0, 1]
    ratio[!is.finite(ratio)] <- 1
    ratio <- pmin(pmax(ratio, 0), 1)
    d <- sqrt(1 / (1 - ratio * (1 - kappa)))
    Snew <- S_raw * outer(d, d)
    done <- max(abs(Snew - S)) < 1e-12 * max(abs(S))
    S <- Snew
    if (done) break
  }
  dimnames(S) <- dimnames(S_raw)
  S
}

#' Tukey bisquare weights for standardised conditional residuals
#'
#' @param Z matrix of standardised conditional residuals.
#' @param c tuning constant; \code{Inf} disables downweighting.
#' @return a matrix of weights in \[0, 1\] with the dimensions of \code{Z}.
#' @keywords internal
.gloc_bisquare <- function(Z, c = 4.685) {
  if (!is.finite(c)) return(matrix(1, nrow(Z), ncol(Z), dimnames = dimnames(Z)))
  u <- abs(Z) / c
  w <- (1 - u^2)^2
  w[!is.finite(u) | u > 1] <- 0
  w[!is.finite(Z)] <- 0
  matrix(w, nrow(Z), ncol(Z), dimnames = dimnames(Z))
}

#' Cellwise weighted maximum likelihood scatter of the residuals
#'
#' Uses \code{cellWise::cwLocScat()} when available. \pkg{cellWise} is in
#' Suggests, so its absence is a supported configuration rather than an edge
#' case; the fallback is a weighted pairwise covariance, which still honours
#' the cell weights, and it is never taken silently.
#'
#' @param R \eqn{n x p} matrix of residuals from the mean structure.
#' @param W \eqn{n x p} matrix of cell weights in \[0, 1\].
#' @param M \eqn{n x p} logical mask of missing cells.
#' @param kappa Gaussian consistency factor to divide by; see
#'   \code{.gloc_consistency}.
#' @param have_cw whether \pkg{cellWise} may be used; exposed so the fallback
#'   path is directly testable.
#' @return a \eqn{p x p} scatter matrix.
#' @keywords internal
.gloc_scatter_soft <- function(R, W, M, kappa = 1,
                               have_cw = requireNamespace("cellWise",
                                                          quietly = TRUE)) {
  Rna <- R; Rna[M] <- NA_real_
  if (have_cw) {
    Wc <- W; Wc[!is.finite(Wc)] <- 0
    fit <- tryCatch(cellWise::cwLocScat(Rna, W = Wc, methods = "all"),
                    error = function(e) NULL)
    if (!is.null(fit) && all(is.finite(fit$cwMLEsigma)))
      return(.gloc_correct_scatter(fit$cwMLEsigma, kappa))
    warning(paste("cellGLoc: cellWise::cwLocScat() failed or returned a",
                  "non-finite scatter; falling back to a weighted pairwise",
                  "covariance, which is a cruder estimator than the cellwise",
                  "weighted MLE."), call. = FALSE)
  } else {
    warning(paste("cellGLoc: the cellWise package is not installed, so the",
                  "cellwise weighted MLE scatter is unavailable; falling back",
                  "to a weighted pairwise covariance, which is a cruder",
                  "estimator. Install cellWise to get the published",
                  "estimator."), call. = FALSE)
  }
  .gloc_correct_scatter(.gloc_wcov(Rna, W), kappa)
}

#' Weighted pairwise covariance of residuals with cell weights
#'
#' Entry \eqn{(j, k)} uses the weights of both cells, \eqn{w_{ij} w_{ik}}, so a
#' downweighted cell is excluded from every covariance it takes part in. The
#' pairwise construction is not guaranteed non-negative definite, so the result
#' is repaired by flooring its eigenvalues.
#'
#' @param R \eqn{n x p} residual matrix, missing cells as \code{NA}.
#' @param W \eqn{n x p} matrix of cell weights.
#' @return a \eqn{p x p} non-negative definite matrix.
#' @keywords internal
.gloc_wcov <- function(R, W) {
  p <- ncol(R)
  nm <- list(colnames(R), colnames(R))
  obs <- is.finite(R)
  Wc <- W; Wc[!is.finite(Wc)] <- 0; Wc[!obs] <- 0
  Rz <- R; Rz[!obs] <- 0
  cw <- colSums(Wc)
  mu <- ifelse(cw > 0, colSums(Wc * Rz) / pmax(cw, .Machine$double.eps), 0)
  C <- sweep(Rz, 2, mu) * obs
  S <- matrix(0, p, p, dimnames = nm)
  for (j in seq_len(p)) for (k in j:p) {
    wjk <- Wc[, j] * Wc[, k]
    den <- sum(wjk)
    v <- if (den > 0) sum(wjk * C[, j] * C[, k]) / den else 0
    S[j, k] <- v; S[k, j] <- v
  }
  .gloc_psd(S)
}

#' Nearest non-negative definite repair of a symmetric matrix
#'
#' @param S a symmetric matrix.
#' @return \code{S} with its eigenvalues floored at a small positive value.
#' @keywords internal
.gloc_psd <- function(S) {
  ev <- tryCatch(eigen(S, symmetric = TRUE), error = function(e) NULL)
  if (is.null(ev)) return(S)
  top <- max(ev$values)
  if (!is.finite(top) || top <= 0) {
    out <- diag(max(.Machine$double.eps, mean(abs(diag(S)))), nrow(S))
    dimnames(out) <- dimnames(S)
    return(out)
  }
  floor_ev <- top * 1e-8
  if (min(ev$values) >= floor_ev) return(S)
  ev$values[ev$values < floor_ev] <- floor_ev
  out <- ev$vectors %*% diag(ev$values, nrow(S)) %*% t(ev$vectors)
  out <- (out + t(out)) / 2
  dimnames(out) <- dimnames(S)
  out
}

#' Fill missing continuous cells by their conditional expectation
#'
#' Rows are grouped by their missingness pattern, so one matrix inversion is
#' done per distinct pattern rather than one per row, matching
#' \code{.gloc_cond_resid}. A row with no observed continuous cell falls back
#' to its fitted mean.
#'
#' @param X \eqn{n x p} numeric matrix of continuous variables.
#' @param U \eqn{n x q} design matrix from \code{.gloc_design}.
#' @param B \eqn{q x p} matrix of mean-structure coefficients.
#' @param Sigma \eqn{p x p} scatter matrix.
#' @param M \eqn{n x p} logical mask of missing cells.
#' @return \code{X} with its missing cells replaced.
#' @keywords internal
.gloc_impute <- function(X, U, B, Sigma, M) {
  if (!any(M)) return(X)
  Mu <- U %*% B
  R  <- X - Mu
  Xi <- X
  rows <- which(rowSums(M) > 0)
  patcode <- apply(M[rows, , drop = FALSE], 1L,
                   function(r) paste0(as.integer(r), collapse = ""))
  for (idx in split(rows, patcode)) {
    i0   <- idx[1L]
    miss <- which(M[i0, ]); obs <- which(!M[i0, ])
    if (!length(obs)) { Xi[idx, miss] <- Mu[idx, miss, drop = FALSE]; next }
    Soo  <- Sigma[obs, obs, drop = FALSE]
    Sinv <- tryCatch(chol2inv(chol(Soo)), error = function(e) MASS::ginv(Soo))
    Beta <- Sigma[miss, obs, drop = FALSE] %*% Sinv          # |miss| x |obs|
    Xi[idx, miss] <- Mu[idx, miss, drop = FALSE] +
      R[idx, obs, drop = FALSE] %*% t(Beta)
  }
  Xi
}

#' Give an imputed column back the class of the column it came from
#'
#' @param x numeric vector of imputed values.
#' @param orig the original column.
#' @param nm the column name, for the warning.
#' @return \code{x} coerced back to \code{orig}'s class where that is safe.
#' @keywords internal
.gloc_restore_class <- function(x, orig, nm) {
  if (!is.integer(orig)) return(x)
  r <- round(x)
  if (all(is.finite(r)) && max(abs(r)) <= .Machine$integer.max)
    return(as.integer(r))
  warning(sprintf(paste("cellGLoc: column '%s' is integer but its imputed",
                        "values do not fit in an integer; returning double."),
                  nm), call. = FALSE)
  x
}
