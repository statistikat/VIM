#' Build the categorical design matrix for cellGLoc
#'
#' Missing factor values become an explicit level rather than dropped rows, so
#' every observation keeps a design row.
#'
#' @param data a data.frame containing the categorical variables.
#' @param design a one-sided formula, e.g. \code{~ .} or \code{~ 1}.
#' @param cat_vars character vector naming the categorical columns.
#' @return an \code{n x q} numeric matrix whose first column is the intercept.
#' @keywords internal
.gloc_design <- function(data, design, cat_vars) {
  if (is.null(design)) design <- ~ 1
  if (!length(cat_vars) || identical(all.vars(design), character(0))) {
    return(matrix(1, nrow = nrow(data), ncol = 1,
                  dimnames = list(NULL, "(Intercept)")))
  }
  df <- data[, cat_vars, drop = FALSE]
  for (v in names(df)) {
    df[[v]] <- as.factor(df[[v]])
    if (anyNA(df[[v]])) df[[v]] <- addNA(df[[v]], ifany = TRUE)
  }
  mf <- stats::model.frame(design, data = df, na.action = stats::na.pass,
                           drop.unused.levels = TRUE)
  stats::model.matrix(design, mf)
}

#' Weighted least-squares update of the cellGLoc mean structure
#'
#' Column \code{j} of \code{B} is fitted on the design using that column's cell
#' weights as observation weights. Cross-response dependence is handled by the
#' scatter step, not here.
#'
#' A column left with no cell of positive weight (e.g. every weight is zero)
#' cannot be fit at all; rather than silently returning \code{NA} into
#' downstream matrices, this case warns (naming the offending column) and
#' falls back to the unweighted median of that column's finite values, or to
#' \code{0} if none exists.
#'
#' @param X \eqn{n x p} numeric matrix of continuous variables, may contain NA.
#' @param U \eqn{n x q} design matrix from \code{.gloc_design}.
#' @param W \eqn{n x p} matrix of cell weights in \[0, 1\].
#' @return a \eqn{q x p} matrix of coefficients.
#' @keywords internal
.gloc_update_B <- function(X, U, W) {
  p <- ncol(X); q <- ncol(U)
  B <- matrix(0, q, p, dimnames = list(colnames(U), colnames(X)))
  for (j in seq_len(p)) {
    w  <- W[, j]
    ok <- is.finite(X[, j]) & is.finite(w) & w > 0
    if (sum(ok) <= q) {
      if (sum(ok) == 0L) {
        cn <- colnames(X)[j]
        if (is.null(cn) || is.na(cn) || !nzchar(cn)) cn <- as.character(j)
        warning(sprintf(
          "cellGLoc: column '%s' has no cell with positive weight; using the unweighted median as a deterministic fallback (0 if no finite value exists)",
          cn), call. = FALSE)
        finite_x <- X[is.finite(X[, j]), j]
        B[1L, j] <- if (length(finite_x)) stats::median(finite_x) else 0
      } else {
        B[1L, j] <- stats::median(X[ok, j])
      }
      next
    }
    sw <- sqrt(w[ok])
    fit <- tryCatch(qr.solve(U[ok, , drop = FALSE] * sw, X[ok, j] * sw),
                    error = function(e) NULL)
    if (is.null(fit)) {                       # rank-deficient design: intercept only
      B[1L, j] <- stats::weighted.mean(X[ok, j], w[ok])
    } else B[, j] <- fit
  }
  B
}

#' Half-width of the peer-reliability band in \code{.gloc_cond_resid}
#'
#' The peer-inclusion rule \eqn{w > w_{\min}} is applied over a band
#' \eqn{[w_{\min} - h, w_{\min} + h]} rather than at a point, which is what
#' makes the soft corner's weight map continuous; see the "peer band" section
#' of \code{.gloc_cond_resid} for the argument and the construction.
#'
#' The value is a compromise between two measurable things. Too narrow and the
#' map is continuous but so steep that the relaxed iteration still overshoots:
#' the damped update \eqn{w \mapsto (1 - d) w + d f(w)} is locally contracting
#' only while \eqn{f' > 1 - 2/d}, and \eqn{f'} across the band is about
#' \eqn{-\Delta / 2h} where \eqn{\Delta \approx 0.2} is the measured jump a
#' single threshold crossing used to produce, so \eqn{h} must exceed about
#' \eqn{\Delta d / 2} -- 0.025 at an unrelaxed step and less when relaxed. Too
#' wide and the interpolation stops being a fringe: bisquare weights sit at
#' 0.5 at \eqn{|z| = 2.55}, so a band of \eqn{\pm h} covers the standardised
#' residuals in roughly \eqn{|z| \in (2.55 - 4h, 2.55 + 4h)}, which at
#' \eqn{h = 0.05} is about 1.2\% of clean Gaussian cells and at \eqn{h = 0.25}
#' would be nearer 10\%.
#'
#' 0.05 sits above the steepness bound and below the fringe bound. Measured on
#' a sweep of one cell's weight across the threshold, the largest one-step jump
#' it leaves in a row-mate's standardised residual is 0.0035, against 0.110 for
#' the hard cut, with identical values at both ends of the sweep.
#'
#' @format a length-one numeric.
#' @keywords internal
.gloc_peer_band <- 0.05

#' Standardised conditional residuals of each cell given the others in its row
#'
#' For each target column \code{j}, rows are grouped by which peer columns are
#' actually observed (finite) in that row, and the conditional mean and
#' conditional variance are computed from the submatrix of \code{Sigma} for
#' that observed peer set -- one matrix inversion per distinct missingness
#' pattern, not one per row. A row whose peers are all absent falls back to
#' the marginal distribution of column \code{j} (conditional mean 0,
#' conditional variance \code{Sigma[j, j]}), because a mean of 0 is only the
#' right no-information answer when it is paired with the no-information
#' (marginal) variance -- dividing it by the full-peer-set conditional
#' variance instead would understate the true spread and inflate the
#' standardised residual.
#'
#' When \code{W} is supplied, a peer counts as usable only if it is finite
#' \emph{and} its cell weight exceeds \code{w_min}: a downweighted peer is
#' treated exactly like an absent one and simply joins the unobserved set, so
#' the grouping machinery above handles it unchanged. Without this, a single
#' contaminated cell corrupts the conditional mean of every other cell in its
#' row and they are all flagged -- outlier propagation. Measured on data whose
#' only contamination was in \code{x1}, the flag rate for the clean peers
#' \code{x2} and \code{x3} in those same rows was 0.875 and 0.940, against
#' 0.017 in clean rows; excluding downweighted peers brings it back to the
#' clean-row rate. This also aligns the soft corner with
#' \code{cellWise::cellMCD}, which predicts a flagged cell from the clean cells
#' in the same row.
#'
#' @section The peer band:
#' Taken literally, "usable iff \eqn{w > w_{\min}}" makes the weight map
#' \emph{discontinuous}: a cell whose weight sits at the threshold switches its
#' row-mates' conditioning set on and off, and their standardised residuals
#' jump. A discontinuous self-map of \eqn{[0, 1]^{n \times p}} need not have a
#' fixed point at all (Brouwer needs continuity), so an iteration asked to
#' drive \eqn{\max|f(W) - W|} below a tolerance can be asked for something that
#' does not exist -- and in the categorical-mean-structure arm it routinely was:
#' one to twenty cells recrossed the threshold every few iterations forever,
#' each crossing kicking the fixed-point residual back up by 0.1 to 0.2 while
#' the rest of the system contracted geometrically.
#'
#' The threshold is therefore a \emph{band}, not a cut. A peer's reliability
#' \eqn{r} ramps smoothly (\eqn{3t^2 - 2t^3}) from 0 at \eqn{w_{\min} - h} to 1
#' at \eqn{w_{\min} + h}, and a partially reliable peer is conditioned on as
#' though it were observed with independent measurement error of variance
#' \eqn{\sigma_{kk}(1/r - 1)}: exact at \eqn{r = 1}, infinitely noisy and hence
#' absent at \eqn{r = 0}. Both endpoints reproduce the hard rule exactly, so
#' nothing changes for a cell that is confidently clean or confidently flagged;
#' only the ambiguous fringe is interpolated, and it is interpolated the way
#' the rest of the soft corner already treats partial information. The map
#' becomes continuous in \eqn{W}, a fixed point exists, and the convergence
#' test can stay exactly as strict as it was.
#'
#' Implementation notes. (i) The noise-inflated system is solved in the scaled
#' form \eqn{\Sigma_{jS} D G^{-1} D} with \eqn{D = \mathrm{diag}(\sqrt r)} and
#' \eqn{G = D \Sigma_{SS} D + \mathrm{diag}(\sigma_{kk}(1 - r))}, which
#' interpolates between \eqn{\mathrm{diag}(\sigma_{kk})} at \eqn{r = 0} and
#' \eqn{\Sigma_{SS}} at \eqn{r = 1} and is well conditioned throughout -- the
#' unscaled \eqn{\Sigma_{SS} + \mathrm{diag}(\sigma_{kk}(1/r - 1))} blows up as
#' \eqn{r \to 0}. (ii) Rows whose peers are all at \eqn{r \in \{0, 1\}} keep
#' the pattern grouping and one inversion per pattern; only rows holding a peer
#' inside the band pay for an inversion of their own. With \eqn{h} small that
#' is a handful of rows, and the whole function is about 1\% of an iteration.
#' (iii) \code{band = 0} restores the hard cut bit-for-bit, which is how the
#' tests pin the endpoint behaviour.
#'
#' @param R \eqn{n x p} matrix of residuals from the mean structure.
#' @param Sigma \eqn{p x p} scatter matrix.
#' @param W optional \eqn{n x p} matrix of cell weights. \code{NULL} (default)
#'   conditions on every finite peer, which is the behaviour existing callers
#'   rely on.
#' @param w_min weight above which a peer counts as clean enough to condition
#'   on. The default 0.5 is the conventional 1% flagging rule: the Tukey
#'   bisquare weight at \eqn{|z| = 2.576} is 0.487, so "weight below 0.5" and
#'   "flagged at the 99% cut-off" coincide.
#' @param band half-width \eqn{h} of the reliability ramp around \code{w_min};
#'   see the section above. \code{0} is the hard cut. The default
#'   \code{.gloc_peer_band} is deliberately narrow.
#' @return an \eqn{n x p} matrix of standardised conditional residuals.
#' @keywords internal
.gloc_cond_resid <- function(R, Sigma, W = NULL, w_min = 0.5,
                             band = .gloc_peer_band) {
  n <- nrow(R); p <- ncol(R)
  Z <- matrix(NA_real_, n, p, dimnames = dimnames(R))
  if (p == 1L) return(R / sqrt(Sigma[1L, 1L]))
  obs <- is.finite(R)
  if (is.null(W)) {
    Rel <- obs + 0
  } else {
    Wf <- W
    Wf[!is.finite(Wf)] <- 0
    Rel <- if (!is.finite(band) || band <= 0) (Wf > w_min) + 0 else {
      tt <- pmin(pmax((Wf - (w_min - band)) / (2 * band), 0), 1)
      tt * tt * (3 - 2 * tt)
    }
    Rel[!obs] <- 0
    dim(Rel) <- dim(W)
  }
  sdiag <- diag(Sigma)
  for (j in seq_len(p)) {
    mj   <- setdiff(seq_len(p), j)
    Relj <- Rel[, mj, drop = FALSE]
    sharp <- rowSums(Relj > 0 & Relj < 1) == 0L

    # ---- peers are in or out: one inversion per distinct pattern, as before
    idx <- which(sharp)
    if (length(idx)) {
      pat <- Relj[idx, , drop = FALSE] > 0
      # string key rather than a 2^k dot product: downweighting multiplies the
      # number of distinct patterns, and the numeric code overflows past ~53 peers
      patcode <- do.call(paste, c(as.data.frame(pat + 0L), sep = ""))
      for (rows in split(idx, patcode)) {
        obs_idx <- mj[pat[match(rows[1L], idx), ]]
        if (length(obs_idx) == 0L) {              # no peer observed: marginal fallback
          mean_i <- 0
          cvar   <- Sigma[j, j]
        } else {
          Sinv <- tryCatch(chol2inv(chol(Sigma[obs_idx, obs_idx, drop = FALSE])),
                           error = function(e) MASS::ginv(Sigma[obs_idx, obs_idx, drop = FALSE]))
          beta   <- Sigma[j, obs_idx, drop = FALSE] %*% Sinv
          cvar   <- as.numeric(Sigma[j, j] - beta %*% Sigma[obs_idx, j, drop = FALSE])
          mean_i <- as.vector(R[rows, obs_idx, drop = FALSE] %*% t(beta))
        }
        cvar <- max(cvar, .Machine$double.eps)
        Z[rows, j] <- (R[rows, j] - mean_i) / sqrt(cvar)
      }
    }

    # ---- at least one peer inside the band: one solve for that row
    for (i in which(!sharp)) {
      r <- Relj[i, ]
      keep <- which(r > 0)
      if (!length(keep)) {
        Z[i, j] <- R[i, j] / sqrt(max(Sigma[j, j], .Machine$double.eps))
        next
      }
      kk <- mj[keep]; rk <- r[keep]; dd <- sqrt(rk)
      G  <- outer(dd, dd) * Sigma[kk, kk, drop = FALSE] +
              diag(sdiag[kk] * (1 - rk), length(kk))
      Gi <- tryCatch(chol2inv(chol(G)), error = function(e) MASS::ginv(G))
      bt <- as.vector((as.vector(Sigma[j, kk]) * dd) %*% Gi)
      cvar <- max(Sigma[j, j] - sum(bt * (dd * Sigma[kk, j])), .Machine$double.eps)
      Z[i, j] <- (R[i, j] - sum(bt * (dd * R[i, kk]))) / sqrt(cvar)
    }
  }
  Z
}
