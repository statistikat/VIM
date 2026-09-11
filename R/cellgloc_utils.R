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
#' @param W \eqn{n x p} matrix of cell weights in [0, 1].
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
#' @param R \eqn{n x p} matrix of residuals from the mean structure.
#' @param Sigma \eqn{p x p} scatter matrix.
#' @param W optional \eqn{n x p} matrix of cell weights. \code{NULL} (default)
#'   conditions on every finite peer, which is the behaviour existing callers
#'   rely on.
#' @param w_min weight above which a peer counts as clean enough to condition
#'   on. The default 0.5 is the conventional 1\% flagging rule: the Tukey
#'   bisquare weight at \eqn{|z| = 2.576} is 0.487, so "weight below 0.5" and
#'   "flagged at the 99\% cut-off" coincide.
#' @return an \eqn{n x p} matrix of standardised conditional residuals.
#' @keywords internal
.gloc_cond_resid <- function(R, Sigma, W = NULL, w_min = 0.5) {
  n <- nrow(R); p <- ncol(R)
  Z <- matrix(NA_real_, n, p, dimnames = dimnames(R))
  if (p == 1L) return(R / sqrt(Sigma[1L, 1L]))
  usable <- is.finite(R)
  if (!is.null(W)) {
    Wf <- W
    Wf[!is.finite(Wf)] <- 0
    usable <- usable & (Wf > w_min)
  }
  for (j in seq_len(p)) {
    mj      <- setdiff(seq_len(p), j)
    pat     <- usable[, mj, drop = FALSE]
    # string key rather than a 2^k dot product: downweighting multiplies the
    # number of distinct patterns, and the numeric code overflows past ~53 peers
    patcode <- do.call(paste, c(as.data.frame(pat + 0L), sep = ""))
    for (rows in split(seq_len(n), patcode)) {
      obs_idx <- mj[pat[rows[1L], ]]
      if (length(obs_idx) == 0L) {                # no peer observed: marginal fallback
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
  Z
}
