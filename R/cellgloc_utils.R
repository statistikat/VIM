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
    if (sum(ok) <= q) { B[1L, j] <- stats::median(X[ok, j]); next }
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
#' @param R \eqn{n x p} matrix of residuals from the mean structure.
#' @param Sigma \eqn{p x p} scatter matrix.
#' @return an \eqn{n x p} matrix of standardised conditional residuals.
#' @keywords internal
.gloc_cond_resid <- function(R, Sigma) {
  p <- ncol(R)
  Z <- matrix(NA_real_, nrow(R), p, dimnames = dimnames(R))
  if (p == 1L) return(R / sqrt(Sigma[1L, 1L]))
  for (j in seq_len(p)) {
    mj   <- -j
    Sinv <- tryCatch(chol2inv(chol(Sigma[mj, mj, drop = FALSE])),
                     error = function(e) MASS::ginv(Sigma[mj, mj, drop = FALSE]))
    beta <- Sigma[j, mj, drop = FALSE] %*% Sinv
    cvar <- as.numeric(Sigma[j, j] - beta %*% Sigma[mj, j, drop = FALSE])
    cvar <- max(cvar, .Machine$double.eps)
    Rm   <- R[, mj, drop = FALSE]
    Rm[!is.finite(Rm)] <- 0                   # absent peers contribute nothing
    Z[, j] <- (R[, j] - as.vector(Rm %*% t(beta))) / sqrt(cvar)
  }
  Z
}
