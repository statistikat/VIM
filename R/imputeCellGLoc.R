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
#' @param data a \code{data.frame} with continuous and categorical columns.
#' @param design one-sided formula for the categorical mean structure.
#'   \code{~ .} (default) is main effects over all categorical columns,
#'   \code{~ .^2} adds interactions, \code{~ 1} is intercept only.
#' @param weights \code{"soft"} for redescending weights in \[0, 1\],
#'   \code{"binary"} for the penalised cellwise MCD objective.
#' @param maxit maximum number of outer iterations.
#' @param eps convergence tolerance on the relative change in \code{B}.
#' @param alpha minimum fraction of unflagged cells per column (binary corner).
#' @param psi_c tuning constant of the Tukey bisquare (soft corner).
#'   \code{Inf} disables downweighting.
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
                           maxit = 50, eps = 5e-3, alpha = 0.75,
                           psi_c = 4.685, trace = FALSE) {
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

  M <- !is.finite(X)                       # missing mask
  W <- matrix(1, n, p, dimnames = dimnames(X))
  W[M] <- 0
  B <- .gloc_update_B(X, U, W)
  converged <- FALSE

  for (it in seq_len(maxit)) {
    B_old <- B
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
      if (is.null(fit)) { Sigma <- stats::cov(R[stats::complete.cases(R), , drop = FALSE]) }
      else { Sigma <- fit$S; W <- fit$W }
    } else {
      Sigma <- .gloc_scatter_soft(R, W, M)
      Z <- .gloc_cond_resid(R, Sigma)
      W <- .gloc_bisquare(Z, psi_c)
    }
    W[M] <- 0
    B <- .gloc_update_B(X, U, W)

    den <- max(1e-8, max(abs(B_old)))
    if (trace) message(sprintf("  iter %d: rel. change in B = %.3g", it,
                               max(abs(B - B_old)) / den))
    if (max(abs(B - B_old)) / den < eps) { converged <- TRUE; break }
  }

  Ximp <- .gloc_impute(X, U, B, Sigma, M)
  out <- data
  out[, cont_vars] <- as.data.frame(Ximp)

  list(B = B, Sigma = Sigma, W = W, U = U, imputed = out,
       converged = converged, iterations = it)
}

#' Fill missing continuous cells by their conditional expectation
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
  for (i in rows) {
    miss <- which(M[i, ]); obs <- which(!M[i, ])
    if (!length(obs)) { Xi[i, miss] <- Mu[i, miss]; next }
    Soo  <- Sigma[obs, obs, drop = FALSE]
    Sinv <- tryCatch(chol2inv(chol(Soo)), error = function(e) MASS::ginv(Soo))
    Xi[i, miss] <- Mu[i, miss] +
      as.vector(Sigma[miss, obs, drop = FALSE] %*% Sinv %*% R[i, obs])
  }
  Xi
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
#' @param R \eqn{n x p} matrix of residuals from the mean structure.
#' @param W \eqn{n x p} matrix of cell weights in \[0, 1\].
#' @param M \eqn{n x p} logical mask of missing cells.
#' @return a \eqn{p x p} scatter matrix.
#' @keywords internal
.gloc_scatter_soft <- function(R, W, M) {
  Rna <- R; Rna[M] <- NA_real_
  if (requireNamespace("cellWise", quietly = TRUE)) {
    Wc <- W; Wc[!is.finite(Wc)] <- 0
    fit <- tryCatch(cellWise::cwLocScat(Rna, W = Wc, methods = "all"),
                    error = function(e) NULL)
    if (!is.null(fit) && all(is.finite(fit$cwMLEsigma))) return(fit$cwMLEsigma)
  }
  ok <- stats::complete.cases(Rna)
  stats::cov(Rna[ok, , drop = FALSE])
}
