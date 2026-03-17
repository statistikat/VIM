#' Huber weight function
#'
#' Computes Huber weights for standardized values.
#' Returns 1 for values within the tuning constant and
#' \code{k / abs(u)} for values outside.
#'
#' @param u numeric vector of standardized values
#' @param k tuning constant, Default: 1.345
#' @return numeric vector of weights in \eqn{[0, 1]}
#' @author Matthias Templ
#' @keywords internal
huber_weight <- function(u, k = 1.345) {
  w <- ifelse(abs(u) <= k, 1, k / abs(u))
  w[is.nan(w)] <- 1
  w
}

#' Tukey bisquare weight function
#'
#' Computes Tukey bisquare weights for standardized values.
#' Returns \code{(1 - (u/k)^2)^2} for values within the tuning
#' constant and 0 for values outside.
#'
#' @param u numeric vector of standardized values
#' @param k tuning constant, Default: 4.685
#' @return numeric vector of weights in \eqn{[0, 1]}
#' @author Matthias Templ
#' @keywords internal
tukey_weight <- function(u, k = 4.685) {
  w <- ifelse(abs(u) <= k, (1 - (u / k)^2)^2, 0)
  w[is.nan(w)] <- 1
  w
}

#' Apply a weight function to standardized values
#'
#' Dispatches to \code{\link{huber_weight}} or \code{\link{tukey_weight}}
#' depending on the \code{method} argument.
#'
#' @param u numeric vector of standardized values
#' @param method weight function to use: \code{"huber"} or \code{"tukey"},
#'   Default: \code{"huber"}
#' @param alpha tuning constant. If \code{NULL}, defaults to 1.345 for Huber
#'   and 4.685 for Tukey.
#' @return numeric vector of weights in \eqn{[0, 1]}
#' @author Matthias Templ
#' @keywords internal
.apply_weight_fun <- function(u, method = "huber", alpha = NULL) {
  method <- match.arg(method, c("huber", "tukey"))
  if (method == "huber") {
    k <- if (is.null(alpha)) 1.345 else alpha
    huber_weight(u, k = k)
  } else {
    k <- if (is.null(alpha)) 4.685 else alpha
    tukey_weight(u, k = k)
  }
}

#' Robust scale estimate via MAD
#'
#' Computes the median absolute deviation with a fallback for
#' zero or near-zero MAD (constant columns). In that case the
#' inter-quartile range scaled to match the normal distribution
#' is used. If both are zero, returns 1 so that standardized
#' values remain unchanged.
#'
#' @param x numeric vector (NAs are removed internally)
#' @return positive numeric scalar
#' @author Matthias Templ
#' @keywords internal
.robust_scale <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) return(1)
  s <- stats::mad(x)
  if (s < .Machine$double.eps) {
    # fallback: IQR / 1.349 (normal-consistent)
    s <- stats::IQR(x) / 1.349
  }
  if (s < .Machine$double.eps) {
    s <- 1
  }
  s
}


#' Compute per-cell contamination weights
#'
#' For each continuous column, standardize by median and MAD,
#' then apply a robust weight function (Huber or Tukey bisquare)
#' to obtain a weight in \eqn{[0, 1]} per cell. Categorical
#' (factor, character, logical) columns receive weight 1.
#'
#' @param X a data frame or matrix of dimension \eqn{n \times p}
#' @param method weight function: \code{"huber"} or \code{"tukey"},
#'   Default: \code{"huber"}
#' @param alpha tuning constant. If \code{NULL}, the default for
#'   the chosen method is used (1.345 for Huber, 4.685 for Tukey).
#' @return an \eqn{n \times p} numeric matrix of weights
#' @author Matthias Templ
#' @keywords internal
cellWeights <- function(X, method = "huber", alpha = NULL) {
  method <- match.arg(method, c("huber", "tukey"))
  if (is.data.frame(X)) {
    X_mat <- X
  } else {
    X_mat <- as.data.frame(X)
  }
  n <- nrow(X_mat)
  p <- ncol(X_mat)
  W <- matrix(1, nrow = n, ncol = p)
  colnames(W) <- colnames(X_mat)

  for (j in seq_len(p)) {
    col <- X_mat[[j]]
    # only weight continuous columns
    if (is.numeric(col) || is.integer(col)) {
      med_j <- stats::median(col, na.rm = TRUE)
      s_j <- .robust_scale(col)
      u_j <- (col - med_j) / s_j
      # NAs in the original data get weight 1 (neutral)
      u_j[is.na(u_j)] <- 0
      W[, j] <- .apply_weight_fun(u_j, method = method, alpha = alpha)
    }
    # categorical columns keep weight 1
  }
  W
}


#' Compute cell weights from regression residuals
#'
#' Given a vector of residuals and a robust scale estimate,
#' standardize and apply a robust weight function. This is used
#' inside the IRWLS loop to compute psi-weights from the current
#' fit residuals.
#'
#' @param residuals numeric n-vector of residuals from the current fit
#' @param sigma robust scale estimate (e.g. MAD of residuals). If
#'   zero or very small, all weights are set to 1.
#' @param method weight function: \code{"huber"} or \code{"tukey"},
#'   Default: \code{"huber"}
#' @param alpha tuning constant. If \code{NULL}, the default for
#'   the chosen method is used.
#' @return numeric n-vector of weights in \eqn{[0, 1]}
#' @author Matthias Templ
#' @keywords internal
cellWeightsFromResiduals <- function(residuals, sigma,
                                     method = "huber", alpha = NULL) {
  method <- match.arg(method, c("huber", "tukey"))
  if (is.null(sigma) || length(sigma) == 0L ||
      is.na(sigma) || sigma < .Machine$double.eps) {
    return(rep(1, length(residuals)))
  }
  u <- residuals / sigma
  u[is.na(u)] <- 0
  .apply_weight_fun(u, method = method, alpha = alpha)
}


#' Cell-weighted Iteratively Reweighted Least Squares
#'
#' Performs robust regression where each predictor cell \eqn{(i, k)}
#' carries its own weight, rather than collapsing to a single
#' row-level weight.  This is the key building block for all three
#' cellwise-robust imputation methods (cellM, cellIRMI, cellEM).
#'
#' @section Algorithm:
#' \enumerate{
#'   \item Initialize with OLS on the cell-weighted design matrix.
#'   \item Iterate until convergence:
#'     \enumerate{
#'       \item Compute residuals \eqn{r = y - X \beta}.
#'       \item Estimate robust scale \eqn{\sigma = \mathrm{MAD}(r)}.
#'       \item Compute psi-weights \eqn{w^{\psi}_i} from standardized
#'             residuals \eqn{r_i / \sigma}.
#'       \item Form combined weights: for each cell \eqn{(i, k)},
#'             \eqn{w^{total}_{ik} = w^{cell}_{ik} \cdot w^{\psi}_i
#'             \cdot w^{resp}_i}.
#'       \item Construct weighted design matrix
#'             \eqn{\tilde{X}_{ik} = \sqrt{w^{total}_{ik}} \cdot X_{ik}}.
#'       \item Solve via QR decomposition:
#'             \eqn{\beta = (\tilde{X}^T \tilde{X})^{-1} \tilde{X}^T \tilde{y}}.
#'     }
#'   \item Return coefficients, residuals, combined weights.
#' }
#'
#' @param X \eqn{n \times p} design matrix (predictors, without intercept)
#' @param y numeric n-vector (response)
#' @param w_cell \eqn{n \times p} matrix of cell weights for the
#'   predictors (from \code{\link{cellWeights}}). If \code{NULL}, all
#'   weights are set to 1.
#' @param w_response numeric n-vector of cell weights for the
#'   response variable. If \code{NULL}, all response weights are 1.
#' @param maxiter maximum number of IRWLS iterations, Default: 50
#' @param tol convergence tolerance on the relative change in
#'   coefficients, Default: 1e-6
#' @param method weight function: \code{"huber"} or \code{"tukey"},
#'   Default: \code{"huber"}
#' @param alpha tuning constant. If \code{NULL}, the default for
#'   the chosen method is used.
#' @return A list with components:
#'   \describe{
#'     \item{coefficients}{named numeric vector of regression coefficients
#'       (including intercept)}
#'     \item{fitted}{numeric n-vector of fitted values}
#'     \item{residuals}{numeric n-vector of residuals}
#'     \item{weights}{numeric n-vector of final combined row-level
#'       weights (product of psi-weight and response-weight)}
#'     \item{w_total}{the \eqn{n \times (p+1)} matrix of final total
#'       weights (including the intercept column)}
#'     \item{sigma}{final robust scale estimate}
#'     \item{converged}{logical indicating convergence}
#'     \item{iterations}{number of iterations performed}
#'   }
#' @author Matthias Templ
#' @keywords internal
cellIRWLS <- function(X, y, w_cell = NULL, w_response = NULL,
                      maxiter = 50, tol = 1e-6,
                      method = "huber", alpha = NULL) {
  method <- match.arg(method, c("huber", "tukey"))

  # --- input coercion and dimensions ---
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)

  if (length(y) != n) {
    stop("length(y) must equal nrow(X)")
  }

  # add intercept column
  X_int <- cbind("(Intercept)" = rep(1, n), X)
  p_int <- ncol(X_int)  # p + 1

  # --- default cell weights ---
  if (is.null(w_cell)) {
    w_cell_int <- matrix(1, nrow = n, ncol = p_int)
  } else {
    w_cell <- as.matrix(w_cell)
    if (nrow(w_cell) != n) {
      stop("nrow(w_cell) must equal nrow(X)")
    }
    # intercept column gets weight 1
    if (ncol(w_cell) == p) {
      w_cell_int <- cbind(rep(1, n), w_cell)
    } else if (ncol(w_cell) == p_int) {
      w_cell_int <- w_cell
    } else {
      stop("ncol(w_cell) must equal ncol(X) or ncol(X) + 1")
    }
  }

  if (is.null(w_response)) {
    w_response <- rep(1, n)
  } else {
    w_response <- as.numeric(w_response)
    if (length(w_response) != n) {
      stop("length(w_response) must equal nrow(X)")
    }
  }

  # --- Step 1: initial fit via cell-weighted OLS ---
  beta <- .weighted_qr_solve(X_int, y, w_cell_int, w_response,
                             w_psi = rep(1, n))
  converged <- FALSE
  iter <- 0

  for (it in seq_len(maxiter)) {
    iter <- it
    beta_old <- beta

    # residuals
    r <- as.numeric(y - X_int %*% beta)

    # robust scale
    sigma <- stats::mad(r)
    if (sigma < .Machine$double.eps) {
      sigma <- .robust_scale(r)
    }

    # psi-weights from residuals
    w_psi <- cellWeightsFromResiduals(r, sigma,
                                      method = method, alpha = alpha)

    # solve weighted LS
    beta <- .weighted_qr_solve(X_int, y, w_cell_int, w_response,
                               w_psi = w_psi)

    # check convergence
    denom <- max(abs(beta_old), 1)
    if (max(abs(beta - beta_old)) / denom < tol) {
      converged <- TRUE
      break
    }
  }

  # final quantities
  r <- as.numeric(y - X_int %*% beta)
  sigma <- stats::mad(r)
  if (sigma < .Machine$double.eps) {
    sigma <- .robust_scale(r)
  }
  w_psi <- cellWeightsFromResiduals(r, sigma,
                                    method = method, alpha = alpha)
  # combined row-level weights (for diagnostics)
  w_row <- w_psi * w_response

  # total weight matrix (n x (p+1))
  w_total <- w_cell_int * w_psi * w_response

  names(beta) <- colnames(X_int)

  list(
    coefficients = beta,
    fitted       = as.numeric(X_int %*% beta),
    residuals    = r,
    weights      = w_row,
    w_total      = w_total,
    sigma        = sigma,
    converged    = converged,
    iterations   = iter
  )
}


#' QR-based weighted least squares with cell-level weights
#'
#' Solves the weighted least squares problem where each cell
#' \eqn{(i, k)} in the design matrix has its own weight. The
#' effective weight for cell \eqn{(i, k)} is
#' \eqn{w^{cell}_{ik} \cdot w^{\psi}_i \cdot w^{resp}_i}.
#' Uses QR decomposition for numerical stability.
#'
#' @param X_int \eqn{n \times (p+1)} design matrix with intercept
#' @param y numeric n-vector
#' @param w_cell_int \eqn{n \times (p+1)} cell weight matrix
#' @param w_response numeric n-vector of response weights
#' @param w_psi numeric n-vector of psi-weights from residuals
#' @return numeric (p+1)-vector of regression coefficients
#' @author Matthias Templ
#' @keywords internal
.weighted_qr_solve <- function(X_int, y, w_cell_int, w_response, w_psi) {
  n <- nrow(X_int)
  p_int <- ncol(X_int)

  # combined weight for each cell (i, k)
  # w_total[i, k] = w_cell[i, k] * w_psi[i] * w_response[i]
  w_row <- w_psi * w_response  # n-vector
  w_total <- w_cell_int * w_row  # n x (p+1) matrix

  # weighted design: X_tilde[i, k] = sqrt(w_total[i, k]) * X[i, k]
  sqrt_w_total <- sqrt(pmax(w_total, 0))
  X_tilde <- sqrt_w_total * X_int

  # weighted response: y_tilde[i] = sqrt(w_row[i]) * y[i]
  sqrt_w_row <- sqrt(pmax(w_row, 0))
  y_tilde <- sqrt_w_row * y

  # solve via QR decomposition
  qr_obj <- qr(X_tilde)

  # check for rank deficiency
  if (qr_obj$rank < p_int) {
    # fall back to ridge-regularised normal equations
    XtX <- crossprod(X_tilde) + diag(1e-10, p_int)
    Xty <- crossprod(X_tilde, y_tilde)
    beta <- as.numeric(solve(XtX, Xty))
  } else {
    beta <- as.numeric(qr.coef(qr_obj, y_tilde))
  }

  # handle any NaN from degenerate cases
  beta[is.nan(beta)] <- 0
  beta
}
