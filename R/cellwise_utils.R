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


#' Internal positional column names for cellwise model fits
#'
#' Model formulas inside the cellwise family are built by pasting column
#' names. Duplicated column names (e.g. after truncation with
#' \code{substr()}) or non-syntactic names then resolve the response or
#' predictors to the wrong column, which silently corrupts the imputation.
#' The df-interface cellwise functions therefore rename the data to these
#' safe positional names on entry and restore the original names on exit.
#'
#' @param p number of columns
#' @return character vector of length \code{p}
#' @keywords internal
#' @noRd
.cw_safe_names <- function(p) {
  sprintf(".cwV%d", seq_len(p))
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


#' Compute per-cell weights using MCD-based conditional residuals
#'
#' For each continuous cell (i,j), computes the conditional expectation
#' \eqn{E(x_{ij} | x_{i,-j})} under a robust Gaussian model (estimated via MCD),
#' standardizes the residual, and applies a weight function.  This captures
#' multivariate outlier structure that univariate standardization misses.
#'
#' @param X a data frame or matrix of dimension n x p (continuous columns only)
#' @param method weight function: \code{"tukey"} or \code{"huber"},
#'   Default: \code{"tukey"}
#' @param alpha tuning constant. If \code{NULL}, defaults to 4.685 for
#'   Tukey and 1.345 for Huber.
#' @return an n x p numeric matrix of weights in \eqn{[0, 1]}
#' @author Matthias Templ
#' @importFrom robustbase covMcd
#' @export
cellWeightsMCD <- function(X, method = "tukey", alpha = NULL) {
  method <- match.arg(method, c("huber", "tukey"))
  X_mat <- as.matrix(X)
  n <- nrow(X_mat)
  p <- ncol(X_mat)
  W <- matrix(1, nrow = n, ncol = p)
  colnames(W) <- colnames(X_mat)

  # exact-constant columns make every MCD subsample singular (covMcd()
  # then warns repeatedly); they carry no outlyingness information, so
  # they keep weight 1 and the MCD runs on the varying columns only
  varying <- vapply(seq_len(p), function(j) {
    v <- X_mat[!is.na(X_mat[, j]), j]
    length(unique(v)) >= 2L
  }, logical(1))
  if (!all(varying)) {
    if (any(varying)) {
      W[, varying] <- cellWeightsMCD(X_mat[, varying, drop = FALSE],
                                     method = method, alpha = alpha)
    }
    return(W)
  }

  if (p < 2 || n < 2 * p) {
    # Not enough data for MCD; fall back to univariate
    for (j in seq_len(p)) {
      med_j <- stats::median(X_mat[, j], na.rm = TRUE)
      s_j <- .robust_scale(X_mat[, j])
      u_j <- (X_mat[, j] - med_j) / s_j
      u_j[is.na(u_j)] <- 0
      W[, j] <- .apply_weight_fun(u_j, method = method, alpha = alpha)
    }
    return(W)
  }

  # Handle NAs: median-impute for MCD estimation
  X_complete <- X_mat
  for (j in seq_len(p)) {
    na_j <- is.na(X_complete[, j])
    if (any(na_j)) X_complete[na_j, j] <- stats::median(X_complete[, j], na.rm = TRUE)
  }

  # Robust covariance via MCD. Heavily tied (discrete-ish) columns make
  # covMcd() warn about singular subsamples while still returning a usable
  # estimate; suppress that noise -- the error path falls back to
  # univariate weights below.
  mcd <- tryCatch(
    suppressWarnings(robustbase::covMcd(X_complete, alpha = 0.75)),
    error = function(e) NULL
  )

  if (is.null(mcd)) {
    # MCD failed; fall back to univariate
    for (j in seq_len(p)) {
      med_j <- stats::median(X_mat[, j], na.rm = TRUE)
      s_j <- .robust_scale(X_mat[, j])
      u_j <- (X_mat[, j] - med_j) / s_j
      u_j[is.na(u_j)] <- 0
      W[, j] <- .apply_weight_fun(u_j, method = method, alpha = alpha)
    }
    return(W)
  }

  mu_rob <- mcd$center
  Sigma_rob <- mcd$cov

  # For each variable j: compute conditional residual
  for (j in seq_len(p)) {
    other <- setdiff(seq_len(p), j)

    # Conditional distribution: x_j | x_{-j} ~ N(mu_cond, sigma2_cond)
    Sigma_jmj <- Sigma_rob[j, other, drop = FALSE]
    Sigma_mjmj <- Sigma_rob[other, other, drop = FALSE]
    Sigma_mjj <- Sigma_rob[other, j, drop = FALSE]

    Sigma_mjmj_inv <- tryCatch(
      solve(Sigma_mjmj + diag(1e-8, length(other))),
      error = function(e) MASS::ginv(Sigma_mjmj)
    )

    beta_cond <- as.numeric(Sigma_mjmj_inv %*% Sigma_mjj)
    sigma2_cond <- max(
      Sigma_rob[j, j] - as.numeric(Sigma_jmj %*% Sigma_mjmj_inv %*% Sigma_mjj),
      .Machine$double.eps * 100
    )
    sigma_cond <- sqrt(sigma2_cond)

    # Conditional expectation for each observation
    dev_other <- sweep(X_complete[, other, drop = FALSE], 2, mu_rob[other], "-")
    mu_cond <- mu_rob[j] + as.numeric(dev_other %*% beta_cond)

    # Standardized conditional residual
    u_j <- (X_complete[, j] - mu_cond) / sigma_cond
    # NAs in the original data get weight 1
    u_j[is.na(X_mat[, j])] <- 0

    W[, j] <- .apply_weight_fun(u_j, method = method, alpha = alpha)
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
#'   \item Initialize coefficients with an S-estimator
#'         (\code{robustbase::lmrob.S}) when \code{init = "s-estimator"}
#'         and \eqn{n > 2p}, otherwise cell-weighted OLS.
#'   \item Iterate until convergence:
#'     \enumerate{
#'       \item Compute residuals \eqn{r = y - X \beta}.
#'       \item Estimate robust scale \eqn{\sigma = \mathrm{MAD}(r)}.
#'       \item Compute psi-weights \eqn{w^{\psi}_i} from standardized
#'             residuals \eqn{r_i / \sigma}.
#'       \item Form row weights \eqn{w^{row}_i = w^{\psi}_i \cdot w^{resp}_i}.
#'       \item Construct weighted design matrix with cell weights entering
#'             linearly:
#'             \eqn{\tilde{X}_{ik} = \sqrt{w^{row}_i} \cdot w^{cell}_{ik} \cdot X_{ik}},
#'             and weighted response
#'             \eqn{\tilde{y}_i = \sqrt{w^{row}_i} \cdot y_i}.
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
#' @param maxit maximum number of IRWLS iterations, Default: 50
#' @param eps convergence tolerance on the relative change in
#'   coefficients, Default: 1e-6
#' @param method weight function: \code{"huber"} or \code{"tukey"},
#'   Default: \code{"tukey"}
#' @param alpha tuning constant. If \code{NULL}, the default for
#'   the chosen method is used.
#' @param init initialisation for the coefficients:
#'   \code{"s-estimator"} (default; \code{robustbase::lmrob.S} with a
#'   cell-weighted-OLS fallback) or \code{"ols"} (cell-weighted OLS).
#' @param damping logical; if \code{TRUE} (default) the cell-weight
#'   update is adaptively damped across iterations for stability.
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
                      maxit = 50, eps = 1e-6,
                      method = "tukey", alpha = NULL,
                      init = "s-estimator", damping = TRUE) {
  method <- match.arg(method, c("huber", "tukey"))
  init <- match.arg(init, c("s-estimator", "ols"))

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

  # --- Step 1: initial fit ---
  if (init == "s-estimator" && n > 2 * p_int &&
      qr(X_int)$rank == p_int &&
      requireNamespace("robustbase", quietly = TRUE)) {
    # Use S-estimator via lmrob.S for high-breakdown initialization
    # Apply to unweighted X to preserve S-estimator's breakdown properties;
    # cell weights are incorporated in the subsequent IRWLS iterations.
    # Rank-deficient designs (e.g. a constant column aliased with the
    # intercept) are excluded above: every elemental subsample would be
    # singular, so lmrob.S warns repeatedly and fails; the cell-weighted
    # OLS fallback below ridge-regularises those instead. The S-init is
    # best-effort, so remaining subsample warnings are suppressed.
    s_fit <- tryCatch({
      suppressWarnings(
        robustbase::lmrob.S(X_int, y, control = robustbase::lmrob.control(
          method = "S", k.max = 200, refine.tol = 1e-7
        ))
      )
    }, error = function(e) NULL)

    if (!is.null(s_fit)) {
      beta <- s_fit$coefficients
    } else {
      # S-estimator failed; fall back to cell-weighted OLS
      beta <- .weighted_qr_solve(X_int, y, w_cell_int, w_response,
                                 w_psi = rep(1, n))
    }
  } else {
    # Cell-weighted OLS initialization
    beta <- .weighted_qr_solve(X_int, y, w_cell_int, w_response,
                               w_psi = rep(1, n))
  }

  converged <- FALSE
  iter <- 0
  w_psi_prev <- rep(1, n)

  for (it in seq_len(maxit)) {
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
    w_psi_new <- cellWeightsFromResiduals(r, sigma,
                                          method = method, alpha = alpha)

    # adaptive damping: lambda increases from 0.3 to 1.0 over iterations
    if (damping && method == "tukey") {
      lambda <- 0.3 + 0.7 * min(it / maxit, 1)
      w_psi <- (1 - lambda) * w_psi_prev + lambda * w_psi_new
    } else {
      w_psi <- w_psi_new
    }
    w_psi_prev <- w_psi

    # solve weighted LS
    beta <- .weighted_qr_solve(X_int, y, w_cell_int, w_response,
                               w_psi = w_psi)

    # check convergence
    denom <- max(abs(beta_old), 1)
    if (max(abs(beta - beta_old)) / denom < eps) {
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


#' QR-based weighted least squares with cell-derived row weights
#'
#' Solves \eqn{\min_\beta \sum_i w_i (y_i - X_i \beta)^2} on the unweighted
#' design, where the row weight \eqn{w_i = w^{cellrow}_i \cdot w^{\psi}_i \cdot
#' w^{resp}_i} and \eqn{w^{cellrow}_i} is the geometric mean of the predictor
#' cell weights of row \eqn{i}. Cell weights thus downweight the influence of
#' rows with contaminated cells without distorting the design values, so the
#' returned \eqn{\beta} is a valid coefficient for \eqn{X \beta}. Uses QR
#' decomposition for numerical stability.
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

  # Cell weights enter the LOSS as per-row reliability, never the design values.
  # A row is downweighted by the average contamination of its predictor cells:
  # the arithmetic mean of the non-intercept cell weights. (A geometric mean lets
  # a single near-zero false-positive flag collapse an otherwise-clean row, which
  # destroys the fit on clean data; the residual psi-weights below still catch
  # gross per-row corruption.) Folding cell weights into the design instead
  # (X_tilde = sqrt(w_row) * w_cell * X) biases beta, because prediction uses the
  # UNWEIGHTED design cbind(1, X) %*% beta.
  if (p_int > 1L) {
    w_cell_row <- rowMeans(w_cell_int[, -1L, drop = FALSE])
  } else {
    w_cell_row <- rep(1, n)
  }

  # Genuine weighted least squares on the unweighted design:
  #   min sum_i w_row_i * (y_i - X_i beta)^2 ,
  #   w_row_i = w_psi_i * w_response_i * w_cell_row_i
  w_row <- w_psi * w_response * w_cell_row
  # Guard total-weight collapse (e.g. a near-exact fit drives the robust scale,
  # and thus all psi-weights, to ~0): fall back to unweighted least squares
  # rather than solving a singular system.
  if (sum(w_row) < 1e-8) w_row <- rep(1, n)
  sqrt_w_row <- sqrt(pmax(w_row, 0))
  X_tilde <- sqrt_w_row * X_int
  y_tilde <- sqrt_w_row * y

  # solve via QR decomposition
  qr_obj <- qr(X_tilde)

  # check for rank deficiency
  if (qr_obj$rank < p_int) {
    # rank-deficient: ridge-regularised normal equations with a scale-aware
    # ridge, backstopped by a pseudo-inverse for numerically singular XtX.
    XtX <- crossprod(X_tilde)
    ridge <- 1e-8 * mean(diag(XtX)) + 1e-12
    Xty <- crossprod(X_tilde, y_tilde)
    beta <- tryCatch(
      as.numeric(solve(XtX + diag(ridge, p_int), Xty)),
      error = function(e) as.numeric(MASS::ginv(XtX + diag(ridge, p_int)) %*% Xty)
    )
  } else {
    beta <- as.numeric(qr.coef(qr_obj, y_tilde))
  }

  # handle any NaN from degenerate cases
  beta[is.nan(beta)] <- 0
  beta
}
