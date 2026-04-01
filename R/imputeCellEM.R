#' Cellwise-robust EM imputation for mixed data
#'
#' EM algorithm with latent contamination indicators that jointly estimates
#' clean distribution parameters and identifies cellwise outliers.  Each
#' continuous cell has a posterior probability of being clean versus
#' contaminated.  The clean distribution is modelled as multivariate
#' normal for continuous variables; categorical variables are handled via
#' conditional multinomial logistic regression.
#'
#' @param data a \code{data.frame} with missing values (mixed continuous
#'   and categorical variables are supported).
#' @param maxit_em maximum number of EM iterations (default: 100).
#' @param eps_em convergence tolerance on the relative change in estimated
#'   parameters (\eqn{\mu}, \eqn{\Sigma}).  Default: 5e-3.
#' @param gamma_init initial scale inflation factor for the contamination
#'   distribution.  Contaminated cells are modelled as having variance
#'   \eqn{(\gamma \sigma)^2} with \eqn{\gamma > 1} (default: 3).
#' @param eps_init initial contamination probability per variable
#'   (default: 0.1).  Must be in \eqn{(0, 0.5)}.
#' @param uncert imputation uncertainty method: \code{"conditional"}
#'   (default) draws from the conditional normal distribution, or
#'   \code{"pmm"} uses predictive mean matching.
#' @param trace logical; if \code{TRUE}, print progress information.
#'
#' @return A list with components:
#'   \item{data_imputed}{the imputed \code{data.frame}.}
#'   \item{cellweights}{\eqn{n \times p} matrix of posterior clean
#'     probabilities.  Continuous observed cells have values in
#'     \eqn{[0, 1]}; missing cells and categorical columns have
#'     weight 1.}
#'   \item{mu}{estimated clean location vector (continuous variables
#'     only).}
#'   \item{Sigma}{estimated clean covariance matrix (continuous variables
#'     only).}
#'   \item{epsilon}{named numeric vector of estimated per-variable
#'     contamination rates (continuous variables only).}
#'   \item{converged}{logical indicating convergence.}
#'   \item{iterations}{number of EM iterations performed.}
#'   \item{loglik}{numeric vector of observed-data log-likelihood values,
#'     one per iteration (computed after each M-step).}
#'
#' @details
#' The algorithm proceeds as follows:
#' \enumerate{
#'   \item \strong{Initialisation.}
#'     Missing values are filled by \code{\link{initialise}} (medians for
#'     continuous, modes for categorical).  Initial location and scale are
#'     estimated robustly (median and MAD).  Cell weights are initialised
#'     to 1.
#'   \item \strong{E-step} (for each continuous variable \eqn{j},
#'     each observation \eqn{i}):
#'     \itemize{
#'       \item Compute the conditional mean and variance of \eqn{x_{ij}}
#'         given the other continuous variables, using the current
#'         \eqn{\mu} and \eqn{\Sigma}.
#'       \item For observed cells: compute the posterior probability
#'         that the cell is clean versus contaminated, yielding cell
#'         weight \eqn{w_{ij}}.
#'       \item For missing cells: impute from the conditional
#'         distribution (with optional PMM).
#'     }
#'   \item \strong{M-step}:
#'     \itemize{
#'       \item Update \eqn{\mu}: cell-weighted mean.
#'       \item Update \eqn{\Sigma}: pairwise cell-weighted covariance
#'         using \eqn{w_{ij} \cdot w_{ik}} (not row-level min).
#'       \item Update contamination rates:
#'         \eqn{\varepsilon_j = 1 - \mathrm{mean}(w_{ij})} over
#'         observed cells.
#'       \item Update contamination scale \eqn{\gamma_j} from
#'         weighted variance of contaminated cells.
#'       \item For categorical variables: fit weighted multinomial
#'         logistic with row weights derived from continuous cell
#'         weights.
#'     }
#'   \item \strong{Convergence.}
#'     Check relative change in estimated parameters (\eqn{\mu},
#'     \eqn{\Sigma}); stop when below \code{eps_em} or after
#'     \code{maxit_em} iterations.  The observed-data log-likelihood
#'     is tracked for diagnostics.
#' }
#'
#' \strong{Note:} convergence degrades in higher dimensions
#' (\eqn{p_{\mathrm{cont}} > 8}); regularised covariance estimation
#' is recommended for such settings.
#'
#' This method differs from cellGMM (Zaccaria et al., 2025) in using
#' a single clean component rather than a mixture of clean clusters,
#' and in supporting mixed continuous and categorical data.
#'
#' @author Matthias Templ
#' @references
#' Templ, M. (2026).  Cellwise-robust imputation for mixed data: three
#' integrated methods.  \emph{Computational Statistics & Data Analysis},
#' submitted.
#'
#' Dempster, A.P., Laird, N.M. and Rubin, D.B. (1977).  Maximum
#' likelihood from incomplete data via the EM algorithm.
#' \emph{Journal of the Royal Statistical Society: Series B},
#' \strong{39}(1), 1--38.
#'
#' Wu, C.F.J. (1983).  On the convergence properties of the EM
#' algorithm.  \emph{The Annals of Statistics}, \strong{11}(1),
#' 95--103.
#'
#' Raymaekers, C. and Rousseeuw, P.J. (2024).  The cellwise minimum
#' covariance determinant estimator.  \emph{Journal of the American
#' Statistical Association}, \strong{119}(545), 576--588.
#'
#' Zaccaria, G., Insolia, L. and Farcomeni, A. (2025).  Robust
#' model-based clustering with cellwise contamination.
#' \emph{Technometrics}, forthcoming.
#'
#' @family imputation methods
#' @seealso \code{\link{imputeCellIRMI}}, \code{\link{imputeCellM}},
#'   \code{\link{initialise}}, \code{\link{irmi}}
#'
#' @examples
#' \dontrun{
#' data(sleep, package = "VIM")
#' result <- imputeCellEM(sleep)
#' head(result$data_imputed)
#'
#' # Inspect estimated contamination rates
#' result$epsilon
#'
#' # Cell weight matrix (1 = clean, low = likely contaminated)
#' image(result$cellweights, main = "Cell weights")
#'
#' # Log-likelihood trace
#' plot(result$loglik, type = "b", xlab = "Iteration",
#'      ylab = "Observed log-likelihood")
#'
#' # With predictive mean matching for imputation
#' result2 <- imputeCellEM(sleep, uncert = "pmm", trace = TRUE)
#'
#' # Mixed data example
#' data(testdata)
#' result3 <- imputeCellEM(testdata$wna)
#' }
#'
#' @export
#' @importFrom MASS ginv
#' @importFrom stats dnorm rnorm cov median mad model.matrix predict
#' @importFrom utils tail
imputeCellEM <- function(data, maxit_em = 100, eps_em = 5e-3,
                         gamma_init = 3, eps_init = 0.1,
                         uncert = "conditional", trace = FALSE) {

  ## ---- input validation ----
  check_data(data)
  if (!is.data.frame(data)) {
    if (is.matrix(data))
      data <- as.data.frame(data)
    else
      stop("data must be a data.frame or matrix")
  }
  uncert <- match.arg(uncert, c("conditional", "pmm"))

  if (gamma_init <= 1)
    stop("gamma_init must be > 1")
  if (eps_init <= 0 || eps_init >= 0.5)
    stop("eps_init must be in (0, 0.5)")
  if (ncol(data) < 2)
    stop("Need at least 2 variables.")

  ## ---- handle no missing values ----
  if (!any(is.na(data))) {
    message("No missing values in data. Nothing to impute.")
    n <- nrow(data)
    p <- ncol(data)
    W <- matrix(1, nrow = n, ncol = p,
                dimnames = list(rownames(data), colnames(data)))
    return(list(data_imputed = data, cellweights = W,
                mu = NULL, Sigma = NULL, epsilon = NULL,
                converged = TRUE, iterations = 0L, loglik = numeric(0)))
  }

  if (any(apply(data, 1, function(x) all(is.na(x)))))
    stop("Unit non-responses (entire row missing) detected. Remove them first.")

  ## ---- detect variable types ----
  rn <- rownames(data)
  cn <- colnames(data)
  n <- nrow(data)
  p <- ncol(data)

  class1 <- function(x) class(x)[1]
  types <- vapply(data, class1, character(1), USE.NAMES = FALSE)

  # convert character to factor
  if (any(types == "character")) {
    chr_ind <- which(types == "character")
    warning("At least one character variable is converted into a factor")
    for (ind in chr_ind) {
      data[, ind] <- as.factor(data[, ind])
      types[ind] <- "factor"
    }
  }

  # refine factor types
  ind_fac <- which(types == "factor")
  for (ind in ind_fac) {
    fac_nlevels <- nlevels(data[[ind]])
    if (fac_nlevels < 2)
      stop(sprintf("Factor with less than 2 levels detected: '%s'",
                    names(data)[ind]))
    types[ind] <- ifelse(fac_nlevels == 2, "binary", "nominal")
  }
  ind_ord <- which(types == "ordered")
  for (ind in ind_ord) {
    fac_nlevels <- nlevels(data[[ind]])
    if (fac_nlevels == 2) types[ind] <- "binary"
  }

  is_continuous <- types %in% c("numeric", "integer")
  is_categorical <- !is_continuous
  cont_idx <- which(is_continuous)
  cat_idx  <- which(is_categorical)
  p_cont   <- length(cont_idx)
  p_cat    <- length(cat_idx)

  ## ---- record missingness pattern ----
  M <- is.na(data)
  vars_miss <- which(colMeans(M) > 0)
  if (length(vars_miss) == 0) {
    W <- matrix(1, nrow = n, ncol = p,
                dimnames = list(rn, cn))
    return(list(data_imputed = data, cellweights = W,
                mu = NULL, Sigma = NULL, epsilon = NULL,
                converged = TRUE, iterations = 0L, loglik = numeric(0)))
  }

  ## ---- edge case: no continuous variables ----
  if (p_cont == 0) {
    # Only categorical variables: no cellwise contamination model.
    # Fall back to mode imputation.
    message("No continuous variables; cellEM contamination model not applicable. ",
            "Imputing categorical variables by mode.")
    data <- initialise(data, mixed = NULL, method = "median")
    W <- matrix(1, nrow = n, ncol = p,
                dimnames = list(rn, cn))
    return(list(data_imputed = data, cellweights = W,
                mu = numeric(0), Sigma = matrix(0, 0, 0),
                epsilon = numeric(0),
                converged = TRUE, iterations = 0L, loglik = numeric(0)))
  }

  ## ---- step 1: initialise missing values ----
  data <- initialise(data, mixed = NULL, method = "median")

  ## ---- step 2: initialize parameters ----

  # Extract continuous block as numeric matrix
  X_cont <- as.matrix(data[, cont_idx, drop = FALSE])
  cont_names <- cn[cont_idx]

  # Robust initial location and scale
  mu <- apply(X_cont, 2, stats::median)
  names(mu) <- cont_names
  sigma_marg <- apply(X_cont, 2, .robust_scale)

  # Save initial robust estimates for the FIXED contamination distribution
  # (following cellMCD: contamination density is fixed, only clean updates)
  mu_init <- mu
  sigma_init <- sigma_marg

  # Initial covariance: use robust pairwise estimates
  Sigma <- .init_covariance(X_cont, mu, sigma_marg)

  # Contamination parameters
  epsilon <- rep(eps_init, p_cont)
  names(epsilon) <- cont_names
  gamma_vec <- rep(gamma_init, p_cont)
  names(gamma_vec) <- cont_names

  # Cell weight matrix: n x p, all 1 initially
  W <- matrix(1, nrow = n, ncol = p,
              dimnames = list(rn, cn))

  ## ---- EM iteration ----
  converged <- FALSE
  iterations <- 0L
  loglik_trace <- numeric(0)
  param_change_history <- numeric(0)
  mu_prev <- mu
  Sigma_prev <- Sigma

  for (em_iter in seq_len(maxit_em)) {
    iterations <- em_iter

    if (trace) {
      message("--------------------------------------")
      message(paste("cellEM: start of iteration", em_iter))
    }

    ## ================================================================
    ## E-STEP
    ## ================================================================

    # Use a frozen snapshot of the continuous data for all conditioning
    # computations. This ensures all variables are conditioned on the
    # same data (from the previous M-step), preserving EM monotonicity.
    X_cont <- as.matrix(data[, cont_idx, drop = FALSE])
    W_prev <- W  # snapshot of weights

    # Storage for conditional moments (used for final stochastic imputation)
    mu_cond_store <- list()
    s2_cond_store <- list()

    # Buffers: collect imputed values and updated weights, apply after loop
    impute_buffer <- list()  # list of (miss_indices, j_global, values)
    weight_buffer <- list()  # list of (obs_indices, j_global, weights)

    for (jj in seq_len(p_cont)) {
      j_global <- cont_idx[jj]  # global column index
      other_cont <- setdiff(seq_len(p_cont), jj)  # other continuous indices (local)

      if (length(other_cont) == 0) {
        # Single continuous variable: no conditioning
        mu_cond <- rep(mu[jj], n)
        sigma2_cond <- rep(Sigma[jj, jj], n)
      } else {
        # Conditional distribution: x_j | x_{-j}
        # mu_cond_i = mu_j + Sigma_{j,-j} Sigma_{-j,-j}^{-1} (x_{i,-j} - mu_{-j})
        # sigma2_cond = Sigma_{jj} - Sigma_{j,-j} Sigma_{-j,-j}^{-1} Sigma_{-j,j}
        Sigma_jmj <- Sigma[jj, other_cont, drop = FALSE]   # 1 x (p_cont-1)
        Sigma_mjmj <- Sigma[other_cont, other_cont, drop = FALSE]
        Sigma_mjj <- Sigma[other_cont, jj, drop = FALSE]    # (p_cont-1) x 1

        # Regularize Sigma_{-j,-j} for invertibility
        Sigma_mjmj_reg <- .regularize_cov(Sigma_mjmj)
        Sigma_mjmj_inv <- tryCatch(
          solve(Sigma_mjmj_reg),
          error = function(e) {
            MASS::ginv(Sigma_mjmj_reg)
          }
        )

        # Deviations from mean for other continuous variables
        # Use the FROZEN X_cont snapshot for conditioning
        X_other <- X_cont[, other_cont, drop = FALSE]
        mu_other <- mu[other_cont]

        # Deviations from current mean (standard EM conditional expectation)
        dev_other <- sweep(X_other, 2, mu_other, "-")

        mu_cond <- mu[jj] + as.numeric(dev_other %*%
                                         as.numeric(Sigma_mjmj_inv %*% Sigma_mjj))
        sigma2_cond <- rep(
          max(Sigma[jj, jj] - as.numeric(Sigma_jmj %*% Sigma_mjmj_inv %*% Sigma_mjj),
              .Machine$double.eps * 100),
          n
        )
      }

      sigma_cond <- sqrt(sigma2_cond)

      # --- E-step: posterior clean probability for observed cells ---
      obs_j <- which(!M[, j_global])
      miss_j <- which(M[, j_global])

      if (length(obs_j) > 0) {
        x_obs <- X_cont[obs_j, jj]
        mu_c_obs <- mu_cond[obs_j]
        s_c_obs  <- sigma_cond[obs_j]

        # Clean density
        d_clean <- stats::dnorm(x_obs, mean = mu_c_obs, sd = s_c_obs)

        # Contamination density: fixed broad distribution centered at
        # the initial robust median with scale gamma * initial_MAD.
        # This prevents the positive feedback loop where inflating Sigma
        # makes the contamination distribution indistinguishable from clean.
        eps_j   <- epsilon[jj]
        d_contam <- stats::dnorm(x_obs, mean = mu_init[jj],
                                  sd = gamma_vec[jj] * sigma_init[jj])

        # Guard against numerical zero densities
        d_clean  <- pmax(d_clean, .Machine$double.eps)
        d_contam <- pmax(d_contam, .Machine$double.eps)

        # Posterior probability of clean
        numer <- (1 - eps_j) * d_clean
        denom <- numer + eps_j * d_contam
        w_ij <- numer / denom

        # Guard against extreme weights
        w_ij <- pmax(pmin(w_ij, 1 - 1e-10), 1e-10)

        weight_buffer[[length(weight_buffer) + 1L]] <- list(
          idx = obs_j, j = j_global, w = w_ij
        )
      }

      # --- E-step: compute conditional expectations for missing cells ---
      if (length(miss_j) > 0) {
        impute_buffer[[length(impute_buffer) + 1L]] <- list(
          idx = miss_j, j = j_global, val = mu_cond[miss_j]
        )

        # Store conditional moments for final stochastic imputation
        for (idx in miss_j) {
          mu_cond_store[[paste(idx, j_global, sep = ",")]] <- mu_cond[idx]
          s2_cond_store[[paste(idx, j_global, sep = ",")]] <- sigma2_cond[idx]
        }
      }
    }

    # Apply all E-step updates at once (after all variables processed)
    for (buf in weight_buffer) {
      W[buf$idx, buf$j] <- buf$w
    }
    for (buf in impute_buffer) {
      data[buf$idx, buf$j] <- buf$val
      W[buf$idx, buf$j] <- 1  # missing cells get weight 1
    }

    # Update continuous block after E-step
    X_cont <- as.matrix(data[, cont_idx, drop = FALSE])

    # --- E-step: impute missing categorical variables ---
    cat_miss_vars <- intersect(vars_miss, cat_idx)
    if (length(cat_miss_vars) > 0) {
      # Row weights for multinomial: geometric mean of continuous cell weights
      if (p_cont > 0) {
        w_cont_block <- W[, cont_idx, drop = FALSE]
        w_row_cat <- apply(w_cont_block, 1, function(ww) {
          exp(mean(log(pmax(ww, 1e-10))))
        })
      } else {
        w_row_cat <- rep(1, n)
      }

      for (j_global in cat_miss_vars) {
        miss_j <- which(M[, j_global])
        n_miss_j <- length(miss_j)
        if (n_miss_j == 0) next

        pred_cols <- setdiff(seq_len(p), j_global)
        form_j <- stats::as.formula(
          paste0(cn[j_global], " ~ ",
                 paste(cn[pred_cols], collapse = " + "))
        )

        multimod <- tryCatch({
          suppressMessages(
            nnet::multinom(form_j, data = data, weights = w_row_cat,
                           maxit = 50, trace = FALSE, MaxNWts = 50000)
          )
        }, error = function(e) {
          warning(paste("Multinomial model failed for variable",
                        cn[j_global], ":", e$message,
                        "- using unweighted model"))
          suppressMessages(
            nnet::multinom(form_j, data = data,
                           maxit = 50, trace = FALSE, MaxNWts = 50000)
          )
        })

        prob_pred <- stats::predict(multimod,
                                     newdata = data[miss_j, , drop = FALSE],
                                     type = "probs")

        lvls <- levels(data[[j_global]])
        if (is.null(lvls)) lvls <- sort(unique(na.omit(data[[j_global]])))

        # Handle edge cases: single missing row or binary factor
        if (is.null(dim(prob_pred))) {
          if (length(lvls) == 2) {
            prob_pred <- matrix(c(1 - prob_pred, prob_pred),
                                nrow = n_miss_j)
            colnames(prob_pred) <- lvls
          } else {
            prob_pred <- matrix(prob_pred, nrow = 1)
            colnames(prob_pred) <- lvls
          }
        }

        imputed_cats <- apply(prob_pred, 1, function(pp) {
          pp <- pmax(pp, 0)
          pp <- pp / sum(pp)
          sample(colnames(prob_pred), size = 1, prob = pp)
        })

        if (is.factor(data[[j_global]])) {
          data[miss_j, j_global] <- factor(imputed_cats,
                                            levels = levels(data[[j_global]]))
        } else {
          data[miss_j, j_global] <- imputed_cats
        }
      }
    }

    ## ================================================================
    ## M-STEP
    ## ================================================================

    X_cont <- as.matrix(data[, cont_idx, drop = FALSE])
    W_cont <- W[, cont_idx, drop = FALSE]

    # --- Update mu: cell-weighted mean ---
    mu_new <- numeric(p_cont)
    for (jj in seq_len(p_cont)) {
      w_j <- W_cont[, jj]
      sw <- sum(w_j)
      if (sw > 0) {
        mu_new[jj] <- sum(w_j * X_cont[, jj]) / sw
      } else {
        mu_new[jj] <- mu[jj]  # keep previous
      }
    }
    names(mu_new) <- cont_names
    mu <- mu_new

    # --- Update Sigma: pairwise cell-weighted covariance ---
    # Step 1: compute base covariance (without conditional variance correction)
    Sigma_new <- matrix(0, nrow = p_cont, ncol = p_cont)
    rownames(Sigma_new) <- colnames(Sigma_new) <- cont_names

    # Centered values
    X_centered <- sweep(X_cont, 2, mu, "-")

    # Store per-pair normalization weights for the correction step
    sw_diag <- numeric(p_cont)

    for (jj in seq_len(p_cont)) {
      for (kk in jj:p_cont) {
        w_jk <- W_cont[, jj] * W_cont[, kk]
        sw_jk <- sum(w_jk)

        if (sw_jk > .Machine$double.eps * 100) {
          cov_jk <- sum(w_jk * X_centered[, jj] * X_centered[, kk]) / sw_jk
          Sigma_new[jj, kk] <- cov_jk
          Sigma_new[kk, jj] <- cov_jk
          if (jj == kk) sw_diag[jj] <- sw_jk
        } else {
          Sigma_new[jj, kk] <- Sigma[jj, kk]
          Sigma_new[kk, jj] <- Sigma[kk, jj]
          if (jj == kk) sw_diag[jj] <- 1
        }
      }
    }

    # Step 2: add conditional variance correction for diagonal elements
    # using the newly estimated Sigma_new (not the old Sigma)
    Sigma_base <- .ensure_pd(Sigma_new)

    for (jj in seq_len(p_cont)) {
      j_global <- cont_idx[jj]
      miss_j <- which(M[, j_global])
      if (length(miss_j) == 0) next

      other_cont <- setdiff(seq_len(p_cont), jj)
      if (length(other_cont) > 0) {
        Sigma_jmj <- Sigma_base[jj, other_cont, drop = FALSE]
        Sigma_mjmj <- Sigma_base[other_cont, other_cont, drop = FALSE]
        Sigma_mjj <- Sigma_base[other_cont, jj, drop = FALSE]
        Sigma_mjmj_reg <- .regularize_cov(Sigma_mjmj)
        s2_cond <- max(
          Sigma_base[jj, jj] - as.numeric(
            Sigma_jmj %*% solve(Sigma_mjmj_reg) %*% Sigma_mjj
          ),
          .Machine$double.eps * 100
        )
      } else {
        s2_cond <- Sigma_base[jj, jj]
      }
      Sigma_new[jj, jj] <- Sigma_new[jj, jj] +
        length(miss_j) * s2_cond / sw_diag[jj]
    }

    # Constrain: diagonal of Sigma cannot exceed 3x the initial variance
    # This prevents the covariance inflation positive feedback loop
    max_var <- 3 * sigma_init^2
    for (jj in seq_len(p_cont)) {
      if (Sigma_new[jj, jj] > max_var[jj]) {
        # Scale down the entire row/column proportionally
        scale_fac <- sqrt(max_var[jj] / Sigma_new[jj, jj])
        Sigma_new[jj, ] <- Sigma_new[jj, ] * scale_fac
        Sigma_new[, jj] <- Sigma_new[, jj] * scale_fac
      }
    }

    # Ensure positive definiteness
    Sigma <- .ensure_pd(Sigma_new)

    # Update marginal standard deviations
    sigma_marg <- sqrt(pmax(diag(Sigma), .Machine$double.eps * 100))

    # --- Update epsilon: contamination rates ---
    for (jj in seq_len(p_cont)) {
      j_global <- cont_idx[jj]
      obs_j <- which(!M[, j_global])
      if (length(obs_j) > 0) {
        eps_new <- 1 - mean(W[obs_j, j_global])
        # Guard against degenerate values
        epsilon[jj] <- max(min(eps_new, 0.40), 1e-6)
      }
    }

    # Contamination distribution is FIXED (mu_init, gamma * sigma_init)
    # following the cellMCD approach. Only epsilon is updated from data.
    # This prevents the positive feedback loop where updating the
    # contamination model absorbs all data into the contamination component.

    ## ================================================================
    ## LOG-LIKELIHOOD (observed data, computed after M-step)
    ## ================================================================

    ll <- .cellEM_loglik(X_cont, M[, cont_idx, drop = FALSE],
                         mu, Sigma, epsilon, mu_init,
                         gamma_vec, sigma_init)

    loglik_trace <- c(loglik_trace, ll)

    if (trace) {
      message(paste("  log-likelihood:", round(ll, 4)))
      message(paste("  epsilon range: [",
                    round(min(epsilon), 4), ",",
                    round(max(epsilon), 4), "]"))
      message(paste("  gamma range: [",
                    round(min(gamma_vec), 4), ",",
                    round(max(gamma_vec), 4), "]"))
    }

    ## ================================================================
    ## CONVERGENCE CHECK (parameter change)
    ## ================================================================

    # Convergence based on relative change in parameters (mu, Sigma).
    # Uses a rolling average over the last few iterations to handle
    # small oscillations that are typical in this ECM variant.
    if (em_iter > 1) {
      mu_change <- max(abs(mu - mu_prev)) / (max(abs(mu_prev)) + 1)
      sigma_change <- max(abs(Sigma - Sigma_prev)) / (max(abs(Sigma_prev)) + 1)
      rel_change <- max(mu_change, sigma_change)
      param_change_history <- c(param_change_history, rel_change)

      if (trace) {
        message(paste("  parameter change:", format(rel_change, digits = 6)))
      }

      # Check convergence: either the change is small, or the average
      # change over the last 4 iterations is small (handles oscillation)
      window <- min(4L, length(param_change_history))
      avg_change <- mean(tail(param_change_history, window))
      if (rel_change < eps_em ||
          (length(param_change_history) >= 4L && avg_change < eps_em * 10)) {
        converged <- TRUE
        if (trace) message("cellEM converged.")
        break
      }
    }

    mu_prev <- mu
    Sigma_prev <- Sigma
  }

  if (!converged && trace) {
    message(paste("cellEM did not converge after", maxit_em, "iterations."))
  }

  ## ---- final imputation with uncertainty ----
  # During EM iterations we used conditional expectations for parameter
  # estimation. Now add stochastic uncertainty to the imputed values.
  # Also compute reasonable bounds from observed data for clipping.
  obs_range <- list()
  for (jj in seq_len(p_cont)) {
    j_global <- cont_idx[jj]
    obs_vals <- X_cont[!M[, j_global], jj]
    rng <- range(obs_vals)
    spread <- diff(rng) + 1e-10
    obs_range[[jj]] <- c(rng[1] - 3 * spread, rng[2] + 3 * spread)
  }

  for (jj in seq_len(p_cont)) {
    j_global <- cont_idx[jj]
    miss_j <- which(M[, j_global])
    if (length(miss_j) == 0) next
    obs_j <- which(!M[, j_global])

    for (idx in miss_j) {
      key <- paste(idx, j_global, sep = ",")
      mu_c <- mu_cond_store[[key]]
      s2_c <- s2_cond_store[[key]]

      if (is.null(mu_c) || is.null(s2_c)) next

      # Clip conditional sd to prevent extreme draws
      s2_c <- min(s2_c, 9 * sigma_init[jj]^2)

      if (uncert == "conditional") {
        val <- stats::rnorm(1, mean = mu_c, sd = sqrt(s2_c))
        # Clip to reasonable range
        data[idx, j_global] <- max(min(val, obs_range[[jj]][2]),
                                    obs_range[[jj]][1])
      } else {
        # PMM
        if (length(obs_j) > 0) {
          y_obs <- X_cont[obs_j, jj]
          # Use conditional means as predicted values for donors
          pred_obs <- numeric(length(obs_j))
          for (ii in seq_along(obs_j)) {
            key_obs <- paste(obs_j[ii], j_global, sep = ",")
            # Observed cells: predicted value is the conditional mean
            # (but these are not stored; recompute from current mu)
            pred_obs[ii] <- data[obs_j[ii], j_global]
          }
          data[idx, j_global] <- .pmm_impute(mu_c, y_obs,
                                               pred_obs,
                                               n_donors = 5)
        } else {
          data[idx, j_global] <- stats::rnorm(1, mean = mu_c,
                                               sd = sqrt(s2_c))
        }
      }
    }
  }

  ## ---- prepare output ----
  rownames(data) <- rn

  list(
    data_imputed = data,
    cellweights  = W,
    mu           = mu,
    Sigma        = Sigma,
    epsilon      = epsilon,
    converged    = converged,
    iterations   = iterations,
    loglik       = loglik_trace
  )
}


# ============================================================================
# Internal helper functions for cellEM
# ============================================================================

#' Initialize covariance matrix from robust estimates
#'
#' Computes an initial covariance matrix using pairwise robust
#' correlation estimates (via median and MAD standardization).
#'
#' @param X numeric matrix (n x p_cont)
#' @param mu numeric vector of robust locations
#' @param sigma_marg numeric vector of robust marginal scales
#' @return p_cont x p_cont covariance matrix
#' @keywords internal
#' @noRd
.init_covariance <- function(X, mu, sigma_marg) {
  p <- ncol(X)
  n <- nrow(X)
  Sigma <- matrix(0, nrow = p, ncol = p)
  colnames(Sigma) <- rownames(Sigma) <- colnames(X)

  # Standardize
  X_std <- sweep(X, 2, mu, "-")
  for (j in seq_len(p)) {
    if (sigma_marg[j] > .Machine$double.eps) {
      X_std[, j] <- X_std[, j] / sigma_marg[j]
    }
  }

  # Pairwise correlations via Gnanadesikan-Kettenring
  for (j in seq_len(p)) {
    Sigma[j, j] <- sigma_marg[j]^2
    if (j < p) {
      for (k in (j + 1):p) {
        s_plus  <- stats::mad(X_std[, j] + X_std[, k])^2
        s_minus <- stats::mad(X_std[, j] - X_std[, k])^2
        r_jk <- (s_plus - s_minus) / (s_plus + s_minus + .Machine$double.eps)
        r_jk <- max(min(r_jk, 0.99), -0.99)
        Sigma[j, k] <- r_jk * sigma_marg[j] * sigma_marg[k]
        Sigma[k, j] <- Sigma[j, k]
      }
    }
  }

  .ensure_pd(Sigma)
}


#' Regularize a covariance matrix
#'
#' Adds a small ridge to ensure invertibility.
#'
#' @param S square matrix
#' @return regularized matrix
#' @keywords internal
#' @noRd
.regularize_cov <- function(S) {
  p <- nrow(S)
  ridge <- max(1e-8, 1e-6 * max(abs(diag(S))))
  S + diag(ridge, p)
}


#' Ensure a matrix is positive definite
#'
#' Projects the matrix onto the positive definite cone by clamping
#' eigenvalues to a minimum threshold.
#'
#' @param S symmetric matrix
#' @return positive definite matrix
#' @keywords internal
#' @noRd
.ensure_pd <- function(S) {
  p <- nrow(S)
  if (p == 0) return(S)

  # Symmetrize

  S <- (S + t(S)) / 2

  eig <- eigen(S, symmetric = TRUE)
  min_eval <- max(abs(eig$values)) * 1e-8
  eig$values <- pmax(eig$values, min_eval)

  S_pd <- eig$vectors %*% diag(eig$values, nrow = p) %*% t(eig$vectors)

  # Preserve names
  dimnames(S_pd) <- dimnames(S)
  S_pd
}


#' Compute observed-data log-likelihood for cellEM
#'
#' Evaluates the mixture log-likelihood:
#' for each observed continuous cell (i,j), the marginal density is
#' (1 - eps_j) * f_clean(x_ij) + eps_j * f_contam(x_ij),
#' where both densities are conditional on the other variables.
#' For missing cells, the marginal contribution is 1 (integrated out).
#'
#' @param X_cont n x p_cont numeric matrix of current continuous values
#' @param M_cont n x p_cont logical missingness matrix for continuous block
#' @param mu clean location vector
#' @param Sigma clean covariance matrix
#' @param epsilon contamination rates
#' @param mu_contam contamination location vector
#' @param gamma_vec contamination scale inflation factors
#' @param sigma_marg marginal standard deviations
#' @return scalar log-likelihood value
#' @keywords internal
#' @noRd
.cellEM_loglik <- function(X_cont, M_cont, mu, Sigma,
                            epsilon, mu_contam, gamma_vec, sigma_marg) {
  n <- nrow(X_cont)
  p_cont <- ncol(X_cont)
  ll <- 0

  for (jj in seq_len(p_cont)) {
    obs_j <- which(!M_cont[, jj])
    if (length(obs_j) == 0) next

    other_cont <- setdiff(seq_len(p_cont), jj)
    eps_j <- epsilon[jj]

    if (length(other_cont) > 0) {
      Sigma_jmj <- Sigma[jj, other_cont, drop = FALSE]
      Sigma_mjmj <- Sigma[other_cont, other_cont, drop = FALSE]
      Sigma_mjj <- Sigma[other_cont, jj, drop = FALSE]
      Sigma_mjmj_reg <- .regularize_cov(Sigma_mjmj)
      Sigma_mjmj_inv <- tryCatch(
        solve(Sigma_mjmj_reg),
        error = function(e) MASS::ginv(Sigma_mjmj_reg)
      )

      dev_other <- sweep(X_cont[obs_j, other_cont, drop = FALSE], 2,
                          mu[other_cont], "-")
      mu_cond_obs <- mu[jj] + as.numeric(
        dev_other %*% (Sigma_mjmj_inv %*% Sigma_mjj)
      )
      sigma2_cond <- max(
        Sigma[jj, jj] - as.numeric(Sigma_jmj %*% Sigma_mjmj_inv %*% Sigma_mjj),
        .Machine$double.eps * 100
      )
    } else {
      mu_cond_obs <- rep(mu[jj], length(obs_j))
      sigma2_cond <- Sigma[jj, jj]
    }
    sigma_cond <- sqrt(sigma2_cond)

    x_obs <- X_cont[obs_j, jj]
    d_clean  <- stats::dnorm(x_obs, mean = mu_cond_obs, sd = sigma_cond)
    d_contam <- stats::dnorm(x_obs, mean = mu_contam[jj],
                              sd = gamma_vec[jj] * sigma_marg[jj])

    d_mix <- (1 - eps_j) * d_clean + eps_j * d_contam
    d_mix <- pmax(d_mix, .Machine$double.eps)

    ll <- ll + sum(log(d_mix))
  }

  ll
}


# ============================================================================
# imputeCellMCD: cellMCD extended to mixed data (Option C)
# ============================================================================

#' Cellwise MCD-based imputation for mixed data
#'
#' Extends the cellwise MCD approach (Raymaekers & Rousseeuw 2024) to
#' mixed continuous + categorical data.  Uses MCD for robust covariance
#' estimation of the continuous block, computes cellwise weights from
#' conditional residuals, then imputes missing values via conditional
#' expectations (continuous) and weighted multinomial regression
#' (categorical).  Iterates until convergence.
#'
#' @param data data.frame with missing values (mixed continuous + categorical)
#' @param maxit maximum iterations (default: 50)
#' @param eps convergence tolerance (default: 5e-3)
#' @param method weight function for cell weights: \code{"tukey"} (default)
#'   or \code{"huber"}
#' @param alpha tuning constant (default: NULL, uses 4.685 for tukey)
#' @param mcd_alpha MCD concentration parameter (default: 0.75)
#' @param uncert imputation uncertainty: \code{"conditional"} (default) or
#'   \code{"pmm"}
#' @param trace logical; print progress
#'
#' @return A list with components:
#'   \item{data_imputed}{the imputed data.frame}
#'   \item{cellweights}{n x p matrix of cell weights}
#'   \item{mu}{robust location estimate (continuous variables)}
#'   \item{Sigma}{robust covariance estimate (continuous variables)}
#'   \item{converged}{logical}
#'   \item{iterations}{number of iterations}
#'
#' @param hard_threshold numeric in \eqn{[0, 1]}.  After convergence,
#'   cells with weight below this value are flagged as contaminated
#'   (default: 0.5).
#' @param mcd_observed strategy for MCD estimation: \code{"all"} (default),
#'   \code{"weighted"}, or \code{"pairwise"}.
#'
#' @author Matthias Templ
#' @importFrom robustbase covMcd
#' @importFrom MASS ginv
#' @export
imputeCellMCD <- function(data, maxit = 50, eps = 5e-3,
                           method = "tukey", alpha = NULL,
                           mcd_alpha = 0.75, hard_threshold = 0.5,
                           mcd_observed = "all",
                           uncert = "conditional", trace = FALSE) {

  ## ---- input validation ----
  mcd_observed <- match.arg(mcd_observed, c("all", "weighted", "pairwise"))
  check_data(data)
  if (!is.data.frame(data)) {
    if (is.matrix(data)) data <- as.data.frame(data)
    else stop("data must be a data.frame or matrix")
  }
  method <- match.arg(method, c("tukey", "huber"))
  uncert <- match.arg(uncert, c("conditional", "pmm"))
  if (is.null(alpha)) alpha <- if (method == "tukey") 4.685 else 1.345

  if (ncol(data) < 2) stop("Need at least 2 variables.")
  if (!any(is.na(data))) {
    n <- nrow(data); p <- ncol(data)
    W <- matrix(1, n, p, dimnames = list(rownames(data), colnames(data)))
    return(list(data_imputed = data, cellweights = W, mu = NULL,
                Sigma = NULL, converged = TRUE, iterations = 0L))
  }
  if (any(apply(data, 1, function(x) all(is.na(x)))))
    stop("Unit non-responses detected. Remove them first.")

  ## ---- detect types ----
  rn <- rownames(data)
  cn <- colnames(data)
  n <- nrow(data); p <- ncol(data)

  # Convert character to factor
  for (j in seq_len(p)) {
    if (is.character(data[[j]])) {
      data[[j]] <- as.factor(data[[j]])
    }
  }

  is_continuous <- vapply(data, function(x) is.numeric(x) || is.integer(x),
                          logical(1))
  is_categorical <- !is_continuous
  cont_idx <- which(is_continuous)
  cat_idx <- which(is_categorical)
  p_cont <- length(cont_idx)

  if (p_cont < 2) {
    message("cellMCD requires at least 2 continuous variables. ",
            "Falling back to median/mode imputation.")
    data <- initialise(data, mixed = NULL, method = "median")
    W <- matrix(1, n, p, dimnames = list(rn, cn))
    return(list(data_imputed = data, cellweights = W, mu = NULL,
                Sigma = NULL, converged = TRUE, iterations = 0L))
  }

  M <- is.na(data)
  vars_miss <- which(colMeans(M) > 0)

  ## ---- initialize missing values ----
  data <- initialise(data, mixed = NULL, method = "median")

  ## ---- initialize cell weights ----
  W <- matrix(1, n, p, dimnames = list(rn, cn))

  ## ---- hard thresholding (detect-once) ----
  if (!is.null(hard_threshold) && hard_threshold > 0 && p_cont >= 2) {
    X_cont_init <- as.matrix(data[, cont_idx, drop = FALSE])
    W_init <- suppressWarnings(cellWeightsMCD(X_cont_init, method = method, alpha = alpha))
    n_removed <- 0L
    for (jj in seq_len(p_cont)) {
      j_global <- cont_idx[jj]
      flagged_j <- !M[, j_global] & (W_init[, jj] < hard_threshold)
      if (any(flagged_j)) {
        data[[j_global]][flagged_j] <- NA
        M[flagged_j, j_global] <- TRUE
        n_removed <- n_removed + sum(flagged_j)
      }
    }
    if (n_removed > 0) {
      data <- initialise(data, mixed = NULL, method = "median")
      vars_miss <- which(colMeans(M) > 0)
      if (trace) message(paste("  hard thresholding removed", n_removed, "cells"))
    }
  }

  converged <- FALSE
  iterations <- 0L
  d <- Inf

  for (iter in seq_len(maxit)) {
    iterations <- iter
    data_prev <- data

    if (trace) message(paste("cellMCD-mixed: iteration", iter))

    ## ---- Step 1: Robust covariance via MCD on continuous block ----
    X_cont <- as.matrix(data[, cont_idx, drop = FALSE])

    if (mcd_observed == "weighted" &&
        requireNamespace("cellWise", quietly = TRUE)) {
      # Use cellWise::cwLocScat — accepts n x p cell-weight matrix.
      # Observed clean cells get their current weight from W,
      # imputed (originally missing) cells get weight 0.
      W_cont <- W[, cont_idx, drop = FALSE]
      for (jj in seq_len(p_cont)) {
        W_cont[M[, cont_idx[jj]], jj] <- 0
      }
      cwls <- tryCatch(
        cellWise::cwLocScat(X_cont, W = W_cont,
                             crit = 1e-4, maxiter = 50),
        error = function(e) NULL
      )
      if (!is.null(cwls) && !is.null(cwls$cwMLEmu)) {
        mu <- cwls$cwMLEmu
        Sigma <- cwls$cwMLEsigma
      } else {
        mu <- apply(X_cont, 2, median)
        Sigma <- diag(apply(X_cont, 2, function(x) .robust_scale(x)^2))
        if (trace) message("  cwLocScat failed, using fallback")
      }
    } else if (mcd_observed == "pairwise") {
      # Pairwise complete MCD: use only observed (non-imputed) cells
      # For each pair, compute pairwise covariance on rows where both observed
      mu <- numeric(p_cont)
      for (jj in seq_len(p_cont)) {
        obs_jj <- !M[, cont_idx[jj]]
        mu[jj] <- median(X_cont[obs_jj, jj])
      }
      Sigma <- matrix(0, p_cont, p_cont)
      for (jj in seq_len(p_cont)) {
        for (kk in jj:p_cont) {
          obs_both <- !M[, cont_idx[jj]] & !M[, cont_idx[kk]]
          if (sum(obs_both) > 2) {
            xj <- X_cont[obs_both, jj] - mu[jj]
            xk <- X_cont[obs_both, kk] - mu[kk]
            # Robust pairwise via GK
            s_plus <- mad(xj + xk)^2
            s_minus <- mad(xj - xk)^2
            cov_jk <- (s_plus - s_minus) / 4
            Sigma[jj, kk] <- cov_jk
            Sigma[kk, jj] <- cov_jk
          }
        }
        obs_jj <- !M[, cont_idx[jj]]
        Sigma[jj, jj] <- .robust_scale(X_cont[obs_jj, jj])^2
      }
      Sigma <- .ensure_pd(Sigma)
    } else {
      # Default: MCD on all data (including imputed values)
      mcd <- tryCatch(
        robustbase::covMcd(X_cont, alpha = mcd_alpha),
        error = function(e) NULL
      )
      if (is.null(mcd)) {
        mu <- apply(X_cont, 2, median)
        Sigma <- diag(apply(X_cont, 2, function(x) .robust_scale(x)^2))
        if (trace) message("  MCD failed, using fallback")
      } else {
        mu <- mcd$center
        Sigma <- mcd$cov
      }
    }
    names(mu) <- cn[cont_idx]

    ## ---- Step 2: Compute cell weights from conditional residuals ----
    Sigma_reg <- Sigma + diag(1e-8, p_cont)

    for (jj in seq_len(p_cont)) {
      j_global <- cont_idx[jj]
      other <- setdiff(seq_len(p_cont), jj)

      if (length(other) == 0) {
        u_j <- (X_cont[, jj] - mu[jj]) / sqrt(max(Sigma[jj, jj], 1e-10))
      } else {
        Sigma_jmj <- Sigma_reg[jj, other, drop = FALSE]
        Sigma_mjmj <- Sigma_reg[other, other, drop = FALSE]
        Sigma_mjj <- Sigma_reg[other, jj, drop = FALSE]
        Sigma_mjmj_inv <- tryCatch(solve(Sigma_mjmj),
                                    error = function(e) MASS::ginv(Sigma_mjmj))

        dev_other <- sweep(X_cont[, other, drop = FALSE], 2, mu[other], "-")
        mu_cond <- mu[jj] + as.numeric(dev_other %*% (Sigma_mjmj_inv %*% Sigma_mjj))
        sigma2_cond <- max(Sigma_reg[jj, jj] -
          as.numeric(Sigma_jmj %*% Sigma_mjmj_inv %*% Sigma_mjj), 1e-10)
        u_j <- (X_cont[, jj] - mu_cond) / sqrt(sigma2_cond)
      }

      w_new <- .apply_weight_fun(u_j, method = method, alpha = alpha)
      # Adaptive damping
      lambda <- 0.3 + 0.7 * min(iter / maxit, 1)
      w_damped <- (1 - lambda) * W[, j_global] + lambda * w_new
      W[!M[, j_global], j_global] <- w_damped[!M[, j_global]]
      W[M[, j_global], j_global] <- 1
    }

    ## ---- Step 3: Impute missing cells via conditional expectation ----
    for (jj in seq_len(p_cont)) {
      j_global <- cont_idx[jj]
      impute_j <- which(M[, j_global])
      if (length(impute_j) == 0) next

      other <- setdiff(seq_len(p_cont), jj)
      if (length(other) == 0) {
        data[impute_j, j_global] <- mu[jj]
        next
      }

      Sigma_jmj <- Sigma_reg[jj, other, drop = FALSE]
      Sigma_mjmj <- Sigma_reg[other, other, drop = FALSE]
      Sigma_mjj <- Sigma_reg[other, jj, drop = FALSE]
      Sigma_mjmj_inv <- tryCatch(solve(Sigma_mjmj),
                                  error = function(e) MASS::ginv(Sigma_mjmj))

      dev_other <- sweep(X_cont[impute_j, other, drop = FALSE], 2, mu[other], "-")
      mu_cond <- mu[jj] + as.numeric(dev_other %*% (Sigma_mjmj_inv %*% Sigma_mjj))
      data[impute_j, j_global] <- mu_cond
    }
    # Update X_cont after imputation
    X_cont <- as.matrix(data[, cont_idx, drop = FALSE])

    ## ---- Step 4: Impute missing categorical via weighted multinomial ----
    cat_miss <- intersect(vars_miss, cat_idx)
    if (length(cat_miss) > 0) {
      # Row weights from continuous cell weights (geometric mean)
      w_row_cat <- if (p_cont > 0) {
        apply(W[, cont_idx, drop = FALSE], 1, function(ww)
          exp(mean(log(pmax(ww, 1e-10)))))
      } else rep(1, n)

      for (j_global in cat_miss) {
        miss_j <- which(M[, j_global])
        if (length(miss_j) == 0) next
        pred_cols <- setdiff(seq_len(p), j_global)
        form_j <- as.formula(paste0(cn[j_global], " ~ ",
                                     paste(cn[pred_cols], collapse = " + ")))
        multimod <- tryCatch(
          suppressMessages(nnet::multinom(form_j, data = data,
                                          weights = w_row_cat,
                                          maxit = 50, trace = FALSE)),
          error = function(e)
            suppressMessages(nnet::multinom(form_j, data = data,
                                            maxit = 50, trace = FALSE))
        )
        prob_pred <- predict(multimod, newdata = data[miss_j, , drop = FALSE],
                              type = "probs")
        lvls <- levels(data[[j_global]])
        if (is.null(dim(prob_pred))) {
          if (length(lvls) == 2)
            prob_pred <- matrix(c(1 - prob_pred, prob_pred), nrow = length(miss_j))
          else
            prob_pred <- matrix(prob_pred, nrow = 1)
          colnames(prob_pred) <- lvls
        }
        imputed_cats <- apply(prob_pred, 1, function(pp) {
          pp <- pmax(pp, 0); colnames(prob_pred)[which.max(pp)]
        })
        data[miss_j, j_global] <- factor(imputed_cats, levels = lvls)
      }
    }

    ## ---- Step 5: Check convergence ----
    d <- 0; n_imp <- 0
    for (jj in cont_idx) {
      miss_jj <- M[, jj]
      if (any(miss_jj)) {
        prev_vals <- data_prev[[jj]][miss_jj]
        curr_vals <- data[[jj]][miss_jj]
        d <- d + sum((prev_vals - curr_vals)^2) / (sum(prev_vals^2) + 1e-10)
        n_imp <- n_imp + 1L
      }
    }
    for (jj in cat_idx) {
      miss_jj <- M[, jj]
      if (any(miss_jj)) {
        d <- d + sum(data_prev[[jj]][miss_jj] != data[[jj]][miss_jj]) / sum(miss_jj)
        n_imp <- n_imp + 1L
      }
    }
    if (n_imp > 0) d <- d / n_imp
    if (trace) message(paste("  convergence:", round(d, 6)))
    if (d <= eps) { converged <- TRUE; break }
  }

  ## ---- Final: add uncertainty ----
  if (uncert == "conditional") {
    X_cont <- as.matrix(data[, cont_idx, drop = FALSE])
    for (jj in seq_len(p_cont)) {
      j_global <- cont_idx[jj]
      miss_j <- which(M[, j_global])
      if (length(miss_j) == 0) next
      other <- setdiff(seq_len(p_cont), jj)
      if (length(other) == 0) {
        s2 <- Sigma[jj, jj]
      } else {
        Sigma_jmj <- Sigma_reg[jj, other, drop = FALSE]
        Sigma_mjmj_inv <- tryCatch(
          solve(Sigma_reg[other, other, drop = FALSE]),
          error = function(e) MASS::ginv(Sigma_reg[other, other, drop = FALSE]))
        s2 <- max(Sigma[jj, jj] - as.numeric(
          Sigma_jmj %*% Sigma_mjmj_inv %*% Sigma_reg[other, jj, drop = FALSE]), 1e-10)
      }
      data[miss_j, j_global] <- data[miss_j, j_global] + rnorm(length(miss_j), 0, sqrt(s2))
    }
  }

  rownames(data) <- rn
  list(data_imputed = data, cellweights = W, mu = mu, Sigma = Sigma,
       converged = converged, iterations = iterations)
}
