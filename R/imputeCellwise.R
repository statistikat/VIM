#' Cellwise-robust iterative regression imputation for mixed data
#'
#' Extends IRMI (Templ, Kowarik, Filzmoser 2011) with cellwise contamination
#' handling. Each conditional regression uses a cell-weighted IRWLS engine
#' where per-cell weights in the design matrix downweight contaminated cells
#' without discarding entire observations.
#'
#' The algorithm works iteratively: in each outer iteration, every variable
#' with missing values is used as response in a conditional regression on
#' all remaining variables. For continuous responses, the custom
#' \code{cellIRWLS()} engine fits a weighted regression where each cell in
#' the design matrix receives its own weight reflecting potential cellwise
#' contamination. For categorical responses, a weighted multinomial model
#' is used. After each regression, cell weights for the response variable
#' are updated from the residuals.
#'
#' @param data data.frame with missing values (mixed continuous + categorical).
#' @param method weight function: \code{"huber"} (default) or \code{"tukey"}.
#' @param alpha tuning constant. \code{NULL} (default) uses 1.345 for Huber
#'   and 4.685 for Tukey, which give 95\% efficiency at the normal model.
#' @param maxit maximum outer IRMI iterations (default: 10).
#' @param maxit_irwls maximum inner IRWLS iterations per regression
#'   (default: 50).
#' @param eps convergence tolerance for outer loop (default: 1e-2).
#' @param eps_irwls convergence tolerance for inner IRWLS (default: 1e-6).
#' @param uncert imputation uncertainty method: \code{"pmm"} (default),
#'   \code{"normalerror"}, or \code{"resid"}.
#' @param trace logical; if \code{TRUE}, print progress information.
#'
#' @return A list with components:
#'   \item{data_imputed}{the imputed data.frame}
#'   \item{cellweights}{n x p matrix of final cell weights (1 = clean,
#'     0 = fully downweighted). Categorical columns always have weight 1.}
#'   \item{converged}{logical indicating whether the outer loop converged}
#'   \item{iterations}{number of outer iterations used}
#'
#' @details
#' The algorithm proceeds as follows:
#' \enumerate{
#'   \item Missing values are initialised using \code{\link{initialise}}.
#'   \item Initial cell weights are computed with \code{cellWeights()} on
#'     all continuous variables in the initialised data.
#'   \item \strong{Outer loop} (up to \code{maxit} iterations):
#'     \itemize{
#'       \item For each variable \eqn{j} with missing values:
#'         \itemize{
#'           \item Form predictor matrix \eqn{X} (all other variables) and
#'             response \eqn{y} (variable \eqn{j}).
#'           \item If \eqn{j} is continuous: fit \code{cellIRWLS(X, y,
#'             w_cell, w_response)} and impute missing values in \eqn{j}
#'             using the fitted model plus uncertainty.
#'           \item If \eqn{j} is categorical: fit \code{nnet::multinom()}
#'             with row weights derived from the cell weight matrix and
#'             impute by sampling from predicted probabilities.
#'           \item Update cell weights for \eqn{j} from residuals via
#'             \code{cellWeightsFromResiduals()}.
#'         }
#'       \item Check convergence: relative change in imputed values < \code{eps}.
#'     }
#' }
#'
#' @author Matthias Templ
#' @references
#' M. Templ, A. Kowarik, P. Filzmoser (2011) Iterative stepwise regression
#' imputation using standard and robust methods. \emph{Journal of
#' Computational Statistics and Data Analysis}, Vol. 55, pp. 2793-2806.
#'
#' @family imputation methods
#' @seealso \code{\link{imputeCellM}}, \code{\link{initialise}},
#'   \code{\link{irmi}}
#'
#' @examples
#' \dontrun{
#' data(sleep, package = "VIM")
#' result <- imputeCellIRMI(sleep)
#' head(result$data_imputed)
#' image(result$cellweights, main = "Cell weights")
#'
#' # With Tukey bisquare weights for stronger downweighting
#' result2 <- imputeCellIRMI(sleep, method = "tukey", trace = TRUE)
#'
#' # Mixed data example
#' data(testdata)
#' result3 <- imputeCellIRMI(testdata$wna)
#' }
#'
#' @export
#' @importFrom VIM initialise
#' @importFrom stats model.matrix median mad rnorm predict sd as.formula
imputeCellIRMI <- function(data, method = "huber", alpha = NULL,
                            maxit = 10, maxit_irwls = 50,
                            eps = 1e-2, eps_irwls = 1e-6,
                            uncert = "pmm", trace = FALSE) {

  ## ---- input validation ----
  check_data(data)
  if (!is.data.frame(data)) {
    if (is.matrix(data))
      data <- as.data.frame(data)
    else
      stop("data must be a data.frame or matrix")
  }
  method <- match.arg(method, c("huber", "tukey"))
  uncert <- match.arg(uncert, c("pmm", "normalerror", "resid"))

  if (is.null(alpha)) {
    alpha <- if (method == "huber") 1.345 else 4.685
  }
  if (ncol(data) < 2) stop("Need at least 2 variables.")
  if (!any(is.na(data))) {
    message("No missing values in data. Nothing to impute.")
    n <- nrow(data)
    p <- ncol(data)
    W <- matrix(1, nrow = n, ncol = p,
                dimnames = list(rownames(data), colnames(data)))
    return(list(data_imputed = data, cellweights = W,
                converged = TRUE, iterations = 0L))
  }
  if (any(apply(data, 1, function(x) all(is.na(x)))))
    stop("Unit non-responses (entire row missing) detected. Remove them first.")

  ## ---- detect variable types ----
  rn <- rownames(data)
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

  ## ---- record missingness pattern ----
  M <- is.na(data)
  vars_miss <- which(colMeans(M) > 0)
  if (length(vars_miss) == 0) {
    W <- matrix(1, nrow = n, ncol = p,
                dimnames = list(rn, colnames(data)))
    return(list(data_imputed = data, cellweights = W,
                converged = TRUE, iterations = 0L))
  }

  ## ---- step 1: initialise missing values ----
  data <- initialise(data, mixed = NULL, method = "median")

  ## ---- step 2: compute initial cell weights ----
  W <- matrix(1, nrow = n, ncol = p,
              dimnames = list(rn, colnames(data)))

  # initial cell weights only for continuous variables
  if (any(is_continuous)) {
    W[, is_continuous] <- cellWeights(
      as.matrix(data[, is_continuous, drop = FALSE]),
      method = method, alpha = alpha
    )
  }

  ## ---- step 3: outer loop ----
  converged <- FALSE
  iterations <- 0L
  d <- Inf

  while (d > eps && iterations < maxit) {
    iterations <- iterations + 1L
    if (trace) {
      message("--------------------------------------")
      message(paste("cellIRMI: start of iteration", iterations))
    }
    data_previous <- data

    ## ---- inner loop: iterate over variables with missings ----
    for (j in vars_miss) {
      if (trace) {
        message(paste("  imputing variable:", j,
                      "(", colnames(data)[j], ") -",
                      ifelse(is_continuous[j], "continuous",
                             "categorical")))
      }

      miss_j <- M[, j]  # logical: which rows are missing for variable j
      n_miss <- sum(miss_j)
      if (n_miss == 0) next

      # predictor columns
      pred_cols <- setdiff(seq_len(p), j)

      if (is_continuous[j]) {
        ## ---- continuous response: cellIRWLS ----
        y <- data[[j]]

        # build numeric design matrix
        X <- .build_design_matrix(data, pred_cols)

        # cell weights for predictors (map to design matrix columns)
        w_cell_raw <- W[, pred_cols, drop = FALSE]
        w_cell <- .expand_cell_weights(data, pred_cols, w_cell_raw)

        # cell weights for response
        w_response <- W[, j]

        # fit cell-weighted IRWLS
        fit <- cellIRWLS(X, y, w_cell = w_cell, w_response = w_response,
                         method = method, alpha = alpha,
                         maxit = maxit_irwls, eps = eps_irwls)

        # compute predictions for all rows
        pred_all <- as.numeric(cbind(1, X) %*% fit$coefficients)
        sigma_hat <- fit$sigma

        # impute with uncertainty
        data[miss_j, j] <- .add_uncertainty(
          pred = pred_all[miss_j],
          y_obs = y[!miss_j],
          pred_obs = pred_all[!miss_j],
          sigma = sigma_hat,
          uncert = uncert
        )

        # update cell weights for column j from residuals
        resid_j <- y - pred_all
        w_new <- cellWeightsFromResiduals(
          resid_j, sigma = sigma_hat,
          method = method, alpha = alpha
        )
        # only update weights for observed rows; imputed rows are trusted
        W[!miss_j, j] <- w_new[!miss_j]
        W[miss_j, j] <- 1

      } else {
        ## ---- categorical response: weighted multinomial ----
        y <- data[[j]]

        # row weights = geometric mean of cell weights across predictors
        w_row <- apply(W[, pred_cols, drop = FALSE], 1, function(ww) {
          exp(mean(log(pmax(ww, 1e-10))))
        })

        # build formula
        form_j <- as.formula(
          paste0(colnames(data)[j], " ~ ",
                 paste(colnames(data)[pred_cols], collapse = " + "))
        )

        # fit weighted multinomial
        multimod <- tryCatch({
          suppressMessages(
            nnet::multinom(form_j, data = data, weights = w_row,
                           maxit = 50, trace = FALSE, MaxNWts = 50000)
          )
        }, error = function(e) {
          warning(paste("Multinomial model failed for variable",
                        colnames(data)[j], ":", e$message,
                        "- using unweighted model"))
          suppressMessages(
            nnet::multinom(form_j, data = data,
                           maxit = 50, trace = FALSE, MaxNWts = 50000)
          )
        })

        # predict probabilities for missing rows
        prob_pred <- predict(multimod,
                             newdata = data[miss_j, , drop = FALSE],
                             type = "probs")

        # handle edge case: single missing row or binary factor
        if (is.null(dim(prob_pred))) {
          if (nlevels(y) == 2) {
            prob_pred <- matrix(c(1 - prob_pred, prob_pred),
                                nrow = n_miss)
            colnames(prob_pred) <- levels(y)
          } else {
            prob_pred <- matrix(prob_pred, nrow = 1)
            colnames(prob_pred) <- levels(y)
          }
        }

        # sample from predicted probabilities
        imputed_cats <- apply(prob_pred, 1, function(pp) {
          pp <- pmax(pp, 0)
          pp <- pp / sum(pp)
          sample(colnames(prob_pred), size = 1, prob = pp)
        })

        if (is.factor(data[[j]])) {
          data[miss_j, j] <- factor(imputed_cats,
                                     levels = levels(data[[j]]))
        } else {
          data[miss_j, j] <- imputed_cats
        }
        # categorical columns keep W[,j] = 1
      }
    }  # end inner loop

    ## ---- check convergence ----
    d <- 0
    if (any(is_continuous)) {
      cont_cols <- which(is_continuous)
      d <- d + sum(
        (as.matrix(data_previous[, cont_cols]) -
           as.matrix(data[, cont_cols]))^2,
        na.rm = TRUE
      )
    }
    if (any(is_categorical)) {
      cat_cols <- which(is_categorical)
      d <- d + sum(
        as.matrix(data_previous[, cat_cols]) !=
          as.matrix(data[, cat_cols]),
        na.rm = TRUE
      )
    }

    if (trace) {
      message(paste("  convergence criterion:", round(d, 6)))
    }

    if (d <= eps) {
      converged <- TRUE
      if (trace) message("cellIRMI converged.")
    }
  }  # end outer loop

  if (!converged && trace) {
    message(paste("cellIRMI did not converge after", maxit, "iterations.",
                  "Final criterion:", round(d, 6)))
  }

  rownames(data) <- rn
  list(
    data_imputed = data,
    cellweights  = W,
    converged    = converged,
    iterations   = iterations
  )
}


#' Cellwise M-estimation imputation
#'
#' Impute missing values in a single response variable using a cell-weighted
#' M-estimation approach. Each cell in the predictor matrix receives its own
#' weight reflecting potential cellwise contamination, so that contaminated
#' predictor cells are downweighted without discarding entire observations.
#'
#' @param formula model formula (e.g., \code{y ~ x1 + x2}).
#' @param data data.frame containing the data.
#' @param method weight function: \code{"huber"} (default) or \code{"tukey"}.
#' @param alpha tuning constant. \code{NULL} (default) uses 1.345 for Huber
#'   and 4.685 for Tukey.
#' @param maxit_irwls maximum IRWLS iterations (default: 50).
#' @param eps_irwls convergence tolerance for IRWLS (default: 1e-6).
#' @param uncert imputation uncertainty method: \code{"pmm"} (default),
#'   \code{"normalerror"}, or \code{"resid"}.
#' @param value_back \code{"all"} (default) returns the complete dataset,
#'   or \code{"ymiss"} returns only the imputed values.
#'
#' @return If \code{value_back = "all"}, the imputed data.frame is returned
#'   (same structure as input). If \code{value_back = "ymiss"}, a named
#'   vector of imputed values (for rows that were originally missing) is
#'   returned.
#'
#' @details
#' This is a lightweight single-formula alternative to
#' \code{\link{imputeCellIRMI}}. It fits one cell-weighted IRWLS regression
#' using \code{cellIRWLS()} and imputes the missing values in the response
#' variable. This is appropriate when only one variable needs imputation
#' and a specific model formula is desired.
#'
#' For categorical response variables, a weighted multinomial model via
#' \code{\link[nnet]{multinom}} is fitted instead. Categorical predictors
#' are not subject to the cellwise contamination model (their cell weights
#' are always 1).
#'
#' @author Matthias Templ
#' @references
#' M. Templ, A. Kowarik, P. Filzmoser (2011) Iterative stepwise regression
#' imputation using standard and robust methods. \emph{Journal of
#' Computational Statistics and Data Analysis}, Vol. 55, pp. 2793-2806.
#'
#' @family imputation methods
#' @seealso \code{\link{imputeCellIRMI}}, \code{\link{imputeRobust}}
#'
#' @examples
#' \dontrun{
#' data(sleep, package = "VIM")
#' # Impute Dream using BodyWgt and BrainWgt as predictors
#' result <- imputeCellM(Dream ~ BodyWgt + BrainWgt, data = sleep)
#' head(result)
#'
#' # Return only imputed values
#' impvals <- imputeCellM(Dream ~ BodyWgt + BrainWgt, data = sleep,
#'                        value_back = "ymiss")
#'
#' # Tukey bisquare weights
#' result2 <- imputeCellM(Dream ~ BodyWgt + BrainWgt, data = sleep,
#'                        method = "tukey")
#' }
#'
#' @export
#' @importFrom stats model.frame model.extract model.matrix as.formula
imputeCellM <- function(formula, data, method = "huber", alpha = NULL,
                        maxit_irwls = 50, eps_irwls = 1e-6,
                        uncert = "pmm", value_back = "all") {

  ## ---- input validation ----
  check_data(data)
  if (!is.data.frame(data)) {
    if (is.matrix(data))
      data <- as.data.frame(data)
    else
      stop("data must be a data.frame or matrix")
  }
  method <- match.arg(method, c("huber", "tukey"))
  uncert <- match.arg(uncert, c("pmm", "normalerror", "resid"))
  value_back <- match.arg(value_back, c("all", "ymiss"))

  if (is.null(alpha)) {
    alpha <- if (method == "huber") 1.345 else 4.685
  }

  rn <- rownames(data)

  ## ---- parse formula ----
  y_var <- all.vars(formula)[1]
  x_vars <- all.vars(formula)[-1]

  if (!y_var %in% colnames(data))
    stop(paste("Response variable", y_var, "not found in data."))
  if (!all(x_vars %in% colnames(data)))
    stop("Not all predictor variables found in data.")

  missindex <- is.na(data[[y_var]])
  n_miss <- sum(missindex)

  if (n_miss == 0) {
    message(paste("No missing values in", y_var,
                  "- nothing to impute."))
    if (value_back == "ymiss") return(numeric(0))
    return(data)
  }

  ## ---- initialise missing values in predictors ----
  pred_cols <- match(x_vars, colnames(data))
  data_work <- data
  for (col_idx in pred_cols) {
    v <- data_work[[col_idx]]
    if (any(is.na(v))) {
      if (is.numeric(v)) {
        data_work[[col_idx]][is.na(v)] <- median(v, na.rm = TRUE)
      } else if (is.factor(v) || is.character(v)) {
        mode_val <- names(which.max(table(v, useNA = "no")))
        data_work[[col_idx]][is.na(v)] <- mode_val
      }
    }
  }
  # also initialise response for complete-data operations
  y_orig <- data[[y_var]]
  if (is.numeric(y_orig)) {
    data_work[[y_var]][missindex] <- median(y_orig, na.rm = TRUE)
  } else if (is.factor(y_orig) || is.character(y_orig)) {
    mode_val <- names(which.max(table(y_orig, useNA = "no")))
    if (is.factor(y_orig)) {
      data_work[[y_var]][missindex] <- factor(mode_val,
                                               levels = levels(y_orig))
    } else {
      data_work[[y_var]][missindex] <- mode_val
    }
  }

  ## ---- determine response type ----
  y_is_continuous <- is.numeric(data[[y_var]])
  n <- nrow(data_work)

  if (y_is_continuous) {
    ## ---- continuous response: cellIRWLS ----
    y <- data_work[[y_var]]

    # build design matrix from predictors
    X <- .build_design_matrix(data_work, pred_cols)

    # compute cell weights for predictors
    w_cell_raw <- .compute_predictor_cellweights(data_work, pred_cols,
                                                  method = method,
                                                  alpha = alpha)
    w_cell <- .expand_cell_weights(data_work, pred_cols, w_cell_raw)

    # initial response weights (all 1)
    w_response <- rep(1, n)

    # fit cellIRWLS
    fit <- cellIRWLS(X, y, w_cell = w_cell, w_response = w_response,
                     method = method, alpha = alpha,
                     maxit = maxit_irwls, eps = eps_irwls)

    # predictions
    pred_all <- as.numeric(cbind(1, X) %*% fit$coefficients)
    sigma_hat <- fit$sigma

    # impute with uncertainty
    ymiss <- .add_uncertainty(
      pred = pred_all[missindex],
      y_obs = y[!missindex],
      pred_obs = pred_all[!missindex],
      sigma = sigma_hat,
      uncert = uncert
    )

  } else {
    ## ---- categorical response: weighted multinomial ----
    y <- data_work[[y_var]]

    # compute row weights from cell weights of continuous predictors
    cont_pred <- pred_cols[vapply(data_work[pred_cols], is.numeric,
                                  logical(1))]
    if (length(cont_pred) > 0) {
      w_cell_raw <- .compute_predictor_cellweights(
        data_work, cont_pred, method = method, alpha = alpha
      )
      w_row <- apply(w_cell_raw, 1, function(ww) {
        exp(mean(log(pmax(ww, 1e-10))))
      })
    } else {
      w_row <- rep(1, n)
    }

    # fit weighted multinomial
    multimod <- tryCatch({
      suppressMessages(
        nnet::multinom(formula, data = data_work, weights = w_row,
                       maxit = 50, trace = FALSE, MaxNWts = 50000)
      )
    }, error = function(e) {
      warning(paste("Multinomial model failed:", e$message,
                    "- using unweighted model"))
      suppressMessages(
        nnet::multinom(formula, data = data_work,
                       maxit = 50, trace = FALSE, MaxNWts = 50000)
      )
    })

    prob_pred <- predict(multimod,
                         newdata = data_work[missindex, , drop = FALSE],
                         type = "probs")

    lvls <- levels(data[[y_var]])
    if (is.null(lvls)) lvls <- sort(unique(na.omit(data[[y_var]])))

    # handle edge cases
    if (is.null(dim(prob_pred))) {
      if (length(lvls) == 2) {
        prob_pred <- matrix(c(1 - prob_pred, prob_pred),
                            nrow = n_miss)
        colnames(prob_pred) <- lvls
      } else {
        prob_pred <- matrix(prob_pred, nrow = 1)
        colnames(prob_pred) <- lvls
      }
    }

    ymiss <- apply(prob_pred, 1, function(pp) {
      pp <- pmax(pp, 0)
      pp <- pp / sum(pp)
      sample(colnames(prob_pred), size = 1, prob = pp)
    })

    if (is.factor(data[[y_var]])) {
      ymiss <- factor(ymiss, levels = levels(data[[y_var]]))
    }
  }

  ## ---- return ----
  if (value_back == "ymiss") {
    return(ymiss)
  } else {
    data[missindex, y_var] <- ymiss
    rownames(data) <- rn
    return(data)
  }
}


# ============================================================================
# Internal helper functions
# ============================================================================

#' Build a numeric design matrix from selected columns
#'
#' Handles both numeric and factor predictors. Factors are expanded into
#' dummy variables (treatment coding, dropping the first level as
#' reference). Does NOT include an intercept column.
#'
#' @param data data.frame
#' @param pred_cols integer vector of column indices
#' @return numeric matrix (n x q) where q is the total number of design
#'   columns after dummy expansion
#' @keywords internal
#' @noRd
.build_design_matrix <- function(data, pred_cols) {
  parts <- list()
  for (col in pred_cols) {
    v <- data[[col]]
    if (is.numeric(v)) {
      parts[[length(parts) + 1L]] <- matrix(
        v, ncol = 1,
        dimnames = list(NULL, colnames(data)[col])
      )
    } else if (is.factor(v) || is.character(v)) {
      if (is.character(v)) v <- as.factor(v)
      # create dummy variables (omit first level as reference)
      lvls <- levels(v)
      if (length(lvls) > 1) {
        dmat <- matrix(0, nrow = nrow(data), ncol = length(lvls) - 1)
        colnames(dmat) <- paste0(colnames(data)[col], lvls[-1])
        for (k in seq_along(lvls[-1])) {
          dmat[, k] <- as.numeric(v == lvls[k + 1])
        }
        parts[[length(parts) + 1L]] <- dmat
      }
    }
  }
  do.call(cbind, parts)
}


#' Expand per-variable cell weights to match the design matrix
#'
#' When factors are dummy-expanded, each dummy column inherits the cell
#' weight of the parent factor variable (which is 1 for categorical
#' variables).
#'
#' @param data data.frame
#' @param pred_cols integer vector of column indices in \code{data}
#' @param w_cell_raw n x length(pred_cols) matrix of per-variable
#'   cell weights
#' @return n x q matrix matching the number of design matrix columns
#' @keywords internal
#' @noRd
.expand_cell_weights <- function(data, pred_cols, w_cell_raw) {
  parts <- list()
  k <- 0L
  for (i in seq_along(pred_cols)) {
    col <- pred_cols[i]
    v <- data[[col]]
    k <- k + 1L
    if (is.numeric(v)) {
      parts[[length(parts) + 1L]] <- w_cell_raw[, k, drop = FALSE]
    } else if (is.factor(v) || is.character(v)) {
      if (is.character(v)) v <- as.factor(v)
      n_dummy <- max(nlevels(v) - 1L, 0L)
      if (n_dummy > 0) {
        # replicate the cell weight for each dummy column
        parts[[length(parts) + 1L]] <- matrix(
          rep(w_cell_raw[, k], n_dummy),
          nrow = nrow(data), ncol = n_dummy
        )
      }
    }
  }
  do.call(cbind, parts)
}


#' Compute cell weights for predictor columns
#'
#' Applies \code{cellWeights()} to the continuous predictors and returns
#' an n x length(pred_cols) matrix where categorical columns get weight 1.
#'
#' @param data data.frame
#' @param pred_cols integer vector of column indices
#' @param method \code{"huber"} or \code{"tukey"}
#' @param alpha tuning constant
#' @return n x length(pred_cols) matrix
#' @keywords internal
#' @noRd
.compute_predictor_cellweights <- function(data, pred_cols,
                                            method, alpha) {
  n <- nrow(data)
  W_pred <- matrix(1, nrow = n, ncol = length(pred_cols))

  is_num <- vapply(data[pred_cols], is.numeric, logical(1))
  cont_idx <- which(is_num)

  if (length(cont_idx) > 0) {
    X_cont <- as.matrix(data[, pred_cols[cont_idx], drop = FALSE])
    W_pred[, cont_idx] <- cellWeights(X_cont, method = method,
                                       alpha = alpha)
  }

  W_pred
}


#' Add imputation uncertainty
#'
#' Depending on the \code{uncert} method, adds noise or uses predictive
#' mean matching to introduce appropriate uncertainty into imputed values.
#'
#' @param pred numeric vector of predictions for the missing rows
#' @param y_obs numeric vector of observed response values
#' @param pred_obs numeric vector of predictions for observed rows
#' @param sigma estimated residual standard deviation
#' @param uncert one of \code{"pmm"}, \code{"normalerror"}, \code{"resid"}
#' @return numeric vector of imputed values (same length as \code{pred})
#' @keywords internal
#' @noRd
.add_uncertainty <- function(pred, y_obs, pred_obs, sigma, uncert) {
  n_miss <- length(pred)

  if (uncert == "normalerror") {
    ymiss <- pred + rnorm(n_miss, mean = 0, sd = sigma)
  } else if (uncert == "resid") {
    obs_resid <- y_obs - pred_obs
    ymiss <- pred + sample(obs_resid, size = n_miss, replace = TRUE)
  } else {
    # PMM: predictive mean matching with 5 donors
    ymiss <- .pmm_impute(pred, y_obs, pred_obs, n_donors = 5)
  }

  ymiss
}


#' Predictive mean matching
#'
#' For each predicted value for a missing row, find the \code{n_donors}
#' observed values whose predictions are closest, then randomly sample
#' one observed value from those donors.
#'
#' @param pred_miss predictions for missing rows
#' @param y_obs observed response values
#' @param pred_obs predictions for observed rows
#' @param n_donors number of candidate donors (default: 5)
#' @return numeric vector of imputed values
#' @keywords internal
#' @noRd
.pmm_impute <- function(pred_miss, y_obs, pred_obs, n_donors = 5) {
  n_miss <- length(pred_miss)
  n_obs <- length(y_obs)
  n_donors <- min(n_donors, n_obs)

  ymiss <- numeric(n_miss)
  for (i in seq_len(n_miss)) {
    dists <- abs(pred_miss[i] - pred_obs)
    donor_idx <- order(dists)[seq_len(n_donors)]
    ymiss[i] <- sample(y_obs[donor_idx], size = 1)
  }

  ymiss
}
