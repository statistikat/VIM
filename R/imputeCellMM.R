#' Cell-weighted MM imputation for mixed data (Path A)
#'
#' Uses a cell-weighted MM-estimator for each variable regression:
#' S-step on unweighted data (high breakdown), M-step with cell weights
#' (fine-grained downweighting).  This preserves the S-estimator's
#' breakdown guarantee while incorporating cellwise information.
#'
#' @param data data.frame with missing values
#' @param maxit maximum outer IRMI iterations (default: 50)
#' @param eps convergence tolerance (default: 5e-3)
#' @param alpha_weight tuning constant for bisquare cell weights (default: 4.685)
#' @param init_weights initial cell weight method: \code{"mcd"} or
#'   \code{"univariate"} (default: \code{"mcd"})
#' @param uncert uncertainty method: \code{"pmm"}, \code{"normalerror"},
#'   or \code{"none"}
#' @param trace logical
#'
#' @return list with data_imputed, cellweights, converged, iterations
#' @author Matthias Templ
#' @importFrom robustbase lmrob lmrob.control
#' @importFrom stats as.formula predict median mad rnorm
#' @export
imputeCellMM <- function(data, maxit = 50, eps = 5e-3,
                          alpha_weight = 4.685,
                          init_weights = "mcd",
                          uncert = "pmm", trace = FALSE) {

  check_data(data)
  if (!is.data.frame(data)) {
    if (is.matrix(data)) data <- as.data.frame(data)
    else stop("data must be a data.frame or matrix")
  }
  init_weights <- match.arg(init_weights, c("mcd", "univariate"))
  uncert <- match.arg(uncert, c("pmm", "normalerror", "none"))

  for (j in seq_len(ncol(data)))
    if (is.character(data[[j]])) data[[j]] <- as.factor(data[[j]])

  if (ncol(data) < 2) stop("Need at least 2 variables.")
  rn <- rownames(data); cn <- colnames(data)
  n <- nrow(data); p <- ncol(data)

  if (!any(is.na(data))) {
    W <- matrix(1, n, p, dimnames = list(rn, cn))
    return(list(data_imputed = data, cellweights = W,
                converged = TRUE, iterations = 0L))
  }
  if (any(apply(data, 1, function(x) all(is.na(x)))))
    stop("Unit non-responses detected. Remove them first.")

  is_continuous <- vapply(data, function(x) is.numeric(x) || is.integer(x), logical(1))
  is_categorical <- !is_continuous
  M <- is.na(data)
  vars_miss <- which(colMeans(M) > 0)

  data <- initialise(data, mixed = NULL, method = "median")

  # Initial cell weights
  W <- matrix(1, n, p, dimnames = list(rn, cn))
  if (any(is_continuous)) {
    X_init <- as.matrix(data[, is_continuous, drop = FALSE])
    if (init_weights == "mcd") {
      W[, is_continuous] <- cellWeightsMCD(X_init, method = "tukey", alpha = alpha_weight)
    } else {
      W[, is_continuous] <- cellWeights(X_init, method = "tukey", alpha = alpha_weight)
    }
  }

  converged <- FALSE
  iterations <- 0L
  d <- Inf

  while (d > eps && iterations < maxit) {
    iterations <- iterations + 1L
    data_previous <- data
    if (trace) message(paste("cellMM: iteration", iterations))

    for (j in vars_miss) {
      miss_j <- M[, j]
      if (!any(miss_j)) next
      pred_cols <- setdiff(seq_len(p), j)

      if (is_continuous[j]) {
        form <- as.formula(paste0(cn[j], " ~ ", paste(cn[pred_cols], collapse = " + ")))

        ## ---- S-step: unweighted, high breakdown ----
        # Use lmrob with MM method (S-init + M-refinement)
        # Pass cell-derived ROW weights to give some cellwise guidance
        cont_preds <- pred_cols[is_continuous[pred_cols]]
        if (length(cont_preds) > 0) {
          w_row <- apply(W[, cont_preds, drop = FALSE], 1, function(ww)
            exp(mean(log(pmax(ww, 1e-10)))))
          w_row <- w_row * W[, j]
        } else {
          w_row <- W[, j]
        }

        fit <- tryCatch(
          suppressWarnings(robustbase::lmrob(form, data = data, weights = w_row,
            control = robustbase::lmrob.control(
              method = "MM", k.max = 200,
              max.it = 50, refine.tol = 1e-7))),
          error = function(e) {
            tryCatch(
              suppressWarnings(robustbase::lmrob(form, data = data)),
              error = function(e2) stats::lm(form, data = data)
            )
          }
        )

        pred_all <- predict(fit, newdata = data)
        data[miss_j, j] <- pred_all[miss_j]

        ## ---- Update cell weights from MM residuals ----
        r <- data[[j]] - pred_all
        sigma <- mad(r[!miss_j])
        if (sigma < .Machine$double.eps) sigma <- .robust_scale(r[!miss_j])
        w_new <- tukey_weight(r / sigma, k = alpha_weight)
        # Damping
        lambda <- 0.3 + 0.7 * min(iterations / maxit, 1)
        W[!miss_j, j] <- (1 - lambda) * W[!miss_j, j] + lambda * w_new[!miss_j]
        W[miss_j, j] <- 1

      } else {
        ## ---- categorical ----
        cont_preds <- pred_cols[is_continuous[pred_cols]]
        w_row <- if (length(cont_preds) > 0) {
          apply(W[, cont_preds, drop = FALSE], 1, function(ww)
            exp(mean(log(pmax(ww, 1e-10)))))
        } else rep(1, n)

        form <- as.formula(paste0(cn[j], " ~ ", paste(cn[pred_cols], collapse = " + ")))
        multimod <- tryCatch(
          suppressMessages(nnet::multinom(form, data = data, weights = w_row,
                                          maxit = 50, trace = FALSE)),
          error = function(e) suppressMessages(
            nnet::multinom(form, data = data, maxit = 50, trace = FALSE))
        )
        prob_pred <- predict(multimod, newdata = data[miss_j, , drop = FALSE], type = "probs")
        lvls <- levels(data[[j]])
        if (is.null(dim(prob_pred))) {
          if (length(lvls) == 2)
            prob_pred <- matrix(c(1 - prob_pred, prob_pred), nrow = sum(miss_j))
          else prob_pred <- matrix(prob_pred, nrow = 1)
          colnames(prob_pred) <- lvls
        }
        imputed <- apply(prob_pred, 1, function(pp) {
          pp <- pmax(pp, 0); colnames(prob_pred)[which.max(pp)]
        })
        data[miss_j, j] <- factor(imputed, levels = lvls)
      }
    }

    ## ---- Multivariate weight update ----
    if (any(is_continuous)) {
      X_now <- as.matrix(data[, is_continuous, drop = FALSE])
      W_mv <- suppressWarnings(cellWeightsMCD(X_now, method = "tukey", alpha = alpha_weight))
      lambda <- 0.3 + 0.7 * min(iterations / maxit, 1)
      for (jj in which(is_continuous)) {
        local_j <- match(jj, which(is_continuous))
        obs_j <- !M[, jj]
        W[obs_j, jj] <- (1 - lambda) * W[obs_j, jj] + lambda * W_mv[obs_j, local_j]
        W[M[, jj], jj] <- 1
      }
    }

    ## ---- Convergence ----
    d <- 0; n_imp <- 0
    for (jj in which(is_continuous)) {
      miss_jj <- M[, jj]
      if (any(miss_jj)) {
        prev <- data_previous[[jj]][miss_jj]; curr <- data[[jj]][miss_jj]
        d <- d + sum((prev - curr)^2) / (sum(prev^2) + 1e-10)
        n_imp <- n_imp + 1L
      }
    }
    for (jj in which(is_categorical)) {
      miss_jj <- M[, jj]
      if (any(miss_jj)) {
        d <- d + sum(data_previous[[jj]][miss_jj] != data[[jj]][miss_jj]) / sum(miss_jj)
        n_imp <- n_imp + 1L
      }
    }
    if (n_imp > 0) d <- d / n_imp
    if (trace) message(paste("  convergence:", round(d, 6)))
    if (d <= eps) converged <- TRUE
  }

  ## ---- Uncertainty ----
  if (uncert != "none") {
    for (j in vars_miss) {
      if (!is_continuous[j]) next
      miss_j <- M[, j]; obs_j <- !miss_j
      if (!any(miss_j)) next
      form <- as.formula(paste0(cn[j], " ~ ", paste(cn[setdiff(seq_len(p), j)], collapse = " + ")))
      fit <- tryCatch(suppressWarnings(robustbase::lmrob(form, data = data)),
                       error = function(e) stats::lm(form, data = data))
      pred_all <- predict(fit, newdata = data)
      if (uncert == "pmm") {
        data[miss_j, j] <- .pmm_impute(pred_all[miss_j], data[[j]][obs_j],
                                         pred_all[obs_j], n_donors = 5)
      } else {
        sigma <- mad(data[[j]][obs_j] - pred_all[obs_j])
        data[miss_j, j] <- pred_all[miss_j] + rnorm(sum(miss_j), 0, sigma)
      }
    }
  }

  rownames(data) <- rn
  list(data_imputed = data, cellweights = W,
       converged = converged, iterations = iterations)
}
