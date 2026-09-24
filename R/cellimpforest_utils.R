# Internal helpers for cellImpForest(). Pure functions, tested in
# inst/tinytest/test_cellimpforest_utils.R and test_cellimpforest_engines.R.

#' Tukey bisquare weights for standardised residuals (NA stays NA)
#' @noRd
.cif_bisquare <- function(z, psi_c = 4.685) {
  w <- (1 - (z / psi_c)^2)^2
  w[!is.na(z) & abs(z) > psi_c] <- 0
  w[is.na(z)] <- NA_real_
  w
}

#' Two-sided normal tail probability of a standardised residual (small = surprising)
#' @noRd
.cif_surprise <- function(z) 2 * stats::pnorm(-abs(z))

#' Robust global residual scale of one column: MAD, falling back to SD, then to 1
#' @param r residuals, NA where the cell is missing or flagged
#' @param use logical, cells allowed into the scale (observed, unflagged, training rows)
#' @noRd
.cif_scale <- function(r, use) {
  s <- stats::mad(r[use], na.rm = TRUE)
  if (!is.finite(s) || s <= 0) s <- stats::sd(r[use], na.rm = TRUE)
  if (!is.finite(s) || s <= 0) s <- 1
  s
}

#' Relative cross-fitted probability of the observed level of a categorical cell
#' @noRd
.cif_rho <- function(prob, y) {
  idx <- match(as.character(y), colnames(prob))
  p_obs <- prob[cbind(seq_len(nrow(prob)), idx)]
  p_max <- apply(prob, 1, function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE))
  rho <- p_obs / p_max
  rho[!is.finite(rho)] <- NA_real_
  rho
}

#' Most frequent level (ties: first), NA ignored
#' @noRd
.cif_mode <- function(v) {
  tab <- table(v, useNA = "no")
  names(tab)[which.max(tab)]
}

#' Fit one ranger forest for a column; out-of-bag predictions for the training rows
#' @return list(oob_pred, oob_prob, levels, rf, predict, center, predict_prob)
#' @noRd
.cif_fit_ranger <- function(y, X, aggregate = "median", residuals = "oob", num.trees = 500,
                            mtry = NULL, min.node.size = 5, num.threads = NULL,
                            quantreg = FALSE, ...) {
  df <- X
  df[["..y.."]] <- y
  args <- list(dependent.variable.name = "..y..", data = df, num.trees = num.trees,
               mtry = mtry, min.node.size = min.node.size, num.threads = num.threads,
               respect.unordered.factors = "order", ...)
  if (is.factor(y)) {
    lev <- levels(y)
    rf <- do.call(ranger::ranger, c(args, list(probability = TRUE)))
    as_prob <- function(pr) {
      out <- matrix(0, nrow(pr), length(lev), dimnames = list(NULL, lev))
      common <- intersect(colnames(pr), lev)
      out[, common] <- pr[, common, drop = FALSE]
      out[!is.finite(out)] <- NA_real_
      out
    }
    predict_prob <- function(newX) {
      as_prob(stats::predict(rf, data = newX, num.threads = num.threads)$predictions)
    }
    prob <- if (residuals == "insample") predict_prob(X) else as_prob(rf$predictions)
    return(list(oob_pred = NULL, oob_prob = prob, levels = lev, rf = rf,
                predict = function(newX) {
                  pr <- predict_prob(newX)
                  pr[is.na(pr)] <- 0
                  factor(lev[max.col(pr, ties.method = "first")], levels = lev)
                },
                center = NULL, predict_prob = predict_prob))
  }
  rf <- do.call(ranger::ranger, c(args, list(quantreg = quantreg)))
  center <- function(newX) stats::predict(rf, data = newX, num.threads = num.threads)$predictions
  pred_fun <- function(newX) {
    if (aggregate == "median") {
      pa <- stats::predict(rf, data = newX, predict.all = TRUE,
                           num.threads = num.threads)$predictions
      apply(pa, 1, stats::median)
    } else {
      center(newX)
    }
  }
  oob_pred <- if (residuals == "insample") center(X) else rf$predictions
  oob_pred[!is.finite(oob_pred)] <- NA_real_
  list(oob_pred = oob_pred, oob_prob = NULL, levels = NULL, rf = rf, predict = pred_fun,
       center = center, predict_prob = NULL)
}
