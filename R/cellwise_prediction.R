# Prediction safeguards use training statistics only, never a prediction batch.
# Gross marginal anomalies are replaced internally; caller data stay unchanged.
.cellwise_predictor_guard <- function(data, features, enabled, cutoff) {
  if (!enabled) return(NULL)
  numeric <- features[vapply(data[features], is.numeric, logical(1))]
  lapply(stats::setNames(numeric, numeric), function(v) {
    x <- data[[v]]
    list(center = stats::median(x), scale = .robust_scale(x), cutoff = cutoff)
  })
}

.cellwise_guard_predictors <- function(data, guard) {
  for (v in names(guard)) {
    g <- guard[[v]]
    bad <- !is.na(data[[v]]) & abs(data[[v]] - g$center) > g$cutoff * g$scale
    data[[v]][bad] <- g$center
  }
  data
}

.cellwise_regression_response <- function(model, data) {
  if (!is.null(model$constant)) return(rep(model$constant, nrow(data)))
  data <- .cellwise_guard_predictors(data, model$predictor_guard)
  mf <- stats::model.frame(model$terms, data, xlev = model$xlevels,
                           na.action = stats::na.pass)
  X <- stats::model.matrix(model$terms, mf, contrasts.arg = model$contrasts)
  if (anyNA(X)) stop("Cellwise prediction requires complete predictors.")
  if (model$engine %in% c("shooting-s", "irwls")) {
    as.numeric(X %*% model$coefficients)
  } else {
    newdata <- as.data.frame(X[, -1L, drop = FALSE])
    names(newdata) <- model$design_names
    as.numeric(stats::predict(model$fit, newdata = newdata))
  }
}

.cellwise_calibrate_donors <- function(model, y, fitted, cutoff) {
  residuals <- y - fitted
  center <- stats::median(residuals)
  scale <- stats::mad(residuals)
  if (scale < .Machine$double.eps) scale <- stats::IQR(residuals) / 1.349
  # A numerical floor permits exact fits without admitting large residuals.
  scale <- max(scale, sqrt(.Machine$double.eps) * max(1, stats::median(abs(y))))
  model$donor_guard <- list(center = center, scale = scale, cutoff = cutoff)
  model
}

# Graph preprocessing nodes may also have a `model` entry. Search for the
# cellwise marker rather than accepting the first preprocessing model.
.cellwise_model <- function(learner) {
  find <- function(state, depth = 0L) {
    if (!is.list(state) || is.data.frame(state) || depth > 8L) return(NULL)
    if ("donor_guard" %in% names(state)) return(state)
    if (inherits(state, "learner_state")) return(find(state$model, depth + 1L))
    for (value in state) {
      result <- find(value, depth + 1L)
      if (!is.null(result)) return(result)
    }
    NULL
  }
  find(learner$model)
}

# Uses the fitted (possibly bootstrapped/pipelined) learner's calibration.
# The donor scores must be on the original target scale, as y is.
.cellwise_valid_donors <- function(learner, y, scores) {
  model <- .cellwise_model(learner)
  guard <- if (is.list(model)) model$donor_guard else NULL
  valid <- is.finite(y) & is.finite(scores)
  if (is.null(guard) || is.infinite(guard$cutoff)) return(valid)
  valid & abs(y - scores - guard$center) <= guard$cutoff * guard$scale
}
