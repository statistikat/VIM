# Internal registration, called by register_cellwise_learners().
.register_cellwise_classifier <- function() {
  learner <- R6::R6Class("LearnerClassifCellwise",
    inherit = mlr3::LearnerClassif,
    public = list(initialize = function() {
      ps <- paradox::ps(
        init_weights = paradox::p_fct(c("mcd", "univariate"), default = "mcd", tags = "train"),
        weight_function = paradox::p_fct(c("tukey", "huber"), default = "tukey", tags = "train"),
        alpha = paradox::p_dbl(lower = .Machine$double.eps, tags = "train"),
        maxit = paradox::p_int(lower = 1L, default = 200L, tags = "train"),
        decay = paradox::p_dbl(lower = 0, default = 0, tags = "train"),
        MaxNWts = paradox::p_int(lower = 1L, default = 10000L, tags = "train")
      )
      ps$values <- list(init_weights = "mcd", weight_function = "tukey",
                        maxit = 200L, decay = 0, MaxNWts = 10000L)
      super$initialize(id = "classif.cellwise", param_set = ps,
        feature_types = c("numeric", "integer", "factor", "ordered"),
        predict_types = c("response", "prob"),
        properties = c("twoclass", "multiclass", "featureless"),
        packages = c("VIM", "nnet", "robustbase"),
        man = "VIM::register_cellwise_learners")
    }),
    private = list(
      .train = function(task) {
        pv <- self$param_set$get_values(tags = "train")
        if (!is.null(pv$alpha) && (!is.finite(pv$alpha) || pv$alpha <= 0))
          stop("alpha must be finite and positive.", call. = FALSE)
        original <- task$feature_names
        data <- as.data.frame(task$data(cols = c(task$target_names, original)))
        names(data) <- .cw_safe_names(ncol(data))
        target <- names(data)[1L]
        if (anyNA(data)) stop("Cellwise training requires complete predictors and response.")
        sanitized <- sanitize_model_features(data, target, names(data)[-1L])
        data <- sanitized$data
        features <- sanitized$features
        numeric_features <- features[vapply(data[features], is.numeric, logical(1))]
        row_weights <- rep(1, nrow(data))
        if (length(numeric_features)) {
          weight_fun <- if (pv$init_weights == "mcd") cellWeightsMCD else cellWeights
          W <- weight_fun(as.matrix(data[numeric_features]),
                          method = pv$weight_function, alpha = pv$alpha)
          row_weights <- exp(rowMeans(log(pmax(W, 1e-10))))
        }
        classes <- task$class_names
        counts <- vapply(classes, function(cl) sum(row_weights[data[[target]] == cl]), numeric(1))
        prior <- counts / sum(counts)
        model <- list(features = original, kept_features = features,
                      classes = classes, factor_levels = sanitized$factor_levels,
                      row_weights = row_weights, prior = prior)
        if (sum(counts > 0) == 1L || !length(features)) return(model)
        # Store weights in the model frame, so multinom's nonstandard
        # evaluation resolves them independently of the formula environment.
        data$.cw_row_weights <- row_weights
        formula <- stats::reformulate(features, response = target)
        model$fit <- nnet::multinom(formula, data = data,
          weights = .cw_row_weights, trace = FALSE, maxit = pv$maxit,
          decay = pv$decay, MaxNWts = pv$MaxNWts)
        if (model$fit$convergence != 0L)
          warning("Cellwise multinomial fit did not converge; consider increasing maxit.")
        model
      },
      .predict = function(task) {
        model <- self$model
        n <- task$nrow
        prob <- matrix(0, n, length(model$classes), dimnames = list(NULL, model$classes))
        if (is.null(model$fit)) {
          prob[,] <- rep(model$prior, each = n)
        } else {
          data <- as.data.frame(task$data(cols = model$features))
          names(data) <- .cw_safe_names(length(model$features) + 1L)[-1L]
          for (v in names(model$factor_levels)) {
            values <- as.character(data[[v]])
            if (any(!is.na(values) & !values %in% model$factor_levels[[v]]))
              stop("Unseen factor levels in cellwise prediction: ", v)
            data[[v]] <- factor(values, levels = model$factor_levels[[v]])
          }
          if (anyNA(data[model$kept_features]))
            stop("Cellwise prediction requires complete predictors.")
          p <- stats::predict(model$fit, newdata = data, type = "probs")
          lev <- model$fit$lev
          if (length(lev) == 2L) {
            p <- cbind(1 - as.numeric(p), as.numeric(p))
          } else if (is.null(dim(p))) {
            p <- matrix(p, nrow = n, byrow = TRUE)
          }
          prob[, lev] <- p
        }
        if (any(!is.finite(prob)) || any(rowSums(prob) <= 0))
          stop("Cellwise multinomial model returned invalid probabilities.")
        prob <- prob / rowSums(prob)
        list(response = factor(model$classes[max.col(prob, ties.method = "first")],
                               levels = model$classes), prob = prob)
      }
    ))
  mlr3::mlr_learners$add("classif.cellwise", learner)
  invisible(NULL)
}
