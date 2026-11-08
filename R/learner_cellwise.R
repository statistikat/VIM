#' Register cellwise regression and classification learners for mlr3
#'
#' Registers `regr.cellwise` and `classif.cellwise`, used by `vimpute(method = "cellwise")`.
#' Training uses only the rows supplied by mlr3; prediction never refits the
#' model or uses the response. Predictors must be complete (vimpute supplies
#' its current completed predictors). Numeric, integer and factor predictors
#' are supported. Constant predictors are removed. Predictor levels in the
#' task schema but absent from training rows map internally to the training
#' mode; set `new_levels = "error"`
#' for strict rejection. Observed input cells are never modified.
#'
#' @details
#' Learner parameters:
#' * `clean_predictors`: `TRUE` (both learners). At prediction, numeric
#'   predictor cells farther than `predictor_cutoff` (4.5) robust scales from
#'   their training median are replaced internally by that median. The scale
#'   is MAD, with IQR and unit-scale fallbacks as in VIM's cell-weight helpers.
#'   Statistics are frozen at training time; factors and caller data are unchanged.
#'   Set `clean_predictors = FALSE` to recover raw-design prediction.
#' * `donor_cutoff`: 4.5 (regression only). In vimpute, PMM (including
#'   `pmm = TRUE`) and midastouch reject observed numeric donors whose residual
#'   exceeds this many training residual scales from the training residual
#'   median. Calibration uses the guarded model predictions, MAD (IQR fallback)
#'   and a numerical scale floor. `Inf` disables donor screening. If no donors
#'   qualify, vimpute warns and retains the point prediction.
#' * `engine`: `"crm"` (default, requires crmReg), `"cellwise-mm"`,
#'   `"shooting-s"`, or `"irwls"`.
#' * `init_weights`: `"mcd"` (default) or `"univariate"`, for cellwise-mm
#'   and irwls. Weights are estimated on the training data only.
#' * `weight_function`: `"tukey"` (default) or `"huber"`, and `alpha`
#'   (optional positive tuning constant), for the initial cell weights and
#'   irwls residual weights.
#' * `max_shooting`: number of shooting iterations (default 5).
#' * `maxit_irwls`, `eps_irwls`: IRWLS iteration limit and tolerance
#'   (defaults 50 and 1e-6).
#'
#' The first three engines reuse the regression code in [imputeCellReg()].
#' Shooting S here denotes that function's contribution-flagging approximation,
#' not a new implementation of the published coordinate-wise S estimator.
#' Cellwise-MM aggregates initial cell weights to row weights for `lmrob`;
#' IRWLS uses [cellIRWLS()] from [imputeCellM()] directly. Dummy columns inherit
#' weight one, while continuous predictors receive cell weights.
#' These are independent per-target fits: they do not carry a shared cell-weight
#' matrix between vimpute sweeps. Robust fitting still uses the original training
#' design and cell weights; the prediction guard protects new predictor cells.
#' The guard is a marginal gross-outlier screen, not conditional cellwise
#' outlier detection. Legitimate tail values may be replaced; raise the cutoff
#' or disable it when extrapolation is intended. Donor screening can narrow
#' the imputation distribution and does not establish proper multiple imputation.
#'
#' The learner returns deterministic responses. Iteration, bootstrap and
#' uncertainty (including PMM) remain the responsibility of vimpute.
#' Categorical targets use weighted multinomial logistic regression via
#' `nnet::multinom()`. The classifier supports `init_weights`,
#' `weight_function` and `alpha` as above, plus `maxit` (200), `decay` (0)
#' and `MaxNWts` (10000). Its row weights are the geometric mean of cell
#' weights of continuous predictors, estimated on training rows only.
#' With no continuous predictors all row weights equal one. It returns
#' class probabilities or the most probable class; vimpute handles sampling.
#' Missing training classes receive probability zero, and a single observed
#' class produces a constant prediction. This does not detect misclassified
#' categorical cells or implement categorical EM. Regression-only parameters
#' such as `engine` should be supplied per numeric target in mixed data.
#'
#' @return Invisibly, `NULL`. The learner is added to `mlr3::mlr_learners`.
#' @examplesIf requireNamespace("mlr3", quietly = TRUE) && requireNamespace("paradox", quietly = TRUE)
#' register_cellwise_learners()
#' learner <- mlr3::lrn("regr.cellwise", engine = "cellwise-mm")
#' task <- mlr3::as_task_regr(mtcars, target = "mpg")
#' learner$train(task)
#' learner$predict(task)
#' @export
register_cellwise_learners <- function() {
  for (pkg in c("mlr3", "paradox", "R6")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Cellwise learners require package '", pkg, "'.", call. = FALSE)
  }
  learner <- R6::R6Class("LearnerRegrCellwise",
    inherit = mlr3::LearnerRegr,
    public = list(initialize = function() {
      ps <- paradox::ps(
        new_levels = paradox::p_fct(c("mode", "error"), default = "mode", tags = "train"),
        clean_predictors = paradox::p_lgl(default = TRUE, tags = "train"),
        predictor_cutoff = paradox::p_dbl(lower = 1, default = 4.5, tags = "train"),
        donor_cutoff = paradox::p_dbl(lower = 1, default = 4.5, tags = "train"),
        engine = paradox::p_fct(c("crm", "cellwise-mm", "shooting-s", "irwls"),
                               default = "crm", tags = "train"),
        init_weights = paradox::p_fct(c("mcd", "univariate"), default = "mcd", tags = "train"),
        weight_function = paradox::p_fct(c("tukey", "huber"), default = "tukey", tags = "train"),
        alpha = paradox::p_dbl(lower = .Machine$double.eps, tags = "train"),
        max_shooting = paradox::p_int(lower = 1L, default = 5L, tags = "train"),
        maxit_irwls = paradox::p_int(lower = 1L, default = 50L, tags = "train"),
        eps_irwls = paradox::p_dbl(lower = .Machine$double.eps, default = 1e-6, tags = "train")
      )
      ps$values <- list(new_levels = "mode", clean_predictors = TRUE, predictor_cutoff = 4.5, donor_cutoff = 4.5,
                        engine = "crm", init_weights = "mcd",
                        weight_function = "tukey", max_shooting = 5L,
                        maxit_irwls = 50L, eps_irwls = 1e-6)
      super$initialize(id = "regr.cellwise", param_set = ps,
        feature_types = c("numeric", "integer", "factor", "ordered"),
        predict_types = "response", properties = "featureless",
        packages = c("VIM", "robustbase"),
        man = "VIM::register_cellwise_learners")
    }),
    private = list(
      .train = function(task) {
        pv <- self$param_set$get_values(tags = "train")
        if ((!is.null(pv$alpha) && (!is.finite(pv$alpha) || pv$alpha <= 0)) ||
            !is.finite(pv$eps_irwls) || pv$eps_irwls <= 0)
          stop("alpha and eps_irwls must be finite and positive.", call. = FALSE)
        if (pv$engine == "crm" && !requireNamespace("crmReg", quietly = TRUE))
          stop("Engine 'crm' requires the crmReg package.", call. = FALSE)
        original <- task$feature_names
        data <- as.data.frame(task$data(cols = c(task$target_names, original)))
        # Positional names avoid formula parsing of arbitrary task column names.
        names(data) <- .cw_safe_names(ncol(data))
        target <- names(data)[1L]
        sanitized <- sanitize_model_features(data, target, names(data)[-1L])
        data <- sanitized$data
        features <- sanitized$features
        if (anyNA(data)) stop("Cellwise training requires complete predictors and response.")
        form <- stats::reformulate(if (length(features)) features else "1")
        mf <- stats::model.frame(form, data)
        terms <- stats::terms(mf)
        design <- stats::model.matrix(terms, mf)
        X <- design[, colnames(design) != "(Intercept)", drop = FALSE]
        y <- data[[target]]
        model <- list(features = original, terms = terms,
                      xlevels = lapply(mf[vapply(mf, is.factor, logical(1))], levels),
                      contrasts = attr(design, "contrasts"), engine = pv$engine,
                      new_levels = pv$new_levels,
                      factor_modes = lapply(data[names(sanitized$factor_levels)],
                        function(x) names(which.max(table(x)))),
                      predictor_guard = .cellwise_predictor_guard(data, features,
                        pv$clean_predictors, pv$predictor_cutoff))
        if (!ncol(X) || length(unique(y)) == 1L) {
          model$constant <- stats::median(y)
          return(.cellwise_calibrate_donors(model, y, rep(model$constant, length(y)),
                                             pv$donor_cutoff))
        }
        # Build cell weights before dummy expansion: categorical contrasts must
        # not enter a continuous MCD scatter estimate.
        W <- matrix(1, nrow(data), ncol(data), dimnames = list(NULL, names(data)))
        numeric_cols <- vapply(data, is.numeric, logical(1))
        if (pv$engine %in% c("cellwise-mm", "irwls")) {
          weight_fun <- if (pv$init_weights == "mcd") cellWeightsMCD else cellWeights
          W[, numeric_cols] <- weight_fun(as.matrix(data[, numeric_cols, drop = FALSE]),
                                          method = pv$weight_function, alpha = pv$alpha)
        }
        assignments <- attr(design, "assign")[-1L]
        WX <- W[, features[assignments], drop = FALSE]
        train <- as.data.frame(cbind(y, X))
        names(train) <- .cw_safe_names(ncol(train))
        model$design_names <- names(train)[-1L]
        args <- list(data = train, j = 1L, pred_cols = seq.int(2L, ncol(train)),
                     miss_j = rep(FALSE, nrow(train)), cn = names(train))
        result <- switch(pv$engine,
          crm = do.call(.engine_crm, args),
          `cellwise-mm` = do.call(.engine_cellwise_mm,
                                 c(args, list(W = cbind(W[, target], WX)))),
          `shooting-s` = do.call(.engine_shooting_s,
                                c(args, list(max_shooting = pv$max_shooting))),
          irwls = cellIRWLS(X, y, w_cell = WX, w_response = W[, target],
                            method = pv$weight_function, alpha = pv$alpha,
                            maxit = pv$maxit_irwls, eps = pv$eps_irwls)
        )
        model$fit <- result$model
        model$coefficients <- result$coefficients
        model$cell_flags <- result$cell_flags
        model$response_weights <- result$response_weights
        .cellwise_calibrate_donors(model, y, .cellwise_regression_response(model, data),
                                   pv$donor_cutoff)
      },
      .predict = function(task) {
        model <- self$model
        data <- as.data.frame(task$data(cols = model$features))
        names(data) <- .cw_safe_names(length(model$features) + 1L)[-1L]
        if (!length(model$features))
          return(list(response = rep(model$constant, task$nrow)))
        list(response = .cellwise_regression_response(model, data))
      }
    ))
  mlr3::mlr_learners$add("regr.cellwise", learner)
  .register_cellwise_classifier()
  invisible(NULL)
}
