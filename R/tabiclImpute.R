#' TabICL 2.2 imputation
#'
#' Impute numeric and categorical targets with pretrained TabICL models through
#' [vimpute()]. Python is initialized lazily when a model is trained. Install
#' `reticulate` and `python -m pip install 'tabicl[finetune]==2.2.0' 'pandas>=2.1.2'` in its
#' Python environment (Python >= 3.10). Checkpoints are downloaded on first use
#' unless `model_path` and `allow_auto_download = FALSE` are supplied in
#' `learner_params`. Each target and iteration fits a separate estimator using
#' only observed target values. By default predictors are passed as a typed
#' pandas data frame: numeric missings remain NaN and factors retain their
#' training-time categorical schema. Python fits and reuses the preprocessing
#' for both training and prediction.
#'
#' `vimpute(method = "tabicl2")` uses the same learners. They support PMM and
#' the other uncertainty mechanisms of `vimpute()`. Fine-tuning adapts pretrained
#' weights on each target's observed data; it may substantially increase runtime.
#' Python model objects must be recreated in each R process.
#'
#' @param data A data.frame or data.table.
#' @param considered_variables Columns used for imputation and prediction, as in
#'   [vimpute()]. Other columns are returned unchanged.
#' @param finetune Whether to train the pretrained model on the observed data.
#' @param learner_params Named TabICL learner parameters. Common parameters are
#'   `device`, `n_estimators`, `random_state`, `model_path`,
#'   `allow_auto_download`, `python`, `epochs`, `learning_rate`, and `patience`.
#'   `input_type` defaults to `"dataframe"`; `"matrix"` retains the legacy
#'   numeric coding of factors. Unknown factor levels become categorical
#'   missings with the default backend, rather than fractional numeric codes.
#'   `epochs` defaults to 5 and sets the maximum fine-tuning epochs; early
#'   stopping may finish sooner. For example, use `learner_params =
#'   list(tabicl2 = list(finetune = TRUE, epochs = 20L))` with [vimpute()],
#'   or `learner_params = list(epochs = 20L)` with `tabiclImpute()`.
#'   `estimator_params` and `finetune_params` are named lists of additional
#'   upstream constructor arguments. `n_estimators` maps to
#'   `n_estimators_inference` for fine-tuning. `tabicl_module` optionally supplies
#'   an API-compatible backend for testing.
#' @param imp_var Add imputation indicator columns.
#' @param ... Additional arguments to [vimpute()], including `seed`, `nseq`,
#'   `sequential`, `predictors`, `uncert`, `pmm`, `edit_rules`, and `edit_control`. The wrapper defaults to one
#'   pass and deterministic predictions (`uncert = "none"`, `pmm = FALSE`).
#' @return The imputed data set, as returned by [vimpute()].
#' @export
#' @examples
#' \dontrun{
#' tabiclImpute(sleep, finetune = TRUE,
#'   learner_params = list(device = "cpu", epochs = 5L))
#' vimpute(sleep, method = "tabicl2",
#'   learner_params = list(tabicl2 = list(finetune = FALSE)))
#' }
tabiclImpute <- function(data, considered_variables = names(data), finetune = FALSE,
                         learner_params = list(), imp_var = TRUE, ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame or data.table")
  if (!is.logical(finetune) || length(finetune) != 1L || is.na(finetune))
    stop("finetune must be TRUE or FALSE")
  variables <- considered_variables
  if (!is.character(variables) || anyNA(variables) ||
      any(!variables %in% names(data))) stop("Unknown or invalid variables")
  if ((!length(variables) || !anyNA(as.data.frame(data)[variables])) &&
      is.null(list(...)$edit_rules)) return(data)
  args <- utils::modifyList(list(sequential = FALSE, nseq = 1L, pmm = FALSE,
                                uncert = "none", verbose = FALSE), list(...))
  do.call(vimpute, c(list(data = data, considered_variables = variables, method = "tabicl2",
    learner_params = list(tabicl2 = utils::modifyList(list(finetune = finetune), learner_params)),
    imp_var = imp_var), args))
}

.tabicl_import <- function(python = NULL, tabicl_module = NULL) {
  if (!is.null(tabicl_module)) return(tabicl_module)
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("TabICL requires the optional R package 'reticulate'.")
  if (!is.null(python)) reticulate::use_python(python, required = TRUE)
  tryCatch({
    version <- reticulate::import("importlib.metadata")$version("tabicl")
    if (utils::compareVersion(version, "2.2.0") < 0L ||
        utils::compareVersion(version, "2.3.0") >= 0L)
      stop("Expected TabICL 2.2.x; found ", version)
    reticulate::import("tabicl", convert = TRUE)
  }, error = function(e) stop("TabICL 2.2 requires Python >= 3.10 and ",
    "'tabicl[finetune]==2.2.0'. Install inst/python/tabicl-requirements.txt ",
    "in reticulate's environment.\n", conditionMessage(e), call. = FALSE))
}

# One fitted encoding is shared by training and all subsequent predictions.
.tabicl_matrix <- function(data, levels) {
  out <- as.matrix(as.data.frame(lapply(names(data), function(nm) {
    x <- data[[nm]]
    if (!is.null(levels[[nm]])) as.numeric(match(as.character(x), levels[[nm]])) - 1
    else as.numeric(x)
  })))
  storage.mode(out) <- "double"
  out[is.na(out)] <- NaN
  out
}

# Preserve one schema across training, prediction and PMM donor scoring.
# R backends used in tests receive the same typed frame without starting Python.
.tabicl_input <- function(data, encoding, input_type = "dataframe", python = FALSE) {
  if (identical(input_type, "matrix")) return(.tabicl_matrix(data, encoding))
  if (!identical(input_type, "dataframe")) stop("Unknown TabICL input_type")
  frame <- as.data.frame(data, stringsAsFactors = FALSE)
  for (nm in names(frame)) {
    if (!is.null(encoding[[nm]])) {
      # Unordered storage avoids reticulate's ordered-factor conversion warning.
      # The original ordered class is restored by vimpute's result builder.
      frame[[nm]] <- factor(as.character(frame[[nm]]), levels = encoding[[nm]])
    } else {
      frame[[nm]] <- as.numeric(frame[[nm]])
    }
  }
  rownames(frame) <- NULL
  if (!isTRUE(python)) return(frame)
  pandas <- tryCatch(reticulate::import("pandas", convert = FALSE),
    error = function(e) stop("TabICL dataframe input requires Python pandas. ",
      "Install inst/python/tabicl-requirements.txt in reticulate's environment.\n",
      conditionMessage(e), call. = FALSE))
  out <- reticulate::r_to_py(frame, convert = FALSE)
  # Set categorical dtype explicitly, including missing/unused/single levels.
  # -1 is pandas' missing-category code; it never becomes a numeric category.
  for (nm in names(frame)) if (!is.null(encoding[[nm]])) {
    codes <- as.integer(frame[[nm]]) - 1L
    codes[is.na(codes)] <- -1L
    categorical <- pandas$Categorical$from_codes(
      as.array(codes), categories = as.list(encoding[[nm]]), ordered = FALSE)
    out$`__setitem__`(nm, categorical)
  }
  out
}

.tabicl_constructor_args <- function(pv) {
  fine <- isTRUE(pv$finetune)
  reserved <- c("finetune", "python", "tabicl_module", "input_type", "estimator_params",
                "finetune_params", "epochs", "learning_rate", "patience")
  args <- pv[setdiff(names(pv), reserved)]
  if (fine) {
    args$n_estimators_inference <- args$n_estimators
    args$n_estimators <- NULL
    args$n_jobs <- NULL
    for (nm in c("epochs", "learning_rate", "patience"))
      if (!is.null(pv[[nm]])) args[[nm]] <- pv[[nm]]
  }
  extra <- if (fine) pv$finetune_params else pv$estimator_params
  if (!is.null(extra) && (!is.list(extra) ||
      (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra)))))))
    stop("estimator_params and finetune_params must be named lists")
  utils::modifyList(args, if (is.null(extra)) list() else extra)
}

.tabicl_train <- function(task, pv, classif) {
  x <- as.data.frame(task$data(cols = task$feature_names))
  encoding <- lapply(x, function(z) if (is.factor(z)) levels(z) else NULL)
  y <- task$truth()
  classes <- if (classif) task$class_names else NULL
  observed <- unique(y)
  if (length(observed) == 1L) return(list(constant = observed[1L], classes = classes))
  module <- .tabicl_import(pv$python, pv$tabicl_module)
  kind <- paste0(if (isTRUE(pv$finetune)) "Finetuned" else "", "TabICL",
                 if (classif) "Classifier" else "Regressor")
  constructor <- module[[kind]]
  if (is.null(constructor)) stop("TabICL backend does not expose ", kind)
  estimator <- do.call(constructor, .tabicl_constructor_args(pv))
  if (classif) y <- as.character(y)
  input_type <- if (is.null(pv$input_type)) "dataframe" else pv$input_type
  python_input <- inherits(estimator, "python.builtin.object")
  estimator$fit(.tabicl_input(x, encoding, input_type, python_input), y)
  list(estimator = estimator, encoding = encoding, features = names(x), classes = classes,
       input_type = input_type, python_input = python_input)
}

.tabicl_predict <- function(task, model, classif, predict_type) {
  n <- task$nrow
  if (!is.null(model$constant)) {
    if (!classif) return(list(response = rep(as.numeric(model$constant), n)))
    prob <- matrix(0, n, length(model$classes), dimnames = list(NULL, model$classes))
    prob[, as.character(model$constant)] <- 1
  } else {
    input_type <- if (is.null(model$input_type)) "matrix" else model$input_type
    x <- .tabicl_input(as.data.frame(task$data(cols = model$features)), model$encoding,
                       input_type, isTRUE(model$python_input))
    if (!classif) {
      response <- as.numeric(model$estimator$predict(x))
      if (length(response) != n || any(!is.finite(response))) stop("Invalid TabICL predictions")
      return(list(response = response))
    }
    raw <- as.matrix(model$estimator$predict_proba(x))
    trained_classes <- as.character(model$estimator$classes_)
    if (nrow(raw) != n || ncol(raw) != length(trained_classes) ||
        any(!is.finite(raw)) || any(raw < 0) || any(raw > 1) ||
        any(abs(rowSums(raw) - 1) > 1e-6) || anyDuplicated(trained_classes) ||
        any(!trained_classes %in% model$classes)) stop("Invalid TabICL probabilities")
    prob <- matrix(0, n, length(model$classes), dimnames = list(NULL, model$classes))
    prob[, trained_classes] <- raw
  }
  if (predict_type == "prob") list(prob = prob)
  else list(response = factor(model$classes[max.col(prob, ties.method = "first")],
                              levels = model$classes))
}

# Register only on use, keeping the optional mlr3 and Python stacks lazy.
register_tabicl_learners <- function() {
  for (type in c("regr", "classif")) {
    local({
      classif <- type == "classif"
      id <- paste0(type, ".tabicl2")
      learner <- R6::R6Class(paste0("Learner", if (classif) "Classif" else "Regr", "TabICL2"),
        inherit = if (classif) mlr3::LearnerClassif else mlr3::LearnerRegr,
        public = list(initialize = function() {
          ps <- paradox::ps(
            finetune = paradox::p_lgl(default = FALSE, tags = "train"),
            n_estimators = paradox::p_int(1L, default = 8L, tags = "train"),
            device = paradox::p_uty(tags = "train"),
            random_state = paradox::p_int(0L, default = 42L, tags = "train"),
            n_jobs = paradox::p_int(1L, tags = "train"),
            model_path = paradox::p_uty(tags = "train"),
            allow_auto_download = paradox::p_lgl(default = TRUE, tags = "train"),
            python = paradox::p_uty(tags = "train"),
            input_type = paradox::p_fct(c("dataframe", "matrix"), default = "dataframe", tags = "train"),
            epochs = paradox::p_int(1L, default = 5L, tags = "train"),
            learning_rate = paradox::p_dbl(lower = 0, tags = "train"),
            patience = paradox::p_int(1L, tags = "train"),
            estimator_params = paradox::p_uty(tags = "train"),
            finetune_params = paradox::p_uty(tags = "train"),
            tabicl_module = paradox::p_uty(tags = "train"))
          super$initialize(id = id, param_set = ps,
            feature_types = c("numeric", "integer", "factor", "ordered", "logical"),
            predict_types = if (classif) c("response", "prob") else "response",
            properties = c("missings", if (classif) c("twoclass", "multiclass")),
            packages = "VIM", man = "VIM::tabiclImpute")
          self$param_set$values <- list(finetune = FALSE, input_type = "dataframe", epochs = 5L, n_estimators = 8L,
            random_state = 42L, allow_auto_download = TRUE)
        }),
        private = list(
          .train = function(task) .tabicl_train(task, self$param_set$get_values(), classif),
          .predict = function(task) .tabicl_predict(task, self$model, classif, self$predict_type)))
      mlr3::mlr_learners$add(id, learner)
    })
  }
  invisible(NULL)
}
