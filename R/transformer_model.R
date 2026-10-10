#' Fit a reusable transformer imputation model
#'
#' Fits once and stores the preprocessing schema for subsequent predictions.
#' Existing list-based models remain accepted by [transformerImpute()].
#' @param data Training data frame.
#' @param target Name of the target column.
#' @param cat_vars Categorical column names; NULL infers factors, characters,
#'   and logical columns (numeric columns remain numeric).
#' @param hier_cat Optional hierarchical category specification.
#' @param seed Optional non-negative integer seed. R RNG state is restored;
#'   backend/device operations may still be nondeterministic.
#' @param ... Training arguments passed to the transformer training helper.
#' @return A `vim_transformer_model` used with `predict()` or saved to disk.
#' @export
fit_transformer <- function(data, target, cat_vars = NULL, hier_cat = NULL,
                            seed = NULL, ...) {
  if (!is.data.frame(data) || !nrow(data) || anyDuplicated(names(data)))
    stop("data must be a nonempty data frame with unique column names")
  if (length(target) != 1L || !target %in% names(data)) stop("Unknown target")
  args <- list(...)
  if (any(c("data", "target", "cat_vars", "hier_cat", "save_model") %in% names(args)))
    stop("Reserved training argument; use save_transformer() to save a fitted bundle")
  if (!is.null(args$include_cols)) data <- as.data.frame(data)[, args$include_cols, drop=FALSE]
  if (!target %in% names(data)) stop("include_cols must contain target")
  if (is.null(cat_vars)) cat_vars <- names(data)[vapply(data, function(x)
    is.factor(x) || is.character(x) || is.logical(x), logical(1))]
  if (any(!cat_vars %in% names(data))) stop("Unknown categorical column")
  if (sum(!is.na(data[[target]])) < 3L) stop("At least three observed target values are required")
  schema <- lapply(data, function(x) list(class=class(x),
    levels=if (is.factor(x)) levels(x) else if (is.character(x) || is.logical(x)) unique(x[!is.na(x)]) else NULL))
  bundle <- .transformer_seed(seed, {
    if (!is.null(seed)) tensorflow::tf$random$set_seed(as.integer(seed))
    do.call(train_transformer, c(list(data=data, target=target,
      cat_vars=cat_vars, hier_cat=hier_cat), args))
  })
  bundle$format_version <- 1L
  bundle$target <- target
  bundle$schema <- schema
  bundle$options <- utils::modifyList(list(cat_vars=cat_vars, hier_cat=hier_cat,
    decimal_points=1, decimal_points_target=1, round_long_decs=TRUE),
    args[intersect(names(args), c("decimal_points", "decimal_points_target", "round_long_decs"))])
  bundle$target_example <- data[[target]][which(!is.na(data[[target]]))[1L]]
  bundle$provenance <- list(VIM=as.character(utils::packageVersion("VIM")),
    keras=as.character(utils::packageVersion("keras")), seed=seed)
  class(bundle) <- "vim_transformer_model"
  bundle
}

.transformer_validate_schema <- function(object, newdata) {
  if (!is.data.frame(newdata) || anyDuplicated(names(newdata))) stop("newdata must have unique column names")
  required <- names(object$schema)
  if (!all(required %in% names(newdata))) stop("newdata is missing required model columns")
  for (nm in required) {
    meta <- object$schema[[nm]]; x <- newdata[[nm]]
    if (!identical(class(x), meta$class)) stop("Column class differs from training: ", nm)
    if (!is.null(meta$levels) && any(!as.character(x[!is.na(x)]) %in% as.character(meta$levels)))
      stop("Unknown category in column: ", nm)
    if (is.numeric(x) && any(!is.finite(x[!is.na(x)]))) stop("Non-finite values in column: ", nm)
  }
  invisible(TRUE)
}

#' @rdname fit_transformer
#' @param object Fitted transformer model.
#' @param newdata Data to impute; training column classes must be preserved.
#' @param imp_col Add an imputation indicator named `imp`.
#' @export
predict.vim_transformer_model <- function(object, newdata, imp_col=FALSE, seed=NULL, ...) {
  if (length(list(...))) stop("Preprocessing cannot be overridden during prediction")
  if (identical(object$kind,"tabular")) return(.predict_tabular_transformer(object,newdata,imp_col))
  .transformer_validate_schema(object,newdata)
  .transformer_seed(seed, {
    if (!is.null(seed)) tensorflow::tf$random$set_seed(as.integer(seed))
    work <- as.data.frame(newdata)[, names(object$schema), drop=FALSE]
    result <- do.call(transformerImpute, c(list(data=work,target=object$target,
      model=unclass(object), imp_col=imp_col), object$options))
    out <- if (data.table::is.data.table(newdata)) data.table::copy(newdata) else newdata
    take <- is.na(newdata[[object$target]])
    values <- result[[object$target]][take]
    out[[object$target]][take] <- values
    if (imp_col) out[["imp"]] <- as.integer(take)
    out
  })
}

#' Save or load a transformer model bundle
#'
#' Stores the model and preprocessing metadata together. A completion marker
#' detects partial writes. Only load model directories from trusted sources.
#' @param object Fitted transformer model.
#' @param path New directory for saving, or an existing bundle for loading.
#' @return Saving returns the path invisibly; loading returns a fitted model.
#' @export
save_transformer <- function(object,path) {
  if (!inherits(object,"vim_transformer_model")) stop("Not a fitted transformer bundle")
  if (file.exists(path)) stop("Refusing to overwrite an existing path")
  parent <- dirname(path)
  if (!dir.exists(parent)) stop("Parent directory does not exist")
  stage <- tempfile(".vim-model-",tmpdir=parent)
  dir.create(stage); on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  keras::save_model_tf(object$model,file.path(stage,"model"))
  meta <- object; meta$model <- NULL
  saveRDS(meta,file.path(stage,"metadata.rds"))
  writeLines("VIM transformer bundle 1",file.path(stage,"COMPLETE"))
  if (!file.rename(stage,path)) stop("Could not finalize model bundle")
  invisible(path)
}

#' @rdname save_transformer
#' @export
load_transformer <- function(path) {
  if (!file.exists(file.path(path,"COMPLETE")) || !file.exists(file.path(path,"metadata.rds")) ||
      !dir.exists(file.path(path,"model"))) stop("Incomplete transformer model bundle")
  meta <- readRDS(file.path(path,"metadata.rds"))
  if (!inherits(meta,"vim_transformer_model") || !identical(meta$format_version,1L))
    stop("Unsupported transformer bundle format")
  .transformer_layers()
  meta$model <- keras::load_model_tf(file.path(path,"model"))
  meta
}
