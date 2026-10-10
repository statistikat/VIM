#' Fit a transformer with direct tabular feature representations
#'
#' Experimental alternative to digit-token prediction. Numeric predictors use
#' training-partition scaling and explicit missing indicators; categories use
#' embeddings with separate missing and unknown codes. Numeric targets are
#' predicted directly. This produces single imputations, not inferential draws.
#' @param data Training data frame with numeric, factor, character or logical columns.
#' @param target Single target column.
#' @param predictors Predictor columns; defaults to all columns except target.
#' @param seed Optional seed.
#' @param validation_fraction Fraction of observed target rows held out for validation.
#' @param embedding_dim Token embedding dimension.
#' @param num_heads Number of attention heads; must divide embedding_dim.
#' @param epochs Maximum training epochs.
#' @param batch_size Training batch size.
#' @param patience Early stopping patience.
#' @param loss Numeric loss, 'mse' or 'huber'.
#' @param learning_rate Adam learning rate.
#' @param verbose Keras verbosity.
#' @return A reusable `vim_transformer_model`. Use `predict()`,
#'   [save_transformer()] and [load_transformer()].
#' @export
fit_tabular_transformer <- function(data,target,predictors=setdiff(names(data),target),
    seed=NULL,validation_fraction=.2,embedding_dim=32L,num_heads=4L,
    epochs=20L,batch_size=64L,patience=3L,loss=c("mse","huber"),learning_rate=.001,verbose=0L) {
  loss <- match.arg(loss)
  .transformer_head_dim("compact",embedding_dim,num_heads)
  if (!is.data.frame(data) || anyDuplicated(names(data)) || length(target)!=1L || !target %in% names(data))
    stop("data must have unique names and contain target")
  if (!length(predictors) || anyDuplicated(predictors) || target %in% predictors ||
      any(!predictors %in% names(data))) stop("Invalid predictors; target cannot predict itself")
  if (length(validation_fraction)!=1L || !is.finite(validation_fraction) ||
      validation_fraction<=0 || validation_fraction>=1) stop("validation_fraction must be between zero and one")
  for (value in list(epochs,batch_size,patience))
    if (length(value)!=1L || !is.finite(value) || value<1 || value!=floor(value)) stop("Training controls must be positive integers")
  if (length(learning_rate)!=1L || !is.finite(learning_rate) || learning_rate<=0) stop("Invalid learning_rate")
  data <- as.data.frame(data)[,c(predictors,target),drop=FALSE]
  categorical <- !is.numeric(data[[target]])
  observed <- which(!is.na(data[[target]]))
  if (length(observed)<5L) stop("At least five observed target values are required")
  .transformer_seed(seed, {
    validation <- sample(observed,max(1L,min(length(observed)-3L,round(length(observed)*validation_fraction))))
    train <- setdiff(observed,validation)
    schema <- .tabular_fit_schema(data[train,,drop=FALSE])
    # Factor levels describe the declared schema, never target values from validation.
    if (categorical && any(!as.character(data[[target]][validation]) %in% schema[[target]]$levels))
      stop("Validation target contains a class absent from training; use more data or another split")
    encoded <- .tabular_encode(data[predictors],schema[predictors])
    y <- if (categorical) match(as.character(data[[target]]),schema[[target]]$levels)-1L else
      (data[[target]]-schema[[target]]$center)/schema[[target]]$scale
    if (!is.null(seed)) tensorflow::tf$random$set_seed(as.integer(seed))
    cards <- lapply(schema[predictors],function(s) if (s$numeric) 0L else length(s$levels)+2L)
    model <- .transformer_layers()$build_tabular(unname(cards),
      as.integer(if(categorical) length(schema[[target]]$levels) else 1L), categorical,
      as.integer(embedding_dim),as.integer(num_heads),loss,learning_rate)
    history <- keras::fit(model,encoded[train,,drop=FALSE],y[train],epochs=as.integer(epochs),
      batch_size=as.integer(batch_size),verbose=as.integer(verbose),
      validation_data=list(encoded[validation,,drop=FALSE],y[validation]),
      callbacks=list(keras::callback_early_stopping(monitor="val_loss",patience=as.integer(patience),restore_best_weights=TRUE)))
    structure(list(model=model,format_version=1L,kind="tabular",target=target,predictors=predictors,
      schema=schema,architecture=list(version=3L,embedding_dim=embedding_dim,num_heads=num_heads),
      provenance=list(seed=seed,train_rows=train,validation_rows=validation,loss=loss),
      history=history$metrics),class="vim_transformer_model")
  })
}

.tabular_fit_schema <- function(data) {
  lapply(data,function(x) {
    numeric <- is.numeric(x) && !inherits(x,c("Date","POSIXt"))
    if (!numeric && !is.factor(x) && !is.character(x) && !is.logical(x)) stop("Unsupported column type")
    observed <- x[!is.na(x)]
    if (!length(observed)) stop("Cannot fit an all-missing column")
    if (numeric && any(!is.finite(observed))) stop("Non-finite numeric value")
    scale <- if (numeric) stats::sd(observed) else 1
    if (!is.finite(scale) || scale==0) scale <- 1
    list(numeric=numeric,class=class(x),center=if(numeric) mean(observed) else 0,scale=scale,
      levels=if(numeric) NULL else if(is.factor(x)) levels(x) else sort(unique(as.character(observed))))
  })
}

.tabular_encode <- function(data,schema) {
  out <- matrix(0,nrow(data),2L*length(schema))
  for (j in seq_along(schema)) {
    s <- schema[[j]]; x <- data[[names(schema)[j]]]
    if (is.null(x) || !identical(class(x),s$class)) stop("Predictor schema mismatch: ",names(schema)[j])
    missing <- is.na(x)
    if (s$numeric) {
      if (any(!is.finite(x[!missing]))) stop("Non-finite predictor")
      z <- (x-s$center)/s$scale; z[missing] <- 0
    } else {
      z <- match(as.character(x),s$levels)+1L
      z[is.na(z)] <- 1L; z[missing] <- 0L
    }
    out[,2L*j-1L] <- z; out[,2L*j] <- as.integer(missing)
  }
  out
}

.predict_tabular_transformer <- function(object,newdata,imp_col=FALSE) {
  if (!is.data.frame(newdata) || !all(names(object$schema) %in% names(newdata))) stop("Missing model columns")
  target <- object$target; meta <- object$schema[[target]]
  if (!identical(class(newdata[[target]]),meta$class)) stop("Target class differs from training")
  out <- if(data.table::is.data.table(newdata)) data.table::copy(newdata) else newdata
  take <- is.na(out[[target]])
  if (any(take)) {
    encoded <- .tabular_encode(as.data.frame(newdata)[take,object$predictors,drop=FALSE],object$schema[object$predictors])
    p <- predict(object$model,encoded,verbose=0L)
    if (any(!is.finite(p))) stop("Non-finite model predictions")
    values <- if(meta$numeric) as.numeric(p)*meta$scale+meta$center else meta$levels[max.col(p,ties.method="first")]
    if (identical(meta$class,"integer")) values <- as.integer(round(values))
    if (identical(meta$class,"logical")) values <- values=="TRUE"
    if (is.factor(out[[target]]) && any(!values %in% levels(out[[target]]))) stop("New target omits trained factor levels")
    out[[target]][take] <- values
  }
  if (imp_col) out$imp <- as.integer(take)
  out
}
