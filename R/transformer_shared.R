#' Fit a shared masked-cell transformer
#'
#' Experimental fixed-schema model with one shared encoder and separate numeric
#' or categorical target heads. Training alternates cell and block masks over
#' observed labels only; validation masks stay fixed. Each target's loss is
#' averaged over its masked observations. Predictions are single imputations.
#' @param data Training data frame.
#' @param variables Target columns, defaulting to all non-ID columns.
#' @param id_vars Columns excluded from both training and prediction.
#' @param seed Optional seed.
#' @param validation_fraction Fraction of rows held out for validation.
#' @param mask_rate Artificial training/validation missing fraction.
#' @param embedding_dim Embedding width.
#' @param num_heads Attention heads.
#' @param epochs Maximum epochs.
#' @param batch_size Batch size.
#' @param patience Early stopping patience.
#' @param verbose Backend verbosity.
#' @return A reusable `vim_transformer_model`. `predict()` fills original
#'   missing cells in selected variables in one simultaneous pass.
#' @export
fit_shared_transformer <- function(data,variables=setdiff(names(data),id_vars),id_vars=NULL,
    seed=NULL,validation_fraction=.2,mask_rate=.3,embedding_dim=32L,num_heads=4L,
    epochs=20L,batch_size=64L,patience=3L,verbose=0L) {
  .transformer_head_dim("compact",embedding_dim,num_heads)
  if (!is.data.frame(data) || anyDuplicated(names(data)) || nrow(data)<10L) stop("Need at least ten rows with unique column names")
  if (!length(variables) || anyDuplicated(variables) || any(!c(variables,id_vars) %in% names(data)) ||
      any(variables %in% id_vars)) stop("Invalid variables or id_vars")
  for (x in list(mask_rate,validation_fraction))
    if (length(x)!=1L || !is.finite(x) || x<=0 || x>=1) stop("Fractions must lie between zero and one")
  for (x in list(epochs,batch_size,patience))
    if(length(x)!=1L || !is.finite(x) || x<1 || x!=floor(x)) stop("Training controls must be positive integers")
  data <- as.data.frame(data)[,setdiff(names(data),id_vars),drop=FALSE]
  .transformer_seed(seed, {
    va <- sample.int(nrow(data),max(2L,min(nrow(data)-3L,round(nrow(data)*validation_fraction))))
    tr <- setdiff(seq_len(nrow(data)),va)
    schema <- .tabular_fit_schema(data[tr,,drop=FALSE])
    encoded <- .tabular_encode(data,schema)
    observed <- !is.na(data[variables])
    if (any(colSums(observed[tr,,drop=FALSE])==0) || any(colSums(observed[va,,drop=FALSE])==0))
      stop("Each target needs observed labels in both partitions")
    labels <- lapply(variables,function(v) {
      s <- schema[[v]]; x <- data[[v]]
      y <- if(s$numeric) (x-s$center)/s$scale else match(as.character(x),s$levels)-1L
      if(any(is.na(y) & !is.na(x))) stop("Target category absent from training: ",v)
      y[is.na(y)] <- 0; as.numeric(y)
    })
    categorical <- lapply(schema[variables],function(s) !s$numeric)
    units <- lapply(schema[variables],function(s) if(s$numeric) 1L else length(s$levels))
    cards <- lapply(schema,function(s) if(s$numeric) 0L else length(s$levels)+2L)
    if(!is.null(seed)) tensorflow::tf$random$set_seed(as.integer(seed))
    fitted <- .transformer_layers()$fit_shared(encoded,unname(labels),observed,unname(cards),
      as.list(as.integer(match(variables,names(data))-1L)),unname(units),unname(categorical),
      as.integer(tr-1L),as.integer(va-1L),as.integer(embedding_dim),as.integer(num_heads),
      as.integer(epochs),as.integer(batch_size),as.integer(patience),mask_rate,
      if(is.null(seed)) NULL else as.integer(seed),as.integer(verbose))
    structure(list(model=fitted$model,format_version=1L,kind="shared",target=variables,
      schema=schema,architecture=list(version=4L),
      provenance=list(seed=seed,train_rows=tr,validation_rows=va,mask_rate=mask_rate),
      history=fitted$validation_loss,validation_mask=fitted$validation_mask),class="vim_transformer_model")
  })
}

.predict_shared_transformer <- function(object,newdata,imp_col=FALSE) {
  if(!is.data.frame(newdata) || !all(names(object$schema) %in% names(newdata))) stop("Missing model columns")
  out <- if(data.table::is.data.table(newdata)) data.table::copy(newdata) else newdata
  mask <- is.na(as.data.frame(newdata)[object$target])
  if(!any(mask)) return(out)
  encoded <- .tabular_encode(as.data.frame(newdata)[names(object$schema)],object$schema)
  predictions <- predict(object$model,encoded,verbose=0L)
  if(!is.list(predictions)) predictions <- list(predictions)
  for(k in seq_along(object$target)) {
    v <- object$target[k]; s <- object$schema[[v]]; p <- predictions[[k]]
    if(any(!is.finite(p))) stop("Non-finite shared-model prediction")
    values <- if(s$numeric) as.numeric(p)*s$scale+s$center else s$levels[max.col(p,ties.method="first")]
    if(identical(s$class,"integer")) values <- as.integer(round(values))
    if(identical(s$class,"logical")) values <- values=="TRUE"
    if(is.factor(out[[v]]) && any(!values[mask[,k]] %in% levels(out[[v]]))) stop("Target omits trained factor levels")
    out[[v]][mask[,k]] <- values[mask[,k]]
    if(imp_col) out[[paste0(v,"_imp")]] <- mask[,k]
  }
  out
}
