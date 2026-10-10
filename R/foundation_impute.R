#' Impute with the optional TabICL foundation model
#'
#' Uses tabicl 2.2.0's unsupervised interface with explicit categorical columns.
#' Numeric columns are never classified as categorical based on cardinality.
#' Models are imported lazily; offline mode requires cached upstream checkpoints.
#' One call shares loaded weights across target columns, with no stale context cache.
#' This is an experimental single-imputation interface.
#' @param data Data frame to impute.
#' @param variables Selected target columns, or NULL for all incomplete columns.
#' @param id_vars Columns excluded from the backend.
#' @param n_estimators Number of ensemble members.
#' @param n_iterations Refinement passes.
#' @param temperature Sampling temperature; near zero gives near-deterministic results.
#' @param seed Optional seed.
#' @param device Backend device, e.g. 'cpu' or 'cuda'.
#' @param allow_download Allow upstream checkpoint downloads; default FALSE.
#' @param python Optional Python executable, selected before Python initialization.
#' @param imp_var Add imputation indicators.
#' @param backend Optional R testing/custom backend accepting x, categorical and seed,
#'   returning a numeric matrix in the same encoded schema.
#' @return Completed data with observed values and column classes preserved.
#' @export
tabiclImpute <- function(data,variables=NULL,id_vars=NULL,n_estimators=4L,n_iterations=2L,
    temperature=1e-8,seed=42L,device="cpu",allow_download=FALSE,python=NULL,
    imp_var=TRUE,backend=NULL) {
  .foundation_controls(n_estimators,n_iterations,temperature,allow_download)
  engine <- if(!is.null(backend)) backend else function(x,categorical,seed) {
    module <- .foundation_python(python)
    module$tabicl_impute(x,as.list(as.integer(categorical)),as.integer(n_estimators),
      as.integer(n_iterations),temperature,if(is.null(seed)) NULL else as.integer(seed),device,allow_download)
  }
  .foundation_apply(data,variables,id_vars,seed,imp_var,engine)
}

.foundation_controls <- function(n_estimators,n_iterations,temperature,allow_download) {
  for(x in list(n_estimators,n_iterations))
    if(length(x)!=1L || !is.numeric(x) || !is.finite(x) || x<1 || x!=floor(x))
      stop("Ensemble and iteration counts must be positive integers")
  if(length(temperature)!=1L || !is.numeric(temperature) || !is.finite(temperature) || temperature<0)
    stop("temperature must be non-negative and finite")
  .tabimpute_validate_scalar_flag(allow_download,"allow_download")
}

.foundation_python <- function(python=NULL) {
  if(!requireNamespace("reticulate",quietly=TRUE)) stop("Install optional R package reticulate")
  if(!is.null(python)) reticulate::use_python(python,required=TRUE)
  reticulate::import_from_path("vim_foundation_backends",
    path=system.file("python",package="VIM"),convert=TRUE)
}

.foundation_apply <- function(data,variables,id_vars,seed,imp_var,engine) {
  if(!is.function(engine)) stop("backend must be an R function")
  adapter <- function(x,mask,seed=NULL,...) {
    codes <- lapply(x,function(z) if(is.character(z) || is.factor(z) || is.logical(z))
      sort(unique(as.character(z[!is.na(z)]))) else NULL)
    categorical <- which(!vapply(codes,is.null,logical(1)))
    encoded <- as.matrix(data.frame(lapply(seq_along(x),function(j) {
      if(j %in% categorical) match(as.character(x[[j]]),codes[[j]])-1L else as.numeric(x[[j]])
    })))
    storage.mode(encoded) <- "double"
    encoded[is.na(encoded)] <- NaN
    result <- engine(encoded,unname(categorical-1L),seed)
    if(!is.matrix(result) || !identical(dim(result),dim(encoded))) stop("Backend changed encoded table dimensions")
    out <- x
    for(j in seq_along(x)) {
      selected <- mask[,j]
      if(!any(selected)) next
      value <- result[selected,j]
      if(any(!is.finite(value))) stop("Backend returned missing/non-finite predictions")
      if(j %in% categorical) {
        if(any(abs(value-round(value))>1e-6 | value<0 | value>=length(codes[[j]])))
          stop("Backend returned an invalid categorical code")
        value <- codes[[j]][as.integer(round(value))+1L]
        if(is.logical(out[[j]])) value <- value=="TRUE"
      }
      out[[j]][selected] <- value
    }
    out
  }
  tabimpute(data,variables=variables,id_vars=id_vars,seed=seed,
    imp_var=imp_var,tabimpute_module=adapter)
}
