# Opt-in paired smoke benchmark after installing VIM (or loading with pkgload).
# Rscript inst/benchmarks/transformer.R /path/to/results core|neural|foundation
library(VIM)
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile("vim-benchmark-")
mode <- if (length(args)>1L) match.arg(args[[2L]],c("core","neural","foundation")) else "core"
# Single incomplete numeric target permits a fair legacy-token baseline.
truth <- VIM:::.transformer_benchmark_data(80L)[c("x","y")]
methods <- list(
  knn = function(x) kNN(x, imp_var = FALSE),
  ranger = function(x) vimpute(x,method="ranger",imp_var=FALSE,verbose=FALSE),
  xgboost = function(x) vimpute(x,method="xgboost",imp_var=FALSE,verbose=FALSE)
)
if(mode=="neural") {
  keras::py_require_legacy_keras(FALSE)
  token <- function(head_dim,pooling) list(
    fit=function(x) fit_transformer(x,"y",seed=1,epochs=2,verbose=0,
      embedding_dim=32,num_heads=4,feed_forward_dim=64,dense_dim=32,
      head_dim=head_dim,pooling=pooling),
    predict=function(model,x) predict(model,x))
  methods$token_legacy <- token(NULL,"legacy")
  methods$token_compact <- token("compact","legacy")
  methods$token_masked <- token("compact","masked")
  methods$direct <- list(fit=function(x) fit_tabular_transformer(x,"y",seed=1,epochs=2),
                         predict=function(model,x) predict(model,x))
  methods$shared <- list(fit=function(x) fit_shared_transformer(x,variables="y",seed=1,epochs=2),
                         predict=function(model,x) predict(model,x))
}
if(mode=="foundation") {
  checkpoint <- Sys.getenv("VIM_TABIMPUTE_CHECKPOINT")
  pfn_path <- Sys.getenv("VIM_TABPFN_REGRESSOR")
  # Construction stays inside the measured call; cold initialization is included.
  methods$tabimpute <- function(x) tabimpute(x,imp_var=FALSE,
    tabimpute_module=tabimpute_backend(checkpoint_path=checkpoint,allow_download=FALSE))
  methods$tabicl <- function(x) tabiclImpute(x,n_estimators=1,n_iterations=1,imp_var=FALSE)
  methods$tabpfn <- function(x) tabpfnImpute(x,n_estimators=1,regressor_path=pfn_path,imp_var=FALSE)
}
report <- VIM:::.transformer_benchmark(truth, methods, seeds=1:2,
                                      mechanisms=c("MCAR","MAR"),output=output)
print(report$results)
cat("Saved reproducible masks, truth, metadata and results to",output,"\n")
