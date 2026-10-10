# Opt-in smoke benchmark. Run after installing VIM:
# Rscript inst/benchmarks/transformer.R /path/to/results
library(VIM)
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile("vim-benchmark-")
truth <- VIM:::.transformer_benchmark_data()
methods <- list(
  knn = function(x) kNN(x, imp_var = FALSE),
  ranger = function(x) rangerImpute(x, imp_var = FALSE),
  xgboost = function(x) xgboostImpute(x, imp_var = FALSE),
  tabimpute = function(x) tabimpute(x, imp_var = FALSE)
)
report <- VIM:::.transformer_benchmark(truth, methods, output = output)
print(report$results)
cat("Saved reproducible masks, truth, metadata and results to", output, "\n")
