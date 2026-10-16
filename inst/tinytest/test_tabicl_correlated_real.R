# Opt-in real-model MCAR experiment with a 20-epoch maximum;
# No routine package check initializes Python or downloads checkpoints.
if (!identical(Sys.getenv("VIM_TEST_TABICL"), "true"))
  exit_file("set VIM_TEST_TABICL=true to run the correlated real-model experiment")
if (!all(vapply(c("reticulate", "mlr3", "R6", "paradox"),
                requireNamespace, logical(1), quietly = TRUE)))
  exit_file("optional dependencies absent")
library(VIM)
e <- new.env()
# Development benchmarks are excluded from installed packages.
benchmark_paths <- file.path(c(".", "..", "../.."), "dev-notes", "benchmarks", "tabicl2-correlated.R")
benchmark_paths <- benchmark_paths[file.exists(benchmark_paths)]
if (!length(benchmark_paths)) exit_file("development benchmark files not available")
sys.source(benchmark_paths[1L], e)
reticulate::import("torch")$set_num_threads(1L)
out <- Sys.getenv("VIM_TABICL_CORRELATED_RESULTS", "")
x <- e$tabicl_correlated_run(output = if (nzchar(out)) out else NULL)
for (method in names(x$completed)) {
  expect_false(anyNA(x$completed[[method]]))
  expect_true(all(is.finite(x$metrics$rmse[x$metrics$method == method &
    x$metrics$variable == "Y"])))
  expect_identical(x$completed[[method]][c("X", "A", "B")],
                   x$truth[c("X", "A", "B")])
  calls <- x$fit_calls[[method]]
  fine <- grepl("finetune", method, fixed = TRUE)
  expect_true(all(vapply(calls, function(c)
    startsWith(c$kind, "Finetuned") == fine, logical(1))))
  if (endsWith(method, "seq10")) expect_true(length(calls) > 1L)
  else expect_equal(length(calls), 1L)
}
print(x$metrics, row.names = FALSE)
print(x$differences, row.names = FALSE)
