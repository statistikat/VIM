# Opt-in real-model MCAR experiment. Uses the same budget as the UCI benchmark;
# no routine package check initializes Python or downloads checkpoints.
if (!identical(Sys.getenv("VIM_TEST_TABICL"), "true"))
  exit_file("set VIM_TEST_TABICL=true to run the correlated real-model experiment")
if (!all(vapply(c("reticulate", "mlr3", "R6", "paradox"),
                requireNamespace, logical(1), quietly = TRUE)))
  exit_file("optional dependencies absent")
library(VIM)
e <- new.env()
sys.source(system.file("benchmarks", "tabicl2-correlated.R", package = "VIM"), e)
reticulate::import("torch")$set_num_threads(1L)
out <- Sys.getenv("VIM_TABICL_CORRELATED_RESULTS", "")
x <- e$tabicl_correlated_run(output = if (nzchar(out)) out else NULL)
for (method in names(x$completed)) {
  expect_false(anyNA(x$completed[[method]]))
  expect_true(all(is.finite(x$metrics$rmse[x$metrics$method == method &
    x$metrics$variable %in% c("X", "Y")])))
  calls <- x$fit_calls[[method]]
  fine <- grepl("finetune", method, fixed = TRUE)
  expect_true(all(vapply(calls, function(c)
    startsWith(c$kind, "Finetuned") == fine, logical(1))))
  if (endsWith(method, "seq10")) expect_true(length(calls) > 4L)
  else expect_equal(length(calls), 4L)
}
print(x$metrics, row.names = FALSE)
print(x$differences, row.names = FALSE)
