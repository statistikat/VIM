# Opt-in integration tests. Run in a configured environment with cached
# checkpoints; routine package checks never download or initialize Python.
if (!identical(Sys.getenv("VIM_TEST_TABICL"), "true"))
  exit_file("set VIM_TEST_TABICL=true to exercise real TabICL 2.2 models")
if (!all(vapply(c("reticulate", "mlr3", "R6", "paradox"),
                requireNamespace, logical(1), quietly = TRUE))) exit_file("optional dependencies absent")
library(VIM)
VIM:::register_tabicl_learners()
reticulate::import("torch")$set_num_threads(1L)
set.seed(13)
d <- data.frame(y = rnorm(32), g = factor(rep(c("a", "b"), 16)), x = rnorm(32))
for (fine in c(FALSE, TRUE)) for (type in c("regr", "classif")) {
  path <- Sys.getenv(if (type == "regr") "VIM_TABICL_REGRESSOR" else "VIM_TABICL_CLASSIFIER")
  if (!file.exists(path)) stop("Set VIM_TABICL_REGRESSOR and VIM_TABICL_CLASSIFIER to cached checkpoints")
  task <- if (type == "regr") mlr3::TaskRegr$new("real", d, target = "y") else
    mlr3::TaskClassif$new("real", d, target = "g")
  l <- mlr3::lrn(paste0(type, ".tabicl2"))
  l$param_set$values <- utils::modifyList(l$param_set$values,
    list(finetune = fine, device = "cpu", n_estimators = 1L,
         model_path = path, allow_auto_download = FALSE, epochs = 1L,
         finetune_params = list(n_estimators_finetune = 1L,
                               n_estimators_validation = 1L, amp = FALSE)))
  l$train(task)
  p <- l$predict(task)
  expect_equal(length(p$response), nrow(d))
  if (type == "regr") expect_true(all(is.finite(p$response))) else {
    expect_true(all(p$response %in% levels(d$g)))
    l$predict_type <- "prob"
    expect_equal(rowSums(l$predict(task)$prob), rep(1, nrow(d)), tolerance = 1e-6)
  }
}
