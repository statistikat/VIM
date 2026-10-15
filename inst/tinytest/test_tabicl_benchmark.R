if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                 "paradox", "R6", "future"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed")
if (!at_home()) exit_file("benchmark integration runs at home")
library(VIM)
e <- new.env(); sys.source(system.file("benchmarks", "tabicl2.R", package = "VIM"), e)
config <- e$uci_config()
expect_equal(config$n, c(250L, 500L, 1000L, 2000L))
expect_equal(config$repetitions, 3L)
expect_equal(config$methods, c("kNN", "ranger", "ranger_seq10", "xgboost", "xgboost_seq10", "vimpute_tabicl2",
 "vimpute_tabicl2_finetune", "vimpute_tabicl2_seq10", "vimpute_tabicl2_finetune_seq10"))
expect_error(e$uci_validate_sizes(c(50L, 50L)), "distinct")
expect_error(e$uci_methods(utils::modifyList(config, list(methods = "transformerImpute"))), "Unknown")
# Deterministic API double lets all nine methods run without model downloads.
reg <- function(...) {
  a <- new.env()
  a$fit <- function(x, y) { a$mean <- mean(y); invisible(a) }
  a$predict <- function(x) rep(a$mean, nrow(x))
  a
}
clf <- function(...) {
  a <- new.env()
  a$fit <- function(x, y) { a$classes_ <- sort(unique(y)); invisible(a) }
  a$predict_proba <- function(x) matrix(1 / length(a$classes_), nrow(x), length(a$classes_))
  a
}
calls <- new.env()
wrap <- function(fun, kind) {
  force(fun); force(kind)
  function(...) {
    calls$kind <- c(calls$kind, kind)
    calls$args[[length(calls$args) + 1L]] <- list(...)
    fun(...)
  }
}
config$tabicl2$tabicl_module <- list(
  TabICLRegressor = wrap(reg, "pretrained"),
  TabICLClassifier = wrap(clf, "pretrained"),
  FinetunedTabICLRegressor = wrap(reg, "finetuned"),
  FinetunedTabICLClassifier = wrap(clf, "finetuned"))
config$trees <- 10L; config$boosting_rounds <- 2L
set.seed(1)
truth <- data.frame(age = runif(60, 18, 70), y = rnorm(60), g = factor(rep(c("a", "b"), 30)))
d <- makeMissing(truth, vars = c("y", "g"), prop = .2, seed = 2)
mask <- attr(d, "where")
methods <- e$uci_methods(config)
for (method in names(methods)) {
  calls$kind <- character(); calls$args <- list()
  ans <- methods[[method]](d, c("y", "g"), 3L)
  scores <- e$uci_score(truth, ans, mask, c("y", "g"))
  expect_equal(nrow(scores), 2L, info = method)
  expect_false(anyNA(ans), info = method)
  if (startsWith(method, "vimpute_tabicl2")) {
    fine <- grepl("finetune", method, fixed = TRUE)
    expect_true(all(calls$kind == if (fine) "finetuned" else "pretrained"), info = method)
    if (endsWith(method, "seq10")) expect_true(length(calls$kind) > 2L, info = method)
    else expect_equal(length(calls$kind), 2L, info = method)
    if (fine) expect_true(all(vapply(calls$args, function(a)
      identical(a$epochs, 5L) && identical(a$n_estimators_inference, 8L), logical(1))))
  }
}
bad <- truth; bad$age[1] <- 0
expect_error(e$uci_score(truth, bad, mask, c("y", "g")), "Changed observed")

# Auxiliary hurdle probabilities may be missing at observed rows; they are not
# imputation targets. Missing target values must still fail scoring.
aux <- truth
aux$y_zero_prob <- NA_real_
expect_identical(e$uci_prediction_table(aux, truth, c("y", "g")), truth)
aux$unexpected <- 1
expect_error(e$uci_prediction_table(aux, truth, c("y", "g")), "schema")
bad <- truth; bad$y[1] <- NA_real_
expect_error(e$uci_score(truth, bad, matrix(TRUE, nrow(truth), ncol(truth), dimnames = list(NULL, names(truth))), c("y", "g")), "Invalid completion")
