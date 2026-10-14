if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                 "paradox", "R6", "future"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed")
if (!at_home()) exit_file("benchmark integration runs at home")
library(VIM)
e <- new.env(); sys.source(system.file("benchmarks", "tabicl2.R", package = "VIM"), e)
config <- e$uci_config()
expect_equal(config$methods, c("kNN", "ranger", "ranger_seq10", "xgboost", "xgboost_seq10", "vimpute_tabicl2"))
expect_error(e$uci_validate_sizes(c(50L, 50L)), "distinct")
expect_error(e$uci_methods(utils::modifyList(config, list(methods = "transformerImpute"))), "Unknown")
# Deterministic API double lets all six methods run without model downloads.
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
config$tabicl2$tabicl_module <- list(TabICLRegressor = reg, TabICLClassifier = clf)
config$trees <- 10L; config$boosting_rounds <- 2L
set.seed(1)
truth <- data.frame(age = runif(60, 18, 70), y = rnorm(60), g = factor(rep(c("a", "b"), 30)))
d <- makeMissing(truth, vars = c("y", "g"), prop = .2, seed = 2)
mask <- attr(d, "where")
methods <- e$uci_methods(config)
for (method in names(methods)) {
  ans <- methods[[method]](d, c("y", "g"), 3L)
  scores <- e$uci_score(truth, ans, mask, c("y", "g"))
  expect_equal(nrow(scores), 2L, info = method)
  expect_false(anyNA(ans), info = method)
}
bad <- truth; bad$age[1] <- 0
expect_error(e$uci_score(truth, bad, mask, c("y", "g")), "Changed observed")
