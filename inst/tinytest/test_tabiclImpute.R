if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                 "paradox", "R6", "future"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed")
library(VIM)

# API-compatible doubles exercise the real mlr3 and vimpute paths without
# downloading weights or making routine checks depend on Python/GPU availability.
calls <- new.env(parent = emptyenv())
calls$fit <- list(); calls$args <- list(); calls$kind <- character()
constructor <- function(kind) {
  force(kind)
  function(...) {
    calls$args[[length(calls$args) + 1L]] <- list(...)
    calls$kind <- c(calls$kind, kind)
    model <- new.env()
    model$fit <- function(x, y) {
      calls$fit[[length(calls$fit) + 1L]] <- list(x = x, y = y)
      model$mean <- if (is.numeric(y)) mean(y) else NULL
      # Deliberately reverse Python's class order to test alignment.
      model$classes_ <- if (is.character(y)) rev(sort(unique(y))) else NULL
      invisible(model)
    }
    model$predict <- function(x) rep(model$mean, nrow(x))
    model$predict_proba <- function(x) {
      p <- seq_along(model$classes_); p <- p / sum(p)
      matrix(rep(p, each = nrow(x)), nrow(x))
    }
    model
  }
}
api <- setNames(lapply(c("TabICLRegressor", "TabICLClassifier",
                       "FinetunedTabICLRegressor", "FinetunedTabICLClassifier"), constructor),
                c("TabICLRegressor", "TabICLClassifier",
                  "FinetunedTabICLRegressor", "FinetunedTabICLClassifier"))
VIM:::register_tabicl_learners()
for (type in c("regr", "classif")) {
  default <- mlr3::lrn(paste0(type, ".tabicl2"))
  expect_identical(default$param_set$values$epochs, 5L)
  pars_default <- utils::modifyList(default$param_set$get_values(), list(finetune = TRUE))
  expect_identical(VIM:::.tabicl_constructor_args(pars_default)$epochs, 5L)
}
expect_true("tabicl2" %in% vimpute_methods())
expect_error(unregister_vimpute_method("tabicl2"), "built-in")

x <- data.frame(y = seq_len(30) / 3, g = factor(rep(c("a", "b", "c"), 10)),
                z = seq_len(30), ord = ordered(rep(c("low", "high"), 15), levels = c("low", "high")))
task <- mlr3::TaskClassif$new("tabicl", x, target = "g")
l <- mlr3::lrn("classif.tabicl2", predict_type = "prob")
l$param_set$values$tabicl_module <- api
l$train(task)
p <- l$predict(task)$prob
expect_equal(colnames(p), task$class_names)
expect_equal(unname(p[1, ]), c(3, 2, 1) / 6)
expect_equal(rowSums(p), rep(1, 30))

# Fit only observed targets; preserve observed cells, ordered levels and schema.
d <- x; d$y[c(2, 4, 6)] <- NA; d$g[c(3, 5)] <- NA
d$z[8] <- NA_integer_
pars <- list(tabicl_module = api, n_estimators = 2L, device = "cpu", random_state = 7L)
res <- tabiclImpute(d, learner_params = pars, seed = 7)
expect_false(anyNA(res[names(d)]))
for (nm in names(d)) expect_equal(res[[nm]][!is.na(d[[nm]])], d[[nm]][!is.na(d[[nm]])])
expect_true(is.ordered(res$ord))
expect_equal(levels(res$g), levels(d$g))
expect_equal(res$y_imp, is.na(d$y))
expect_true(all(vapply(calls$fit, function(z) !anyNA(z$y), logical(1))))
missing_task <- mlr3::TaskRegr$new("missing", data.frame(y = 1:5, z = c(NA, 2:5)), target = "y")
missing_lrn <- mlr3::lrn("regr.tabicl2")
missing_lrn$param_set$values$tabicl_module <- api
missing_lrn$train(missing_task)
expect_true(anyNA(tail(calls$fit, 1)[[1]]$x))

# Fine-tuning selects both upstream fine-tuners and maps ensemble size.
calls$kind <- character(); calls$args <- list()
fine <- tabiclImpute(d, finetune = TRUE,
  learner_params = c(pars, list(epochs = 2L, learning_rate = 1e-4,
                               finetune_params = list(validation_split_ratio = .2))),
  imp_var = FALSE, seed = 8)
expect_false(anyNA(fine))
expect_true(all(c("FinetunedTabICLRegressor", "FinetunedTabICLClassifier") %in% calls$kind))
expect_true(all(vapply(calls$args, function(a) identical(a$epochs, 2L) &&
  identical(a$n_estimators_inference, 2L) && is.null(a[["n_estimators"]]) &&
  identical(a$validation_split_ratio, .2), logical(1))))

# Registry method parameters, sequential rounds and PMM follow existing methods.
r <- vimpute(d, method = "tabicl2", learner_params = list(tabicl2 = pars),
             sequential = TRUE, nseq = 2L, uncert = "pmm", seed = 11,
             imp_var = FALSE)
expect_false(anyNA(r))
expect_true(all(r$y[is.na(d$y)] %in% d$y[!is.na(d$y)]))
r2 <- vimpute(d, method = "tabicl2", learner_params = list(tabicl2 = pars),
             sequential = TRUE, nseq = 2L, uncert = "pmm", seed = 11,
             imp_var = FALSE)
expect_equal(r, r2)
expect_equal(tabiclImpute(x), x)
expect_error(tabiclImpute(d, finetune = NA), "finetune")
expect_error(tabiclImpute(d, considered_variables = "absent"), "variables")
expect_error(VIM:::.tabicl_constructor_args(list(estimator_params = list(1))), "named lists")

# Unseen categories become NaN and train/predict encodings remain identical.
enc <- list(g = c("a", "b"))
expect_equal(unname(VIM:::.tabicl_matrix(data.frame(g = factor(c("b", "new"))), enc)[1, 1]), 1)
expect_true(is.nan(VIM:::.tabicl_matrix(data.frame(g = "new"), enc)[1, 1]))

# Constant targets need no Python; invalid predictions fail explicitly.
t <- mlr3::TaskRegr$new("constant", data.frame(y = rep(3, 4), x = 1:4), target = "y")
l <- mlr3::lrn("regr.tabicl2"); l$train(t)
expect_equal(l$predict(t)$response, rep(3, 4))
expect_error(VIM:::.tabicl_predict(t, list(features = "x", encoding = list(x = NULL),
  estimator = list(predict = function(x) rep(Inf, nrow(x)))), FALSE, "response"), "Invalid")

# data.table inputs and explicitly excluded columns follow vimpute's contract.
dt <- data.table::as.data.table(d)
copy <- data.table::copy(dt)
out <- tabiclImpute(dt, considered_variables = c("y", "g", "ord"),
                    learner_params = pars, imp_var = FALSE, seed = 7)
expect_true(data.table::is.data.table(out))
expect_equal(dt, copy)
expect_equal(out$z, d$z)
expect_false(anyNA(out$y))

# Classification can retain unused declared levels and handle a single class.
t <- mlr3::TaskClassif$new("one", data.frame(g = factor(rep("a", 5),
  levels = c("a", "b")), x = 1:5), target = "g")
l <- mlr3::lrn("classif.tabicl2", predict_type = "prob"); l$train(t)
expect_equal(unname(l$predict(t)$prob), cbind(rep(1, 5), rep(0, 5)))
