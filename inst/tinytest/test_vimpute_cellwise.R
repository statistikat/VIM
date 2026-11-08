if (!all(vapply(c("mlr3", "paradox", "R6", "mlr3learners", "mlr3pipelines",
                  "mlr3tuning", "future"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed")
suppressMessages(library(VIM))
register_cellwise_learners()
expect_true("cellwise" %in% vimpute_methods())

set.seed(144)
d <- data.frame(x = rnorm(100), z = rnorm(100),
                group = factor(rep(c("a", "b"), 50)))
d$y <- 2 + 3 * d$x - d$z + (d$group == "b") + rnorm(100, sd = .1)
names(d)[1] <- "odd name"
engines <- c("cellwise-mm", "shooting-s", "irwls")
if (requireNamespace("crmReg", quietly = TRUE)) engines <- c(engines, "crm")
for (engine in engines) {
  task <- mlr3::as_task_regr(d, target = "y")
  learner <- mlr3::lrn("regr.cellwise", engine = engine)
  set.seed(42)
  learner$train(task, row_ids = 1:80)
  prediction <- learner$predict(task, row_ids = 81:100)$response
  expect_true(all(is.finite(prediction)), info = engine)
  expect_true(mean((prediction - d$y[81:100])^2) < 1, info = engine)
  # Prediction is inductive, works without a target, and is batch invariant.
  newdata <- d[81:100, setdiff(names(d), "y")]
  expect_equal(learner$predict_newdata(newdata)$response, prediction, info = engine)
  expect_equal(learner$predict_newdata(newdata[1, , drop = FALSE])$response,
               prediction[1], info = engine)
  # Held-out targets cannot enter training, even through weight initialization.
  changed <- d
  changed$y[81:100] <- 1e8
  set.seed(42)
  other <- mlr3::lrn("regr.cellwise", engine = engine)
  other$train(mlr3::as_task_regr(changed, target = "y"), row_ids = 1:80)
  expect_equal(other$predict_newdata(newdata)$response, prediction, info = engine)
  # Serialization keeps everything needed to predict, including factor coding.
  restored <- unserialize(serialize(learner, NULL))
  expect_equal(restored$predict_newdata(newdata)$response, prediction, info = engine)
}

# Constant and single-level predictors are removed; intercept-only fit works.
constant <- data.frame(y = 1:20, x = 1, g = factor(rep("a", 20)))
l <- mlr3::lrn("regr.cellwise", engine = "cellwise-mm")
l$train(mlr3::as_task_regr(constant, target = "y"))
expect_equal(l$predict_newdata(constant[, -1])$response, rep(10.5, 20))
l$train(mlr3::as_task_regr(constant[, "y", drop = FALSE], target = "y"))
expect_equal(l$predict(mlr3::as_task_regr(constant[, "y", drop = FALSE],
                                     target = "y"))$response, rep(10.5, 20))
expect_error(mlr3::lrn("regr.cellwise", engine = "invalid"))
expect_error(mlr3::lrn("regr.cellwise", alpha = -1))

# Registry integration, observed-cell preservation and PMM donor membership.
names(d)[1] <- "x"
d$y[81:90] <- NA
for (engine in engines) {
  out <- suppressWarnings(vimpute(d, method = "cellwise",
    learner_params = list(cellwise = list(engine = engine)),
    sequential = FALSE, uncert = "none", imp_var = FALSE,
    verbose = FALSE, seed = 42))
  expect_true(all(is.finite(out$y)), info = engine)
  expect_equal(out$y[-(81:90)], d$y[-(81:90)], info = engine)
}
out <- suppressWarnings(vimpute(d, method = "cellwise",
  learner_params = list(y = list(engine = "irwls", weight_function = "huber")),
  nseq = 2, uncert = "pmm", pmm_k = 1, imp_var = FALSE,
  verbose = FALSE, seed = 42))
# vimpute rounds numeric outputs to the observed decimal precision.
expect_true(all(vapply(out$y[81:90], function(y)
  min(abs(y - d$y[-(81:90)])) < 1e-12, logical(1))))

# A mixed-data run uses the cellwise classifier for the categorical target.
d$group[91:95] <- NA
out <- suppressWarnings(vimpute(d, method = "cellwise",
  learner_params = list(y = list(engine = "cellwise-mm")),
  sequential = FALSE, uncert = "none", imp_var = FALSE,
  verbose = FALSE, seed = 42))
expect_false(anyNA(out))
expect_true(is.factor(out$group))
