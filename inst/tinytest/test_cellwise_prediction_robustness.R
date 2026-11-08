if (!all(vapply(c("mlr3", "paradox", "R6", "mlr3learners", "mlr3pipelines",
                  "mlr3tuning", "future"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed")
library(VIM)
library(mlr3pipelines)
register_cellwise_learners()
set.seed(661)
d <- data.frame(x = rnorm(180), z = rnorm(180))
d$y <- 1 + 3*d$x - d$z + rnorm(180, sd = .15)
task <- mlr3::as_task_regr(d, target = "y")
l <- mlr3::lrn("regr.cellwise", engine = "cellwise-mm")
l$train(task)
nd <- data.frame(x = c(1e3, 1e6), z = 0)
p <- l$predict_newdata(nd)$response
expect_equal(p[1], p[2])
expect_equal(l$predict_newdata(nd[1, , drop = FALSE])$response, p[1])
expect_equal(p[1], l$predict_newdata(data.frame(x = median(d$x), z = 0))$response)
expect_equal(unserialize(serialize(l, NULL))$predict_newdata(nd)$response, p)
expect_equal(task$data(cols = "x")$x, d$x)
# Disabling the guard recovers the raw-design predictor behavior.
raw <- mlr3::lrn("regr.cellwise", engine = "cellwise-mm", clean_predictors = FALSE)
raw$train(task)
expect_true(abs(raw$predict_newdata(nd[1, , drop = FALSE])$response) > 100)
# Calibration is available through a preprocessing GraphLearner too.
g <- mlr3::as_learner(mlr3pipelines::po("imputemedian") %>>%
                               mlr3pipelines::po("learner", l$clone(deep = TRUE)))
g$train(task)
expect_true(!is.null(VIM:::.cellwise_model(g)$donor_guard))
expect_false(VIM:::.cellwise_valid_donors(g, 100, 0))
# Exact fit / constant target has a positive numerical scale.
cmodel <- VIM:::.cellwise_calibrate_donors(list(), rep(.001, 10), rep(.001, 10), 4.5)
expect_true(cmodel$donor_guard$scale > 0)
expect_true(cmodel$donor_guard$scale < .001)

# Corrupted observed responses must never become PMM donors. The observed
# data itself remains unchanged; this covers uncertainty PMM, legacy PMM,
# midastouch, and a bootstrapped graph model.
d$y[1:25] <- d$y[1:25] + 50
d$y[151:180] <- NA
bad <- d$y[1:25]
for (mode in c("pmm", "legacy", "midastouch", "boot")) {
  args <- list(data = d, method = "cellwise", sequential = FALSE,
    learner_params = list(engine = "cellwise-mm"), imp_var = FALSE, seed = 42,
    uncert = if (mode == "midastouch") "midastouch" else "pmm")
  if (mode == "legacy") {args$pmm <- TRUE; args$uncert <- "none"}
  if (mode == "boot") args$boot <- TRUE
  out <- do.call(vimpute, args)
  expect_true(all(is.finite(out$y)), info = mode)
  expect_true(all(out$y[151:180] < 20), info = mode)
  expect_equal(out$y[1:150], d$y[1:150], info = mode)
  expect_true(all(vapply(out$y[151:180], function(y)
    min(abs(y - d$y[26:150])) < 1e-8, logical(1))), info = mode)
}
# Classifier predictor screening uses the same frozen training statistics.
cdata <- data.frame(x = d$x, z = d$z,
                     y = factor(ifelse(d$x > 0, "yes", "no")))
cl <- mlr3::lrn("classif.cellwise", predict_type = "prob", decay = .1)
cl$train(mlr3::as_task_classif(cdata, target = "y"))
cp <- cl$predict_newdata(nd)$prob
expect_equal(cp[1, ], cp[2, ])
expect_equal(cl$predict_newdata(nd[1, , drop = FALSE])$prob,
             cp[1, , drop = FALSE], check.attributes = FALSE)
