if (!all(vapply(c("mlr3", "paradox", "R6", "mlr3learners", "mlr3pipelines",
                  "mlr3tuning", "future"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed")
library(VIM)
register_cellwise_learners()
set.seed(812)
x <- rnorm(180)
d <- data.frame(x = x, z = rnorm(180), f = factor(rep(c("b", "a"), 90)),
                y = factor(ifelse(x + rlogis(180) / 4 > 0, "yes", "no"),
                           levels = c("yes", "no")))
names(d)[1] <- "odd predictor"
for (multi in c(FALSE, TRUE)) {
  dd <- d
  if (multi) dd$y <- factor(ifelse(x < -.5, "low", ifelse(x > .5, "high", "mid")),
                            levels = c("mid", "high", "low"))
  t <- mlr3::as_task_classif(dd, target = "y")
  l <- mlr3::lrn("classif.cellwise", predict_type = "prob", init_weights = "univariate")
  l$train(t, row_ids = 1:140)
  p <- l$predict(t, row_ids = 141:180)
  expect_equal(colnames(p$prob), t$class_names)
  expect_equal(rowSums(p$prob), rep(1, 40), check.attributes = FALSE)
  expect_true(all(is.finite(p$prob) & p$prob >= 0 & p$prob <= 1))
  expect_true(mean(p$response == dd$y[141:180]) >= .8)
  nd <- dd[141:180, setdiff(names(dd), "y")]
  expect_equal(l$predict_newdata(nd[1, , drop = FALSE])$prob,
               p$prob[1, , drop = FALSE], check.attributes = FALSE)
  expect_equal(unserialize(serialize(l, NULL))$predict_newdata(nd)$prob, p$prob)
  # Neither held-out targets nor held-out predictor outliers enter estimation.
  dd$y[141:180] <- levels(dd$y)[1]
  dd[[1]][141:180] <- 1e8
  other <- l$clone(deep = TRUE)
  other$train(mlr3::as_task_classif(dd, target = "y"), row_ids = 1:140)
  expect_equal(other$predict_newdata(nd)$prob, p$prob)
}
# Check actual row weighting and numerical equivalence to weighted multinom.
d[[1]][1] <- 1000
l <- mlr3::lrn("classif.cellwise", predict_type = "prob", init_weights = "univariate",
                clean_predictors = FALSE)
l$train(mlr3::as_task_classif(d, target = "y"))
W <- VIM:::cellWeights(as.matrix(d[c(1, 2)]), method = "tukey")
w <- exp(rowMeans(log(pmax(W, 1e-10))))
expect_equal(l$model$row_weights, w, check.attributes = FALSE)
expect_true(w[1] < .001)
reference <- d
names(reference) <- c("x", "z", "f", "y")
reference$.w <- w
fit <- nnet::multinom(y ~ x + z + f, reference, weights = .w, trace = FALSE, maxit = 200)
pp <- predict(fit, reference, type = "probs")
expect_equal(l$predict_newdata(d)$prob[, "no"], as.numeric(pp), tolerance = 1e-6)
# Pure categorical predictors, absent classes in training and single-class folds.
catdata <- data.frame(f = factor(rep(c("a", "b"), 20)),
                      y = factor(rep(c("a", "b"), 20), levels = c("a", "b", "c")))
t <- mlr3::as_task_classif(catdata, target = "y")
l$train(t)
expect_equal(l$model$row_weights, rep(1, 40))
expect_equal(l$predict(t)$prob[, "c"], rep(0, 40))
l$train(t, row_ids = which(catdata$y == "a"))
expect_equal(l$predict(t)$prob[, "a"], rep(1, 40))
# Featureless prior keeps task-level class mapping.
t0 <- mlr3::as_task_classif(catdata["y"], target = "y")
l$train(t0)
expect_equal(l$predict(t0)$prob[1, ], c(a = .5, b = .5, c = 0))
# vimpute routes categorical targets to the classifier and samples reproducibly.
names(d)[1] <- "x"
d$y[151:180] <- NA
warnings <- character()
run <- function(uncert) withCallingHandlers(vimpute(d, method = "cellwise",
  learner_params = list(init_weights = "univariate", maxit = 300),
  sequential = FALSE, uncert = uncert, imp_var = FALSE, seed = 17),
  warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")})
a <- run("none")
expect_false(anyNA(a))
expect_equal(a$y[1:150], d$y[1:150])
expect_equal(levels(a$y), levels(d$y))
expect_equal(run("pmm")$y, run("pmm")$y)
expect_false(any(grepl("fallback|falling back|ignored|not supported", warnings, ignore.case = TRUE)))

# MCD weighting is also computed solely from continuous predictors.
l <- mlr3::lrn("classif.cellwise", predict_type = "prob", init_weights = "mcd")
complete <- d[1:150, ]
set.seed(19)
l$train(mlr3::as_task_classif(complete, target = "y"))
set.seed(19)
W <- VIM:::cellWeightsMCD(as.matrix(complete[c("x", "z")]), method = "tukey")
expect_equal(l$model$row_weights, exp(rowMeans(log(pmax(W, 1e-10)))),
             check.attributes = FALSE)
nd <- complete[1, ]
nd$f <- factor("unseen")
expect_error(l$predict_newdata(nd))
nd <- complete[1, ]
nd$x <- NA_real_
expect_error(l$predict_newdata(nd))
