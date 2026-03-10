library(VIM)

# === build_gam_formula tests ===

# Test: numeric predictors get s() wrapping
df <- data.frame(y = rnorm(20), x1 = rnorm(20), x2 = rnorm(20), f1 = factor(sample(letters[1:3], 20, TRUE)))
f <- VIM:::build_gam_formula("y", c("x1", "x2", "f1"), df)
f_str <- deparse(f, width.cutoff = 500)
expect_true(grepl("s\\(x1\\)", f_str), info = "numeric x1 wrapped in s()")
expect_true(grepl("s\\(x2\\)", f_str), info = "numeric x2 wrapped in s()")
expect_true(grepl("f1", f_str), info = "factor f1 included as linear term")
expect_false(grepl("s\\(f1\\)", f_str), info = "factor f1 NOT wrapped in s()")

# Test: numeric with < 4 unique values becomes linear
df2 <- data.frame(y = rnorm(20), x_few = rep(c(1, 2, 3), length.out = 20), x_many = rnorm(20))
f2 <- VIM:::build_gam_formula("y", c("x_few", "x_many"), df2)
f2_str <- deparse(f2, width.cutoff = 500)
expect_false(grepl("s\\(x_few\\)", f2_str), info = "few-unique numeric not smoothed")
expect_true(grepl("x_few", f2_str), info = "few-unique numeric still included")
expect_true(grepl("s\\(x_many\\)", f2_str), info = "many-unique numeric smoothed")

# Test: all factors => no s() terms
df3 <- data.frame(y = rnorm(10), f1 = factor(rep(c("a","b"), 5)), f2 = factor(rep(c("c","d"), 5)))
f3 <- VIM:::build_gam_formula("y", c("f1", "f2"), df3)
f3_str <- deparse(f3, width.cutoff = 500)
expect_false(grepl("s\\(", f3_str), info = "all-factor formula has no s()")

# Test: single predictor
df4 <- data.frame(y = rnorm(20), x1 = rnorm(20))
f4 <- VIM:::build_gam_formula("y", "x1", df4)
f4_str <- deparse(f4, width.cutoff = 500)
expect_true(grepl("s\\(x1\\)", f4_str), info = "single numeric predictor smoothed")

# === LearnerRegrGAM tests ===
library(mlr3)

VIM:::register_gam_learners()

# Test: learner is registered and can be instantiated
lrn_gam <- lrn("regr.gam_imp")
expect_true(inherits(lrn_gam, "Learner"), info = "regr.gam_imp is a Learner")
expect_true("numeric" %in% lrn_gam$feature_types, info = "supports numeric features")
expect_true("factor" %in% lrn_gam$feature_types, info = "supports factor features")

# Test: train and predict on simple data
set.seed(42)
train_df <- data.frame(y = rnorm(50), x1 = rnorm(50), x2 = rnorm(50),
                        f1 = factor(sample(letters[1:3], 50, TRUE)))
task_regr <- TaskRegr$new(id = "test_gam", backend = train_df, target = "y")
lrn_gam$train(task_regr)
expect_true(!is.null(lrn_gam$model), info = "model is fitted")

pred <- lrn_gam$predict(task_regr)
expect_equal(length(pred$response), 50, info = "predictions have correct length")
expect_true(all(is.finite(pred$response)), info = "predictions are finite")

# Test: model object is a gam
raw_mod <- lrn_gam$model
expect_true(inherits(raw_mod, "gam"), info = "stored model is a gam object")

# === LearnerClassifGAM tests ===

# Binary classification
set.seed(42)
train_bin <- data.frame(y = factor(sample(c("a", "b"), 50, TRUE)),
                         x1 = rnorm(50), x2 = rnorm(50))
task_bin <- TaskClassif$new(id = "test_gam_bin", backend = train_bin, target = "y")
lrn_gam_c <- lrn("classif.gam_imp")
lrn_gam_c$train(task_bin)
expect_true(!is.null(lrn_gam_c$model), info = "binary classif model fitted")

lrn_gam_c$predict_type <- "prob"
pred_bin <- lrn_gam_c$predict(task_bin)
expect_equal(nrow(pred_bin$prob), 50, info = "binary probs correct size")
expect_true(all(rowSums(pred_bin$prob) > 0.99), info = "binary probs sum to ~1")

# Multiclass classification (OvR)
set.seed(42)
train_multi <- data.frame(y = factor(sample(c("a", "b", "c"), 60, TRUE)),
                           x1 = rnorm(60), x2 = rnorm(60))
task_multi <- TaskClassif$new(id = "test_gam_multi", backend = train_multi, target = "y")
lrn_gam_m <- lrn("classif.gam_imp")
lrn_gam_m$train(task_multi)
expect_true(!is.null(lrn_gam_m$model), info = "multiclass model fitted")

lrn_gam_m$predict_type <- "prob"
pred_multi <- lrn_gam_m$predict(task_multi)
expect_equal(ncol(pred_multi$prob), 3, info = "multiclass has 3 prob columns")
expect_true(all(abs(rowSums(pred_multi$prob) - 1) < 0.01), info = "multiclass probs sum to ~1")

# Response prediction
lrn_gam_r <- lrn("classif.gam_imp")
lrn_gam_r$train(task_bin)
pred_resp <- lrn_gam_r$predict(task_bin)
expect_true(all(pred_resp$response %in% c("a", "b")), info = "response predictions are valid levels")

# === LearnerRegrRobGAM tests ===

# Test: simple method (default)
set.seed(42)
n <- 80
train_rob <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
train_rob$y[1:5] <- train_rob$y[1:5] + 20
task_rob <- TaskRegr$new(id = "test_robgam", backend = train_rob, target = "y")

lrn_rob <- lrn("regr.robgam_imp")
lrn_rob$train(task_rob)
expect_true(!is.null(lrn_rob$model), info = "robgam simple model fitted")
pred_rob <- lrn_rob$predict(task_rob)
expect_equal(length(pred_rob$response), n, info = "robgam predictions correct length")
expect_true(all(is.finite(pred_rob$response)), info = "robgam predictions finite")

# Test: model stores good/bad subset info
mod_info <- lrn_rob$model
expect_true(!is.null(mod_info$subset_good), info = "robgam stores good subset")
expect_true(!is.null(mod_info$subset_bad), info = "robgam stores bad subset")
expect_true(length(mod_info$subset_good) + length(mod_info$subset_bad) == n,
            info = "good + bad = total n")

# Test: irw method
lrn_irw <- lrn("regr.robgam_imp")
lrn_irw$param_set$values$robust_method <- "irw"
lrn_irw$train(task_rob)
expect_true(!is.null(lrn_irw$model), info = "robgam irw model fitted")
pred_irw <- lrn_irw$predict(task_rob)
expect_true(all(is.finite(pred_irw$response)), info = "robgam irw predictions finite")

# Test: alpha parameter changes good subset size
lrn_alpha <- lrn("regr.robgam_imp")
lrn_alpha$param_set$values$alpha <- 0.5
lrn_alpha$train(task_rob)
expect_true(length(lrn_alpha$model$subset_good) <= ceiling(n * 0.5 + 1),
            info = "alpha=0.5 restricts good subset")

# === LearnerClassifRobGAM tests ===

# Binary classification with outliers
set.seed(42)
n <- 60
train_robcl <- data.frame(y = factor(sample(c("a", "b"), n, TRUE)),
                           x1 = rnorm(n), x2 = rnorm(n))
train_robcl$x1[1:3] <- train_robcl$x1[1:3] + 15
task_robcl <- TaskClassif$new(id = "test_robgam_cl", backend = train_robcl, target = "y")

lrn_robcl <- lrn("classif.robgam_imp")
lrn_robcl$train(task_robcl)
expect_true(!is.null(lrn_robcl$model), info = "robgam classif binary fitted")

lrn_robcl$predict_type <- "prob"
pred_robcl <- lrn_robcl$predict(task_robcl)
expect_equal(nrow(pred_robcl$prob), n, info = "robgam classif probs correct size")
expect_true(all(abs(rowSums(pred_robcl$prob) - 1) < 0.01), info = "robgam classif probs sum to ~1")

# Multiclass
train_robmc <- data.frame(y = factor(sample(c("a", "b", "c"), 90, TRUE)),
                           x1 = rnorm(90), x2 = rnorm(90))
task_robmc <- TaskClassif$new(id = "test_robgam_mc", backend = train_robmc, target = "y")
lrn_robmc <- lrn("classif.robgam_imp")
lrn_robmc$train(task_robmc)
lrn_robmc$predict_type <- "prob"
pred_robmc <- lrn_robmc$predict(task_robmc)
expect_equal(ncol(pred_robmc$prob), 3, info = "robgam multiclass has 3 prob cols")

# === vimpute integration tests ===

library(data.table)

# Test: method = "gam" is accepted by precheck
data(sleep, package = "VIM")
dt <- as.data.table(sleep)
expect_silent(
  VIM:::precheck(dt, pmm = FALSE, formula = FALSE, method = "gam",
                 sequential = FALSE, pmm_k = NULL, pmm_k_method = "mean",
                 learner_params = NULL, tune = FALSE)
)

# Test: method = "robgam" is accepted by precheck
expect_silent(
  VIM:::precheck(dt, pmm = FALSE, formula = FALSE, method = "robgam",
                 sequential = FALSE, pmm_k = NULL, pmm_k_method = "mean",
                 learner_params = NULL, tune = FALSE)
)

# Test: simple GAM imputation works
set.seed(123)
result_gam <- vimpute(data = sleep, method = "gam", sequential = FALSE)
expect_true(is.data.table(result_gam) || is.data.frame(result_gam),
            info = "gam imputation returns data")
expect_true(sum(is.na(result_gam$Dream)) == 0, info = "gam imputes Dream")
expect_true(sum(is.na(result_gam$Sleep)) == 0, info = "gam imputes Sleep")

# Test: simple robgam imputation works
set.seed(123)
result_robgam <- vimpute(data = sleep, method = "robgam", sequential = FALSE)
expect_true(is.data.table(result_robgam) || is.data.frame(result_robgam),
            info = "robgam imputation returns data")
expect_true(sum(is.na(result_robgam$Dream)) == 0, info = "robgam imputes Dream")

# Test: GAM with sequential imputation
set.seed(42)
result_seq <- vimpute(data = sleep, method = "gam", sequential = TRUE, nseq = 3)
expect_true(sum(is.na(result_seq$Dream)) == 0, info = "sequential gam imputes Dream")

# Test: robgam with irw via learner_params
set.seed(42)
result_irw <- vimpute(data = sleep, method = "robgam",
                       learner_params = list(robgam = list(robust_method = "irw")),
                       sequential = FALSE)
expect_true(sum(is.na(result_irw$Dream)) == 0, info = "robgam irw imputes Dream")

# Test: GAM with bootstrap and uncertainty
set.seed(42)
result_boot <- vimpute(data = sleep, method = "gam", sequential = FALSE,
                        boot = TRUE, uncert = "normalerror")
expect_true(sum(is.na(result_boot$Dream)) == 0, info = "gam+boot+uncert works")

# Test: robgam with MI (m > 1)
set.seed(42)
result_mi <- vimpute(data = sleep, method = "robgam", sequential = FALSE,
                      boot = TRUE, uncert = "normalerror", m = 3)
expect_true(inherits(result_mi, "vimmi"), info = "robgam MI returns vimmi")
expect_equal(result_mi$m, 3L, info = "robgam MI has m=3")
d1 <- complete(result_mi, 1)
expect_true(sum(is.na(d1$Dream)) == 0, info = "robgam MI complete(1) has no NAs")

# Test: mixed methods (gam for some, ranger for others)
set.seed(42)
vars_na <- names(which(sapply(sleep, function(x) any(is.na(x)))))
mixed_method <- setNames(as.list(rep("ranger", length(vars_na))), vars_na)
mixed_method[[vars_na[1]]] <- "gam"
result_mixed <- vimpute(data = sleep, method = mixed_method, sequential = FALSE)
expect_true(sum(is.na(result_mixed[[vars_na[1]]])) == 0, info = "mixed gam+ranger works")
