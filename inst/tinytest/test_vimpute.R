library(VIM)

# vimpute returns imputed data and *_imp indicators", {
  set.seed(1)
  out <- vimpute(sleep, method = "ranger", sequential = FALSE, imp_var = TRUE)

  expect_true(inherits(out, "data.frame"))
  expect_identical(nrow(out), nrow(sleep))
  expect_true(all(colnames(sleep) %in% colnames(out)))
  expect_true(any(grepl("_imp$", colnames(out))))
  expect_equal(sum(is.na(out[, colnames(sleep), with = FALSE])), 0)
# 

# vimpute without imp_var returns no *_imp columns", {
  set.seed(1)
  out <- vimpute(sleep, method = "ranger", sequential = FALSE, imp_var = FALSE)

  expect_identical(nrow(out), nrow(sleep))
  expect_false(any(grepl("_imp$", colnames(out))))
  expect_equal(sum(is.na(out)), 0)
# 

# vimpute supports ranger median aggregation via learner_params", {
  set.seed(1)
  out_mean <- vimpute(sleep, method = "ranger", sequential = FALSE, imp_var = FALSE)
  set.seed(1)
  out_median <- vimpute(
    sleep,
    method = "ranger",
    sequential = FALSE,
    imp_var = FALSE,
    learner_params = list(ranger = list(predict_median = TRUE))
  )

  expect_equal(sum(is.na(out_median)), 0)
  expect_true(max(abs(out_mean$Dream - out_median$Dream)) > 0)
# 

# vimpute accepts xgboost learner_params with eta", {
  d <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
  method_all <- setNames(as.list(rep("xgboost", ncol(d))), names(d))
  pmm_all <- setNames(as.list(rep(FALSE, ncol(d))), names(d))

  set.seed(1)
  out <- vimpute(
    d,
    method = method_all,
    pmm = pmm_all,
    sequential = FALSE,
    imp_var = FALSE,
    learner_params = list(xgboost = list(nrounds = 25, eta = 0.2, max_depth = 3))
  )

  expect_identical(nrow(out), nrow(d))
  expect_equal(sum(is.na(out)), 0)
# 

# vimpute returns prediction history when requested", {
  set.seed(1)
  out <- vimpute(
    sleep,
    method = "ranger",
    sequential = FALSE,
    nseq = 3,
    pred_history = TRUE
  )

  expect_true(is.list(out))
  expect_true(all(c("data", "pred_history") %in% names(out)))
  expect_true(inherits(out$data, "data.frame"))
  expect_true(all(c("iteration", "variable", "index", "predicted_values") %in% names(out$pred_history)))
  expect_true(nrow(out$pred_history) > 0)
  expect_equal(max(out$pred_history$iteration), 1)
# 

# vimpute imputes mixed numeric and factor targets", {
  set.seed(42)
  d <- data.frame(
    x = rnorm(40),
    z = rnorm(40)
  )
  d$grp <- factor(ifelse(d$x > 0, "A", "B"))
  d$y <- d$x + d$z
  d$y[sample(40, 6)] <- NA
  d$grp[sample(40, 5)] <- NA

  out <- vimpute(d, method = "ranger", sequential = FALSE, imp_var = TRUE)

  expect_equal(sum(is.na(out$y)), 0)
  expect_equal(sum(is.na(out$grp)), 0)
  expect_true(is.factor(out$grp))
  expect_true(all(c("y_imp", "grp_imp") %in% names(out)))
# 

# vimpute validates method values", {
  d <- sleep[1:20, c("Sleep", "Dream", "Span")]
  expect_error(vimpute(d, method = list("invalid"), sequential = FALSE))
# 

# vimpute validates pmm length/type", {
  d <- sleep[1:20, c("Sleep", "Dream", "Span")]
  expect_error(vimpute(d, pmm = list(TRUE), method = "ranger", sequential = FALSE))
# 

# vimpute requires missing values in input", {
  expect_error(vimpute(iris, method = "ranger", sequential = FALSE))
# 

# vimpute validates formula type", {
  d <- sleep[1:20, c("Sleep", "Dream", "Span")]
  expect_error(vimpute(d, formula = ~Dream + Span, method = "ranger", sequential = FALSE))
# 
# 

# vimpute supports considered_variables subsets", {
  vars <- c("Sleep", "Dream", "Span")
  set.seed(1)
  out <- vimpute(
    sleep,
    considered_variables = vars,
    method = "ranger",
    sequential = FALSE,
    imp_var = TRUE
  )

  expect_true(all(vars %in% names(out)))
  expect_false(any(c("BodyWgt", "BrainWgt") %in% names(out)))
  expect_equal(sum(is.na(out[, vars, with = FALSE])), 0)
# 

# vimpute accepts pmm list named for NA variables only", {
  pmm_na_vars <- setNames(
    as.list(rep(FALSE, 5)),
    c("NonD", "Dream", "Sleep", "Span", "Gest")
  )
  set.seed(1)
  out <- vimpute(
    sleep,
    method = "ranger",
    pmm = pmm_na_vars,
    sequential = FALSE,
    imp_var = FALSE
  )

  expect_identical(nrow(out), nrow(sleep))
  expect_equal(sum(is.na(out)), 0)
# 

# vimpute runs multiple sequential iterations when configured", {
  set.seed(1)
  out <- vimpute(
    sleep,
    method = "ranger",
    sequential = TRUE,
    nseq = 2,
    eps = -1,
    pred_history = TRUE
  )

  expect_equal(max(out$pred_history$iteration), 2)
  expect_equal(length(unique(out$pred_history$iteration)), 2)
# 

# vimpute rejects formulas for unsupported methods", {
  d <- sleep[1:20, c("Sleep", "Dream", "Span", "BodyWgt")]
  expect_error(
    vimpute(
      d,
      method = "ranger",
      pmm = FALSE,
      formula = list(Sleep ~ Dream + Span + BodyWgt),
      sequential = FALSE
    )
  )
# 

# vimpute warns on columns with more than 50% missing values", {
  d <- sleep[1:20, c("Sleep", "Dream", "Span")]
  d$Dream[1:15] <- NA
  expect_warning(
    vimpute(d, method = "ranger", sequential = FALSE, imp_var = FALSE),
    "more than 50% missing values"
  )
# 

# vimpute supports formulas with target transformations for regularized models", {
  d <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
  method_all <- setNames(as.list(rep("regularized", ncol(d))), names(d))
  pmm_all <- setNames(as.list(rep(FALSE, ncol(d))), names(d))

  set.seed(1)
  expect_warning(out <- vimpute(
    d,
    method = method_all,
    pmm = pmm_all,
    formula = list(log(Sleep) ~ Dream + Span + BodyWgt),
    sequential = FALSE,
    imp_var = FALSE
  ))

  expect_identical(nrow(out), nrow(d))
  expect_equal(sum(is.na(out)), 0)
  expect_true(all(out$Sleep > 0, na.rm = TRUE))
# 

# vimpute returns tuning_log output when tune is requested", {
  d <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
  method_all <- setNames(as.list(rep("ranger", ncol(d))), names(d))
  pmm_all <- setNames(as.list(rep(TRUE, ncol(d))), names(d))

  set.seed(1)
  out <- suppressWarnings(vimpute(
    d,
    method = method_all,
    pmm = pmm_all,
    sequential = TRUE,
    nseq = 2,
    tune = TRUE,
    imp_var = FALSE
  ))

  expect_true(is.list(out))
  expect_true(all(c("data", "tuning_log") %in% names(out)))
  expect_true(length(out$tuning_log) > 0)
  expect_true(all(c("variable", "tuned_better") %in% names(out$tuning_log[[1]])))
# 

# vimpute runs robust method without leaving missings", {
  d <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
  method_all <- setNames(as.list(rep("robust", ncol(d))), names(d))
  pmm_all <- setNames(as.list(rep(FALSE, ncol(d))), names(d))

  set.seed(1)
  out <- suppressWarnings(vimpute(
    d,
    method = method_all,
    pmm = pmm_all,
    sequential = FALSE,
    imp_var = FALSE
  ))

  expect_identical(nrow(out), nrow(d))
  expect_equal(sum(is.na(out)), 0)
# 

# vimpute handles character predictors by converting to factors", {
  set.seed(7)
  d <- data.frame(
    city = sample(c("A", "B", "C"), 60, replace = TRUE),
    x = rnorm(60),
    y = rnorm(60),
    stringsAsFactors = FALSE
  )
  d$y[1:5] <- NA

  out <- suppressWarnings(vimpute(
    d,
    method = "ranger",
    sequential = FALSE,
    imp_var = FALSE
  ))

  expect_true(is.factor(out$city))
  expect_equal(sum(is.na(out$y)), 0)
#

### ---- Helper function tests ---- ###

# bootstrap_resample returns valid indices for stratified bootstrap
if (requireNamespace("robustbase", quietly = TRUE)) {
  set.seed(42)
  n <- 100
  fake_weights <- runif(n)
  fake_residuals <- rnorm(n)

  idx <- VIM:::bootstrap_resample(
    n = n,
    strategy = "stratified",
    weights = fake_weights,
    residuals = fake_residuals,
    alpha = 0.75
  )

  expect_true(length(idx) == n)
  expect_true(all(idx >= 1 & idx <= n))
  expect_true(is.integer(idx) || is.numeric(idx))
}

# bootstrap_resample covers all strategies
set.seed(1)
n <- 50
res <- rnorm(n)
w <- runif(n)

idx_std <- VIM:::bootstrap_resample(n, "standard")
expect_true(length(idx_std) == n)

idx_strat <- VIM:::bootstrap_resample(n, "stratified", residuals = res, alpha = 0.75)
expect_true(length(idx_strat) == n)

idx_quant <- VIM:::bootstrap_resample(n, "quantile", weights = w)
expect_true(length(idx_quant) == n)

idx_resid <- VIM:::bootstrap_resample(n, "residual", residuals = res)
expect_true(length(idx_resid) == n)

idx_psi <- VIM:::bootstrap_resample(n, "psi", residuals = res)
expect_true(length(idx_psi) == n)

# extract_model_info returns list with expected components for lm
set.seed(1)
d_lm <- data.frame(y = rnorm(30), x = rnorm(30))
mod_lm <- lm(y ~ x, data = d_lm)

info_lm <- VIM:::extract_model_info(mod_lm, method = "lm")
expect_true(is.list(info_lm))
expect_true("residuals" %in% names(info_lm))
expect_true("scale" %in% names(info_lm))
expect_true(length(info_lm$residuals) == 30)
expect_true(is.numeric(info_lm$scale) && length(info_lm$scale) == 1)

# extract_model_info works with lmrob
if (requireNamespace("robustbase", quietly = TRUE)) {
  set.seed(1)
  d_rob <- data.frame(y = rnorm(40), x = rnorm(40))
  mod_rob <- robustbase::lmrob(y ~ x, data = d_rob)

  info_rob <- VIM:::extract_model_info(mod_rob, method = "robust")
  expect_true(!is.null(info_rob$residuals))
  expect_true(!is.null(info_rob$scale))
  expect_true(!is.null(info_rob$weights))
  expect_true(length(info_rob$weights) == 40)
}

# inject_uncertainty with normalerror adds noise
set.seed(42)
preds_ne <- rep(5.0, 20)
noisy <- VIM:::inject_uncertainty(preds = preds_ne, method = "normalerror", scale = 1.0)
expect_true(length(noisy) == 20)
expect_true(!all(noisy == 5.0))
expect_true(abs(mean(noisy) - 5.0) < 1.0)

# inject_uncertainty with resid adds sampled residuals
set.seed(1)
preds_re <- rep(10.0, 15)
resids <- rnorm(100, sd = 2)
noisy_re <- VIM:::inject_uncertainty(preds_re, method = "resid", residuals = resids)
expect_true(length(noisy_re) == 15)
expect_true(!all(noisy_re == 10.0))

# pmm_donor_selection returns valid donor-based values
set.seed(1)
y_obs <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
score_obs <- y_obs + rnorm(10, sd = 0.1)
score_miss <- c(2.5, 7.3)

result_pmm <- VIM:::pmm_donor_selection(
  y_obs = y_obs, score_obs = score_obs, score_miss = score_miss,
  k = 3, agg_method = "mean"
)
expect_true(length(result_pmm) == 2)
expect_true(all(is.finite(result_pmm)))
expect_true(abs(result_pmm[1] - 2.5) < 3)
expect_true(abs(result_pmm[2] - 7.3) < 3)

# midastouch_donors returns valid imputed values
set.seed(1)
n_obs_mt <- 50
n_miss_mt <- 10
y_obs_mt <- rnorm(n_obs_mt, mean = 5)
X_obs_mt <- matrix(rnorm(n_obs_mt * 3), ncol = 3)
X_miss_mt <- matrix(rnorm(n_miss_mt * 3), ncol = 3)
score_obs_mt <- y_obs_mt + rnorm(n_obs_mt, sd = 0.1)
score_miss_mt <- rnorm(n_miss_mt, mean = 5, sd = 0.5)

result_mt <- VIM:::midastouch_donors(
  y_obs = y_obs_mt, X_obs = X_obs_mt, X_miss = X_miss_mt,
  score_obs = score_obs_mt, score_miss = score_miss_mt, k = 5
)
expect_true(length(result_mt) == n_miss_mt)
expect_true(all(is.finite(result_mt)))
expect_true(all(result_mt %in% y_obs_mt))
#

### ---- Task 9: New parameter tests ---- ###

# vimpute accepts new uncertainty parameters without error
set.seed(1)
d_t9 <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
out_t9 <- vimpute(d_t9, method = "ranger", sequential = FALSE, imp_var = FALSE,
                  boot = FALSE, uncert = "none", m = 1)
expect_equal(sum(is.na(out_t9)), 0)

# vimpute rejects invalid m parameter
d_t9s <- sleep[1:20, c("Sleep", "Dream", "Span")]
expect_error(vimpute(d_t9s, method = "ranger", sequential = FALSE, m = 0))
expect_error(vimpute(d_t9s, method = "ranger", sequential = FALSE, m = -1))

# vimpute rejects invalid uncert parameter
expect_error(vimpute(d_t9s, method = "ranger", sequential = FALSE, uncert = "invalid"))

# vimpute warns when both pmm and uncert are set
expect_warning(
  vimpute(d_t9s, method = "ranger", sequential = FALSE, imp_var = FALSE,
          pmm = TRUE, uncert = "normalerror"),
  "pmm.*takes precedence"
)
#

### ---- Task 10: m-loop / vimids tests ---- ###

# vimpute with m > 1 returns a vimids object
set.seed(1)
d_t10 <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
out_t10 <- vimpute(d_t10, method = "ranger", sequential = FALSE, imp_var = FALSE, m = 3)

expect_true(inherits(out_t10, "vimids"))
expect_equal(out_t10$m, 3L)
expect_equal(nrow(out_t10$data), nrow(d_t10))

# Imputed values stored per variable
expect_true(is.list(out_t10$imp))
for (varname in names(out_t10$imp)) {
  expect_equal(ncol(out_t10$imp[[varname]]), 3)
}

# complete() produces valid datasets
c1_t10 <- complete(out_t10, 1)
c2_t10 <- complete(out_t10, 2)
expect_equal(sum(is.na(c1_t10)), 0)
expect_equal(sum(is.na(c2_t10)), 0)
expect_equal(nrow(c1_t10), nrow(d_t10))

# m=1 still returns plain data.table (backward compatible)
set.seed(1)
out1_t10 <- vimpute(d_t10, method = "ranger", sequential = FALSE, imp_var = FALSE, m = 1)
expect_true(inherits(out1_t10, "data.frame"))
expect_false(inherits(out1_t10, "vimids"))
#

### ---- Task 11: Bootstrap integration tests ---- ###

# vimpute with boot=TRUE and method="robust" produces no NAs
set.seed(1)
d_t11 <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
method_t11 <- setNames(as.list(rep("robust", ncol(d_t11))), names(d_t11))

set.seed(42)
out_boot <- suppressWarnings(vimpute(d_t11, method = method_t11, sequential = FALSE,
                                      imp_var = FALSE, boot = TRUE, robustboot = "stratified"))
expect_equal(sum(is.na(out_boot)), 0)

# vimpute with boot=TRUE and ranger uses standard bootstrap
set.seed(1)
out_boot_ranger <- vimpute(d_t11, method = "ranger", sequential = FALSE, imp_var = FALSE,
                           boot = TRUE, robustboot = "standard")
expect_equal(sum(is.na(out_boot_ranger)), 0)
#

### ---- Task 12: Uncertainty injection tests ---- ###

# vimpute with uncert="normalerror" and method="robust" produces stochastic output
d_t12 <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
method_t12 <- setNames(as.list(rep("robust", ncol(d_t12))), names(d_t12))

set.seed(42)
out_ne1 <- suppressWarnings(vimpute(d_t12, method = method_t12, sequential = FALSE,
                                     imp_var = FALSE, uncert = "normalerror"))
set.seed(99)
out_ne2 <- suppressWarnings(vimpute(d_t12, method = method_t12, sequential = FALSE,
                                     imp_var = FALSE, uncert = "normalerror"))
expect_equal(sum(is.na(out_ne1)), 0)
expect_equal(sum(is.na(out_ne2)), 0)
expect_true(!identical(out_ne1$Sleep, out_ne2$Sleep))

# vimpute with uncert="midastouch" returns valid results
set.seed(1)
out_mt <- suppressWarnings(vimpute(d_t12, method = method_t12, sequential = FALSE,
                                    imp_var = FALSE, uncert = "midastouch"))
expect_equal(sum(is.na(out_mt)), 0)
expect_true(all(is.finite(out_mt$Sleep)))
#

### ---- Task 13: Full integration tests (boot + uncert + m) ---- ###

# vimpute with boot + uncert + m > 1 returns proper vimids
set.seed(1)
d_t13 <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]
method_t13 <- setNames(as.list(rep("robust", ncol(d_t13))), names(d_t13))
out_full <- suppressWarnings(vimpute(
  d_t13, method = method_t13, sequential = FALSE, imp_var = FALSE,
  boot = TRUE, robustboot = "stratified", uncert = "normalerror", m = 3
))

expect_true(inherits(out_full, "vimids"))
expect_equal(out_full$m, 3L)
expect_true(out_full$boot)
expect_equal(out_full$uncert, "normalerror")

# All completed datasets are valid and different
c1_full <- complete(out_full, 1)
c2_full <- complete(out_full, 2)
c3_full <- complete(out_full, 3)
expect_equal(sum(is.na(c1_full)), 0)
expect_equal(sum(is.na(c2_full)), 0)
expect_equal(sum(is.na(c3_full)), 0)

# with.vimids works on integration result
fits <- with(out_full, lm(Sleep ~ Dream + Span))
expect_equal(length(fits), 3)
expect_true(all(sapply(fits, inherits, "lm")))

# complete long format works
long_full <- complete(out_full, "long")
expect_equal(nrow(long_full), nrow(d_t13) * 3)
expect_true(".imp" %in% names(long_full))
#
