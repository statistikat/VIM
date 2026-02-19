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

# vimpute supports ranger median aggregation via ...", {
  set.seed(1)
  out_mean <- vimpute(sleep, method = "ranger", sequential = FALSE, imp_var = FALSE)
  set.seed(1)
  out_median <- vimpute(
    sleep,
    method = "ranger",
    sequential = FALSE,
    imp_var = FALSE,
    ranger_median = TRUE
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
