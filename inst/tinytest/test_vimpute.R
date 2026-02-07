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
