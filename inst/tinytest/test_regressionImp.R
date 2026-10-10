## VIM suggests the mlr3 stack rather than importing it: it backs vimpute()
## alone, and a hard dependency would put every reverse dependency of VIM at
## the mercy of the mlr3 chain. This file exercises that path, so it cannot
## run when the stack is absent -- e.g. under _R_CHECK_DEPENDS_ONLY_=true.
if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                  "paradox", "R6", "future"),
                requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed (VIM only suggests it)")

# regressionImp works with integer and double columns", {
  test_df <- data.frame(col_int = c(1L, NA, 3L, 4L, 5L), col_dbl = c(1.1, 2, NA, 4, 5))
  imp_col_dbl <- regressionImp(col_dbl ~ col_int, test_df)
  expect_false(anyNA(imp_col_dbl$col_dbl))
  imp_val_dbl <- imp_col_dbl[3, "col_dbl"]
  expect_true(2 < imp_val_dbl && imp_val_dbl < 4)

  imp_col_int <- regressionImp(col_int ~ col_dbl, test_df)
  expect_false(anyNA(imp_col_int$col_int))
  imp_val_int <- imp_col_int[2, "col_int"]
  expect_true(1 < imp_val_int && imp_val_int < 3)
# 

# regressionImp works with link function", {
  dataset <- sleep[, c("Dream", "NonD", "BodyWgt", "Span")]
  dataset$BodyWgt <- log(dataset$BodyWgt)
  lapply(dataset, class)
  imp_regression <- regressionImp(Span ~ BodyWgt, dataset,family=Gamma(link="log"))
  expect_false(any(is.na(imp_regression$Span)))

# 

# regressionImp handles the VisualImp single-predictor call", {
  dataset <- sleep[, c("Span", "NonD", "Sleep")]
  imp_regression <- regressionImp(NonD ~ Sleep, dataset)
  expect_true(is.data.frame(imp_regression))
  expect_identical(nrow(imp_regression), nrow(dataset))
  expect_true(sum(is.na(imp_regression$NonD)) < sum(is.na(dataset$NonD)))
  expect_true(sum(imp_regression$NonD_imp) > 0)

# 
