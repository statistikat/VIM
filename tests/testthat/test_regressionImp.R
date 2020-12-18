test_that("regressionImp works with integer and double columns", {
  test_df <- data.frame(col_int = c(1L, NA, 3L, 4L, 5L), col_dbl = c(1.1, 2, NA, 4, 5))
  imp_col_dbl <- regressionImp(col_dbl ~ col_int, test_df)
  expect_false(anyNA(imp_col_dbl$col_dbl))
  imp_val_dbl <- imp_col_dbl[3, "col_dbl"]
  expect_true(2 < imp_val_dbl && imp_val_dbl < 4)

  imp_col_int <- regressionImp(col_int ~ col_dbl, test_df)
  expect_false(anyNA(imp_col_int$col_int))
  imp_val_int <- imp_col_int[2, "col_int"]
  expect_true(1 < imp_val_int && imp_val_int < 3)
})

test_that("regressionImp works with link function", {
  dataset <- sleep[, c("Dream", "NonD", "BodyWgt", "Span")]
  dataset$BodyWgt <- log(dataset$BodyWgt)
  lapply(dataset, class)
  imp_regression <- regressionImp(Span ~ BodyWgt, dataset,family=Gamma(link="log"))
  expect_false(any(is.na(imp_regression$Span)))

})