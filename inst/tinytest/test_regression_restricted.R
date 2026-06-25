if (requireNamespace("validate", quietly = TRUE)) {
  expect_error(
    regression_restricted(
      y ~ x,
      data.frame(y = c(1, NA), x = c(1, 2)),
      rules = list()
    )
  )
}

if (
  requireNamespace("validate", quietly = TRUE) &&
    requireNamespace("ECOSolveR", quietly = TRUE)
) {
  test_df <- data.frame(y = c(1, 2, NA), x = c(1, 2, 3))
  rules <- validate::validator(y >= 4)

  imp <- regression_restricted(y ~ x, test_df, rules = rules)

  expect_false(anyNA(imp$y))
  expect_true(imp$y[3] >= 4 - 1e-6)
  expect_true(imp$y_imp[3])
}

if (
  requireNamespace("validate", quietly = TRUE) &&
    requireNamespace("ECOSolveR", quietly = TRUE)
) {
  set.seed(42)
  n <- 500L
  full_data <- data.frame(
    x1 = stats::runif(n, 0, 10),
    x2 = stats::runif(n, -2, 2)
  )
  full_data$amount <- 20 + 1.5 * full_data$x1 - 0.75 * full_data$x2
  full_data$lower <- full_data$amount - 0.5
  full_data$upper <- full_data$amount + 0.5

  rules <- validate::validator(
    amount >= lower,
    amount <= upper,
    amount >= 0,
    lower <= upper
  )

  expect_true(all(validate::values(validate::confront(full_data, rules))))

  missing_idx <- sample.int(n, 125L)
  data_with_missing <- full_data
  data_with_missing$amount[missing_idx] <- NA_real_

  imp <- regression_restricted(
    amount ~ x1 + x2 + lower + upper,
    data_with_missing,
    rules = rules
  )

  rule_values <- validate::values(
    validate::confront(imp[, names(full_data), drop = FALSE], rules)
  )

  expect_false(anyNA(imp$amount))
  expect_equal(sum(imp$amount_imp), length(missing_idx))
  expect_true(all(rule_values))

  data_with_missing <- rbind(
    data_with_missing,
    data.frame(amount = NA, x1 = 3, x2 = 5, lower = 20.5, upper = 21)
  )

  imp <- regression_restricted(
    amount ~ x1 + x2,
    data_with_missing,
    rules = rules
  )

  rule_values <- validate::values(
    validate::confront(imp[, names(full_data), drop = FALSE], rules)
  )

  expect_false(anyNA(imp$amount))
  expect_equal(sum(imp$amount_imp), length(missing_idx) + 1L)
  expect_true(all(rule_values))
}
