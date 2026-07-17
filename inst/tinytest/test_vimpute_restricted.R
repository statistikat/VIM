if (
  requireNamespace("validate", quietly = TRUE) &&
    requireNamespace("ECOSolveR", quietly = TRUE)
) {
  expect_error(
    vimpute(
      data.frame(y = c(1, NA), x = c(1, 2)),
      method = list(y = "restricted"),
      pmm = FALSE,
      sequential = FALSE,
      learner_params = list(y = list())
    )
  )

  test_df <- data.frame(y = c(1, 2, NA), x = c(1, 2, 3))
  rules <- validate::validator(y >= 4)

  imp <- vimpute(
    test_df,
    method = list(y = "restricted"),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(restricted = list(
      rules = rules,
      save_optimization_problem = TRUE
    ))
  )

  expect_false(anyNA(imp$y))
  expect_true(imp$y[3] >= 4 - 1e-6)
  expect_true(imp$y_imp[3])
  optimization_problems <- attr(
    imp,
    "restricted_optimization_problems",
    exact = TRUE
  )
  expect_equal(names(optimization_problems), "y")
  expect_equal(length(optimization_problems$y), 1L)
  expect_equal(optimization_problems$y[[1L]]$solver, "ECOSolveR")
  expect_equal(
    names(optimization_problems$y[[1L]]$arguments),
    c("c", "G", "h", "dims", "A", "b", "control")
  )
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

  full_data <- full_data[, c("amount", "x1", "x2", "lower", "upper")]
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

  imp <- vimpute(
    data_with_missing,
    method = list(amount = "restricted"),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(amount = list(rules = rules))
  )

  rule_values <- validate::values(
    validate::confront(
      as.data.frame(imp)[, names(full_data), drop = FALSE],
      rules
    )
  )

  expect_false(anyNA(imp$amount))
  expect_equal(sum(imp$amount_imp), length(missing_idx))
  expect_true(all(rule_values))

  data_with_missing <- rbind(
    data_with_missing,
    data.frame(amount = NA, x1 = 3, x2 = 5, lower = 20.5, upper = 21)
  )

  imp <- vimpute(
    data_with_missing,
    method = list(amount = "restricted"),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(amount = list(rules = rules))
  )

  rule_values <- validate::values(
    validate::confront(
      as.data.frame(imp)[, names(full_data), drop = FALSE],
      rules
    )
  )

  expect_false(anyNA(imp$amount))
  expect_equal(sum(imp$amount_imp), length(missing_idx) + 1L)
  expect_true(all(rule_values))

  categorical_data <- data_with_missing
  categorical_data$c1 <- factor(
    rep(c("A", "B", "C"), length.out = nrow(categorical_data)),
    levels = c("A", "B", "C")
  )
  categorical_data$c2 <- factor(
    rep(c("B", "C", "A"), length.out = nrow(categorical_data)),
    levels = c("A", "B", "C")
  )
  categorical_data$c3 <- factor(
    rep(c("C", "A", "B"), length.out = nrow(categorical_data)),
    levels = c("A", "B", "C")
  )
  categorical_data <- rbind(
    categorical_data,
    data.frame(
      x1 = 4,
      x2 = 4,
      amount = NA,
      lower = 1,
      upper = 15,
      c1 = "A",
      c2 = "B",
      c3 = "C"
    )
  )
  categorical_rules <- validate::validator(
    amount >= lower,
    amount <= upper,
    amount >= 0,
    lower <= upper,
    (c1 == "A") <= 2,
    (c2 == "A") <= 2,
    (c3 == "A") <= 2
  )

  imp <- vimpute(
    categorical_data,
    method = list(amount = "restricted"),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(amount = list(rules = categorical_rules))
  )

  rule_values <- validate::values(
    validate::confront(
      as.data.frame(imp)[, c(names(full_data), "c1", "c2", "c3"), drop = FALSE],
      categorical_rules
    )
  )

  expect_false(anyNA(imp$amount))
  expect_equal(sum(imp$amount_imp), length(missing_idx) + 2L)
  expect_true(all(rule_values))

  conditional_rules <- validate::validator(
    amount >= lower,
    amount <= upper,
    amount >= 0,
    lower <= upper,
    (c1 == "A") + (c2 == "A") + (c3 == "A") <= 2,
    if (c1 == "A") amount >= 2
  )

  imp <- vimpute(
    categorical_data,
    method = list(amount = "restricted"),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(amount = list(rules = conditional_rules))
  )

  rule_values <- validate::values(
    validate::confront(
      as.data.frame(imp)[, c(names(full_data), "c1", "c2", "c3"), drop = FALSE],
      conditional_rules
    )
  )

  expect_false(anyNA(imp$amount))
  expect_equal(sum(imp$amount_imp), length(missing_idx) + 2L)
  expect_true(any(imp$amount_imp & imp$c1 == "A"))
  expect_true(all(rule_values))

  n_formula <- 80L
  formula_data <- data.frame(
    y = 10 + 2 * seq_len(n_formula),
    x_signal = seq_len(n_formula),
    x_noise = 10 + 2 * seq_len(n_formula),
    lower = 0,
    upper = 200
  )
  formula_missing_idx <- c(12L, 24L, 36L)
  expected_formula_y <- formula_data$y[formula_missing_idx]
  formula_data$y[formula_missing_idx] <- NA_real_
  formula_data$x_noise[formula_missing_idx] <- -1000
  formula_rules <- validate::validator(y >= lower, y <= upper, lower <= upper)

  formula_imp <- vimpute(
    formula_data,
    method = list(y = "restricted"),
    formula = list(y = y ~ x_signal),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(y = list(rules = formula_rules))
  )

  expect_false(anyNA(formula_imp$y))
  expect_equal(sum(formula_imp$y_imp), length(formula_missing_idx))
  expect_true(
    max(abs(formula_imp$y[formula_missing_idx] - expected_formula_y)) < 1e-6
  )
  expect_true(all(validate::values(validate::confront(
    formula_imp,
    formula_rules
  ))))

  robust_rules <- validate::validator(y >= 0)
  clean_data <- data.frame(x = 0:21, y = 1 + 2 * (0:21))
  clean_data$y[22L] <- NA_real_

  clean_ordinary <- vimpute(
    clean_data,
    method = list(y = "restricted"),
    formula = list(y = y ~ x),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(y = list(rules = robust_rules))
  )
  clean_robust <- vimpute(
    clean_data,
    method = list(y = "restricted"),
    formula = list(y = y ~ x),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(y = list(
      rules = robust_rules,
      robust = TRUE
    ))
  )

  expect_equal(clean_robust$y[22L], clean_ordinary$y[22L], tolerance = 1e-5)

  outlier_data <- clean_data
  outlier_data$y[21L] <- 500
  outlier_ordinary <- vimpute(
    outlier_data,
    method = list(y = "restricted"),
    formula = list(y = y ~ x),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(y = list(rules = robust_rules))
  )
  outlier_robust <- vimpute(
    outlier_data,
    method = list(y = "restricted"),
    formula = list(y = y ~ x),
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(y = list(
      rules = robust_rules,
      robust = TRUE,
      huber_k = 1.345
    ))
  )

  expected_y <- 43
  expect_true(
    abs(outlier_robust$y[22L] - expected_y) <
      abs(outlier_ordinary$y[22L] - expected_y)
  )
  expect_true(validate::values(validate::confront(
    outlier_robust,
    robust_rules
  ))[[1L]])
}
