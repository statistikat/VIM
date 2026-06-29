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
    learner_params = list(restricted = list(rules = rules))
  )

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

  utils::data("lse_synthetic", package = "VIM")
  rules_path <- system.file("data", "lse_synthetic_rules.rds", package = "VIM")
  if (!nzchar(rules_path)) {
    rules_path <- file.path(getwd(), "data", "lse_synthetic_rules.rds")
  }
  lse_rules <- readRDS(rules_path)
  lse_accounting_rules <- lse_rules$accounting
  lse_edit_rules <- lse_rules$edit

  numeric_cols <- c(
    "persons_employed",
    "employees_paid",
    "self_employed",
    "employees_male",
    "employees_female",
    "employees_blue_collar",
    "employees_white_collar",
    "apprentices",
    "marginal_employees",
    "turnover_total",
    "turnover_domestic",
    "turnover_exports",
    "e_commerce_turnover",
    "material_costs",
    "purchased_services",
    "rents_leasing",
    "other_operating_expense",
    "intermediate_consumption",
    "gross_value_added",
    "personnel_costs",
    "wages_salaries",
    "social_security_costs",
    "other_personnel_costs",
    "gross_operating_surplus",
    "investments_tangible",
    "investment_machinery",
    "investment_buildings",
    "investment_software"
  )
  edit_cols <- c(
    "reporting_year",
    "onace_section",
    "onace_group",
    "nuts2",
    "data_source",
    "survey_mode",
    "employment_size_class",
    "turnover_size_class",
    numeric_cols
  )
  lse_complete <- lse_synthetic[seq_len(80L), edit_cols]
  expect_true(all(validate::values(validate::confront(
    lse_synthetic,
    lse_edit_rules
  ))))

  lse_missing <- lse_complete[, numeric_cols]
  missing_map <- list(
    turnover_total = c(2L, 7L),
    intermediate_consumption = c(13L, 18L),
    gross_value_added = c(24L, 31L),
    personnel_costs = c(39L, 45L),
    investments_tangible = c(52L, 60L)
  )
  for (var in names(missing_map)) {
    lse_missing[missing_map[[var]], var] <- NA_real_
  }

  lse_imp <- vimpute(
    lse_missing,
    method = "restricted",
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(restricted = list(rules = lse_accounting_rules))
  )

  lse_complete[, numeric_cols] <- as.data.frame(lse_imp)[, numeric_cols]
  lse_rule_values <- validate::values(
    validate::confront(
      lse_complete,
      lse_edit_rules
    )
  )
  expected_nimp <- sum(lengths(missing_map))

  expect_false(anyNA(as.data.frame(lse_imp)[,
    names(missing_map),
    drop = FALSE
  ]))
  expect_equal(
    sum(lse_imp$turnover_total_imp),
    length(missing_map$turnover_total)
  )
  expect_equal(
    sum(lse_imp$intermediate_consumption_imp),
    length(missing_map$intermediate_consumption)
  )
  expect_equal(
    sum(lse_imp$gross_value_added_imp),
    length(missing_map$gross_value_added)
  )
  expect_equal(
    sum(lse_imp$personnel_costs_imp),
    length(missing_map$personnel_costs)
  )
  expect_equal(
    sum(lse_imp$investments_tangible_imp),
    length(missing_map$investments_tangible)
  )
  expect_equal(
    sum(as.data.frame(lse_imp)[paste0(names(missing_map), "_imp")]),
    expected_nimp
  )
  expect_true(all(lse_rule_values))
}
