if (
  requireNamespace("validate", quietly = TRUE) &&
    requireNamespace("ECOSolveR", quietly = TRUE)
) {
  utils::data("lse_synthetic", package = "VIM")
  utils::data("lse_synthetic_rules", package = "VIM")
  lse_edit_rules <- lse_synthetic_rules$edit

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
  expect_equal(length(lse_edit_rules), 92L)
  expect_true(all(validate::values(validate::confront(
    lse_complete,
    lse_edit_rules
  ))))

  lse_missing <- lse_complete
  missing_map <- list(
    turnover_total = c(2L, 15L),
    intermediate_consumption = c(13L, 18L),
    gross_value_added = c(24L, 31L),
    personnel_costs = c(39L, 45L),
    investments_tangible = c(52L, 60L)
  )
  for (var in names(missing_map)) {
    lse_missing[missing_map[[var]], var] <- NA_real_
  }
  restricted_formulas <- setNames(
    lapply(names(missing_map), function(var) {
      stats::reformulate(setdiff(numeric_cols, var), response = var)
    }),
    names(missing_map)
  )

  lse_imp <- vimpute(
    lse_missing,
    method = "restricted",
    formula = restricted_formulas,
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(
      restricted = list(
        rules = lse_edit_rules,
        save_optimization_problem = TRUE
      )
    )
  )

  lse_completed <- as.data.frame(lse_imp)[, edit_cols, drop = FALSE]
  lse_rule_values <- validate::values(
    validate::confront(lse_completed, lse_edit_rules)
  )
  expected_nimp <- sum(lengths(missing_map))

  expect_false(anyNA(lse_completed[, names(missing_map), drop = FALSE]))
  for (var in names(missing_map)) {
    expect_equal(
      sum(lse_imp[[paste0(var, "_imp")]]),
      length(missing_map[[var]])
    )
  }
  expect_equal(
    sum(as.data.frame(lse_imp)[paste0(names(missing_map), "_imp")]),
    expected_nimp
  )
  expect_true(all(lse_rule_values))
  expect_equal(
    sort(names(attr(lse_imp, "restricted_optimization_problems"))),
    sort(names(missing_map))
  )

  lse_missing2 <- lse_synthetic[seq_len(10000L), edit_cols]
  set.seed(20260630)
  missing_rows <- sample.int(10000L, 600L)
  missing_map <- list(
    turnover_total = missing_rows[seq_len(200L)],
    intermediate_consumption = missing_rows[201:400],
    personnel_costs = missing_rows[401:600]
  )
  for (var in names(missing_map)) {
    lse_missing2[missing_map[[var]], var] <- NA_real_
  }
  restricted_formulas <- list(
    turnover_total = turnover_total ~ persons_employed +
      employees_paid +
      self_employed +
      employees_male +
      employees_female +
      employees_blue_collar +
      employees_white_collar +
      apprentices +
      marginal_employees +
      turnover_domestic +
      turnover_exports +
      e_commerce_turnover +
      material_costs +
      purchased_services +
      rents_leasing +
      other_operating_expense +
      intermediate_consumption +
      gross_value_added +
      personnel_costs +
      wages_salaries +
      social_security_costs +
      other_personnel_costs +
      gross_operating_surplus +
      investments_tangible +
      investment_machinery +
      investment_buildings +
      investment_software,
    intermediate_consumption = intermediate_consumption ~ persons_employed +
      employees_paid +
      self_employed +
      employees_male +
      employees_female +
      employees_blue_collar +
      employees_white_collar +
      apprentices +
      marginal_employees +
      turnover_total +
      turnover_domestic +
      turnover_exports +
      e_commerce_turnover +
      material_costs +
      purchased_services +
      rents_leasing +
      other_operating_expense +
      gross_value_added +
      personnel_costs +
      wages_salaries +
      social_security_costs +
      other_personnel_costs +
      gross_operating_surplus +
      investments_tangible +
      investment_machinery +
      investment_buildings +
      investment_software,
    personnel_costs = personnel_costs ~ persons_employed +
      employees_paid +
      self_employed +
      employees_male +
      employees_female +
      employees_blue_collar +
      employees_white_collar +
      apprentices +
      marginal_employees +
      turnover_total +
      turnover_domestic +
      turnover_exports +
      e_commerce_turnover +
      material_costs +
      purchased_services +
      rents_leasing +
      other_operating_expense +
      intermediate_consumption +
      gross_value_added +
      wages_salaries +
      social_security_costs +
      other_personnel_costs +
      gross_operating_surplus +
      investments_tangible +
      investment_machinery +
      investment_buildings +
      investment_software
  )

  lse_imp <- vimpute(
    lse_missing2,
    method = "restricted",
    formula = restricted_formulas,
    pmm = FALSE,
    sequential = FALSE,
    learner_params = list(
      restricted = list(
        rules = lse_edit_rules,
        save_optimization_problem = TRUE
      )
    )
  )

  lse_completed <- as.data.frame(lse_imp)[, edit_cols, drop = FALSE]
  lse_rule_values <- validate::values(
    validate::confront(lse_completed, lse_edit_rules)
  )
  expected_nimp <- sum(lengths(missing_map))

  expect_false(anyNA(lse_completed[, names(missing_map), drop = FALSE]))
  for (var in names(missing_map)) {
    expect_equal(
      sum(lse_imp[[paste0(var, "_imp")]]),
      length(missing_map[[var]])
    )
  }
  expect_equal(
    sum(as.data.frame(lse_imp)[paste0(names(missing_map), "_imp")]),
    expected_nimp
  )
  expect_true(all(lse_rule_values))
  expect_equal(
    sort(names(attr(lse_imp, "restricted_optimization_problems"))),
    sort(names(missing_map))
  )
}
