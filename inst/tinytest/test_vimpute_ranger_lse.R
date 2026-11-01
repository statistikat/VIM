if (!all(vapply(c("validate", "ECOSolveR", "ranger", "mlr3", "mlr3learners",
                  "mlr3pipelines", "mlr3tuning", "paradox", "R6", "future"),
                requireNamespace, logical(1), quietly = TRUE))) {
  exit_file("optional ranger/edit stack not installed")
}
if (!at_home()) exit_file("LSE integration checks run at home")

utils::data("lse_synthetic", package = "VIM")
utils::data("lse_synthetic_rules", package = "VIM")
lse_edit_rules <- validate::validator(.data = lse_synthetic_rules$edit)
edit_cols <- setdiff(names(lse_synthetic), c("unit_id", "enterprise_group_id"))

check_ranger <- function(n, missing_map) {
  original <- lse_synthetic[seq_len(n), edit_cols]
  masked <- original
  for (v in names(missing_map)) masked[missing_map[[v]], v] <- NA_real_
  fit <- vimpute(
    masked, method = "ranger", edit_rules = lse_edit_rules,
    learner_params = list(ranger = list(num.trees = 15L, num.threads = 1L)),
    sequential = FALSE, pmm = FALSE, seed = 8
  )
  completed <- as.data.frame(fit)[, edit_cols, drop = FALSE]
  for (v in names(missing_map)) {
    rows <- sort(missing_map[[v]])
    expect_equal(which(fit[[paste0(v, "_imp")]]), rows)
    expect_equal(completed[[v]][rows], original[[v]][rows], tolerance = 1e-7)
    expect_equal(completed[[v]][-rows], original[[v]][-rows])
  }
  checks <- validate::values(validate::confront(completed, lse_edit_rules))
  expect_equal(dim(checks), c(n, 92L))
  expect_true(all(checks))
}

check_ranger(80L, list(turnover_total = c(2L, 15L), intermediate_consumption = c(13L, 18L),
                       gross_value_added = c(24L, 31L), personnel_costs = c(39L, 45L),
                       investments_tangible = c(52L, 60L)))

set.seed(20260630)
rows <- sample.int(10000L, 600L)
overlap <- rows[seq_len(200L)]
check_ranger(10000L, list(turnover_total = overlap, intermediate_consumption = rows[201:400],
                          personnel_costs = rows[401:600]))
check_ranger(10000L, list(turnover_total = overlap, personnel_costs = overlap,
                          investments_tangible = overlap))
check_ranger(10000L, list(turnover_total = overlap, intermediate_consumption = overlap,
                          personnel_costs = overlap))
