if (!all(vapply(c("validate", "ECOSolveR"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("optional edit-rule packages not installed")
library(VIM)
project <- function(pred, rules, control = list(weights = "equal"), original = NULL, mask = NULL) {
  if (is.null(original)) { original <- pred; original[] <- lapply(original, function(x) rep(NA_real_, length(x))) }
  if (is.null(mask)) mask <- lapply(original, is.na)
  VIM:::.project_imputed_values(pred, original, mask, rules, control)
}

# Analytic bounds, equalities and weighted optima (no fitted model).
r <- project(data.frame(x = c(-2, 3, 12)), validate::validator(x >= 0, x <= 10),
             list(weights = "equal", save_optimization_problem = TRUE))
expect_equal(r$data$x, c(0, 3, 10), tolerance = 1e-6)
expect_equal(r$diagnostics$optimization_problems_solved, 2L)
expect_equal(r$diagnostics$records_already_satisfying, 1L)
expect_equal(r$problems[[2]]$original_predictions, c(x = 3))
expect_null(r$problems[[2]]$ecos_input)
expect_true(all(r$problems[[1]]$constraint_residuals <= 1e-6))
expect_true(r$problems[[1]]$solver_status %in% c(0, 10))
r <- project(data.frame(x = 1, y = 3), validate::validator(x + y == 10), list(weights = c(x = 1, y = 3)))
expect_equal(unlist(r$data), c(x = 5.5, y = 4.5), tolerance = 1e-6)
r <- project(data.frame(a = 0, b = 0, total = 0), validate::validator(total == a + b, a == 2 * b, total == 12))
expect_equal(unlist(r$data), c(a = 8, b = 4, total = 12), tolerance = 1e-6)
# Redundant equations must not change the optimum.
r <- project(data.frame(x = 1, y = 3), validate::validator(x + y == 10, 2*x + 2*y == 20))
expect_equal(unlist(r$data), c(x = 4, y = 6), tolerance = 1e-6)

# Observed offsets and coefficients vary by record, and excluded columns stay fixed.
p <- data.frame(x = c(0, 20), y = c(6, 3), ratio = c(2, 1), kind = c("A", "B"))
o <- p; o$x <- c(NA_real_, NA_real_)
r <- project(p, validate::validator(y <= ratio * x, if (kind == "A") x <= 4,
                                     kind == "A" | x <= 10), original = o)
expect_equal(r$data$x, c(3, 10), tolerance = 1e-6)
expect_equal(r$data[c("y", "ratio", "kind")], p[c("y", "ratio", "kind")])
r <- project(data.frame(x = 2, y = 3), validate::validator(x + y == 5))
expect_equal(r$data, data.frame(x = 2, y = 3))
expect_equal(r$diagnostics$optimization_problems_solved, 0L)

# Unsupported and infeasible edits fail explicitly, with record/rule context.
expect_error(project(data.frame(x = 1), validate::validator(x >= 2, x <= 0)), "record 1")
expect_error(project(data.frame(x = 1), validate::validator(x == 2, x == 3)), "equalities")
expect_error(project(data.frame(x = 1), validate::validator(x >= absent)), "missing columns")
expect_error(project(data.frame(x = 1, cap = NA_real_), validate::validator(x <= cap),
  original = data.frame(x = NA_real_, cap = NA_real_), mask = list(x = TRUE, cap = FALSE)), "Unavailable")
expect_error(project(data.frame(x = 1), validate::validator(x^2 <= 4)), "nonlinear")
expect_error(project(data.frame(x = 1, y = 1), validate::validator(x*y >= 3)), "nonlinear")
expect_error(project(data.frame(x = 1), validate::validator(x <= 0 | x >= 2)), "disjunction")
expect_error(project(data.frame(x = 1), validate::validator(if(x > 0) x >= 2)), "fixed variables")
expect_error(project(data.frame(x = 1L), validate::validator(x >= 0),
  original = data.frame(x = NA_integer_)), "Integer")
expect_error(project(data.frame(x = 1, y = 1), validate::validator(x+y >= 3),
  list(weights = c(x = 1))), "Missing edit weights")
expect_error(vimpute_edit_control(weights = c(x = 0)), "positive")
expect_error(vimpute_edit_control(weights = c(x = Inf)), "finite")
expect_error(vimpute_edit_control(tolerance = NA_real_), "tolerance")
expect_error(vimpute_edit_control(on_infeasible = "keep"), "only 'error'")
expect_error(VIM:::.edit_control(list(typo = 1)), "Unknown")
expect_error(VIM:::.edit_prepare(list()), "validator")
expect_error(project(data.frame(x = 2), validate::validator(x >= 0),
  original = data.frame(x = 1), mask = list(x = FALSE)), "Fixed observed")
expect_error(project(data.frame(x = -1), validate::validator(x >= 0),
  original = data.frame(x = -1), mask = list(x = FALSE)), "fixed values")
expect_equal(project(data.frame(x = 3), validate::validator(in_range(x, 0, 2)))$data$x, 2, tolerance = 1e-6)
expect_equal(VIM:::.edit_scale(rep(100, 4)), 1)
# Scaled weights make the optimum invariant to a change of measurement units.
p <- data.frame(a = c(1, 2, 3, 0), b = c(10, 20, 30, 0))
o <- p; o[4, ] <- NA_real_
expect_error(r <- project(p, validate::validator(a+b == 40), list(weights = "scaled"), o), "fixed values")
# Apply the identity only on the missing record using a fixed gate.
p$gate <- c(FALSE, FALSE, FALSE, TRUE); o$gate <- p$gate
r <- project(p, validate::validator(if(gate == TRUE) a + b == 40), list(weights = "scaled"), o)
expect_equal(r$data$a[4], 40 / 101, tolerance = 1e-5)
expect_equal(r$data$b[4], 4000 / 101, tolerance = 1e-5)

# Thousands of records: feasible records skip the solver, and saved problems
# have only record-sized matrices. No stochastic operation occurs in projection.
p <- data.frame(a = rep(2, 3000), b = rep(3, 3000)); p$a[1:10] <- -1
r <- project(p, validate::validator(a >= 0, a+b <= 10), list(weights = "equal"))
expect_equal(r$diagnostics$records_evaluated, 3000L)
expect_equal(r$diagnostics$optimization_problems_solved, 10L)
expect_null(r$problems)
r <- project(p[1:20, ], validate::validator(a >= 0, a+b <= 10),
  list(weights = "equal", save_optimization_problem = TRUE))
expect_true(all(vapply(r$problems, function(z) is.null(z$ecos_input) || ncol(z$ecos_input$G) == 3L, logical(1))))

err <- tryCatch(project(data.frame(x=1),validate::validator(x>=2,x<=0),
  list(weights="equal",save_optimization_problem=TRUE)),error=identity)
expect_true(inherits(err,"vimpute_edit_error"))
expect_equal(err$edit_diagnostics$infeasible_problems,1L)
expect_true(length(err$rule_names)>0)
expect_true(!is.null(err$edit_optimization_problem$constraints))
expect_error(VIM:::.project_imputed_values(data.frame(x=1),data.frame(x=NA_real_),
  list(TRUE),validate::validator(x>=0)),"Invalid imputation_mask")
