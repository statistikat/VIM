library(VIM)
library(data.table)

collect_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

make_issue98_data <- function() {
  df <- as.data.table(iris)
  setnames(df, c("S.Length", "S.Width", "P.Length", "P.Width", "Species"))
  set.seed(1)
  miss <- data.frame(
    row = sample(nrow(df), size = 50, replace = TRUE),
    col = sample(ncol(df), size = 50, replace = TRUE)
  )
  miss <- miss[!duplicated(miss), ]
  df[as.matrix(miss)] <- NA
  df
}

# Regression test for issue #98: xgboost + factor predictors should not
# construct supervised mlr3 tasks with missing targets.
issue98_data <- make_issue98_data()
issue98_method <- setNames(as.list(rep("xgboost", ncol(issue98_data))), names(issue98_data))

issue98_out <- collect_warnings(vimpute(
  data = issue98_data,
  method = issue98_method,
  sequential = FALSE,
  pred_history = TRUE,
  imp_var = FALSE
))

expect_true(is.list(issue98_out$value))
expect_false(any(grepl("Target column '.*'( contains| must not contain) missing values", issue98_out$warnings)))
expect_equal(sum(is.na(issue98_out$value$data)), 0)

# The formula/model-matrix preprocessing path had the same target-NA problem.
formula_data <- as.data.table(iris[, c("Sepal.Length", "Sepal.Width", "Species")])
setnames(formula_data, c("S.Length", "S.Width", "Species"))
set.seed(2)
formula_data[sample(nrow(formula_data), 12), S.Length := NA]

formula_out <- collect_warnings(vimpute(
  data = formula_data,
  considered_variables = c("S.Length", "S.Width", "Species"),
  method = list(S.Length = "regularized"),
  formula = list(S.Length ~ S.Width + Species),
  sequential = FALSE,
  imp_var = FALSE
))

expect_true(inherits(formula_out$value, "data.frame"))
expect_false(any(grepl("Target column '.*'( contains| must not contain) missing values", formula_out$warnings)))
expect_equal(sum(is.na(formula_out$value$S.Length)), 0)
