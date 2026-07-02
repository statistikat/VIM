required_pkgs <- c(
  "data.table",
  "future",
  "mlr3",
  "mlr3pipelines",
  "mlr3learners",
  "mlr3tuning",
  "paradox",
  "ranger"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0L) {
  message("Skipping vimpute smoke test. Missing packages: ", paste(missing_pkgs, collapse = ", "))
  quit(status = 0L)
}

suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(mlr3)
  library(mlr3pipelines)
  library(mlr3learners)
  library(mlr3tuning)
  library(paradox)
  library(ranger)
})

if (file.exists(file.path("R", "vimpute.R"))) {
  source("R/helper_vimpute.R")
  source("R/vimmi.R")
  source("R/vimpute.R")
} else {
  suppressPackageStartupMessages(library(VIM))
}

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

has_pkgs <- function(pkgs) {
  all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))
}

expect_warning <- function(expr, pattern) {
  seen <- FALSE
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(pattern, conditionMessage(w))) {
        seen <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  check(seen, paste("Expected warning matching pattern:", pattern))
  value
}

expect_error <- function(expr, pattern) {
  error_message <- NULL
  tryCatch(
    force(expr),
    error = function(e) {
      error_message <<- conditionMessage(e)
    }
  )
  check(!is.null(error_message), "Expected an error but none was thrown.")
  check(grepl(pattern, error_message), paste("Unexpected error message:", error_message))
}

make_smoke_data <- function(n = 80L) {
  set.seed(20260701)

  x_num <- rnorm(n)
  x_aux <- runif(n)
  x_fac <- factor(sample(c("a", "b", "c"), n, replace = TRUE))

  data <- data.table(
    x_num = x_num,
    x_aux = x_aux,
    x_fac = x_fac,
    y_num = 2 + x_num - 0.5 * x_aux + rnorm(n, sd = 0.15),
    y_pos = exp(0.5 + 0.3 * x_num + 0.2 * x_aux + rnorm(n, sd = 0.08)),
    y_semi = ifelse(runif(n) < 0.35, 0, exp(0.4 + x_num + rnorm(n, sd = 0.1))),
    y_fac = factor(ifelse(x_num + x_aux + rnorm(n, sd = 0.2) > 0.5, "yes", "no")),
    y_multi = factor(ifelse(x_num < -0.4, "low", ifelse(x_num > 0.6, "high", "mid")),
                     levels = c("low", "mid", "high"))
  )

  data[sample(.N, 10L), y_num := NA_real_]
  data[sample(.N, 8L), y_pos := NA_real_]
  data[sample(.N, 8L), y_semi := NA_real_]
  data[sample(.N, 8L), y_fac := NA]
  data[sample(.N, 8L), y_multi := NA]
  data[sample(.N, 6L), x_num := NA_real_]
  data[sample(.N, 6L), x_fac := NA]
  data
}

run_case <- function(label, expr) {
  cat("\n---", label, "---\n")
  withCallingHandlers(
    force(expr),
    warning = function(w) {
      if (grepl("^Package '.+' required but not installed for Learner", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  cat("ok\n")
}

run_optional_case <- function(label, pkgs, expr) {
  if (!has_pkgs(pkgs)) {
    cat("\n---", label, "---\n")
    cat("skipped: missing optional packages ", paste(pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)], collapse = ", "), "\n", sep = "")
    return(invisible(FALSE))
  }
  run_case(label, expr)
}

set.seed(42)
future::plan(future::sequential)

# Checks the basic single-imputation path with numeric ranger prediction,
# imputation flags, and prediction history output.
run_case("numeric ranger with imp_var and pred_history", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    sequential = FALSE,
    imp_var = TRUE,
    pred_history = TRUE,
    verbose = FALSE
  )

  check(is.list(result), "pred_history should return a list result.")
  check(is.data.table(result$data), "result$data should be a data.table.")
  check("pred_history" %in% names(result), "prediction history is missing.")
  check(!anyNA(result$data$y_num), "numeric target still contains missing values.")
  check("y_num_imp" %in% names(result$data), "imputation flag column is missing.")
})

# Checks score-based PMM in sequential mode and verifies that the numeric target
# is fully completed.
run_case("PMM uses donor values and returns completed numeric target", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    pmm = list(y_num = TRUE),
    pmm_k = list(y_num = 3L),
    pmm_k_method = list(y_num = "mean"),
    sequential = TRUE,
    nseq = 2L,
    verbose = FALSE
  )

  check(is.data.table(result), "PMM case should return a data.table.")
  check(!anyNA(result$y_num), "PMM target still contains missing values.")
})

# Checks the two-part semicontinuous path: zero/positive classification plus
# positive-value regression.
run_case("semicontinuous target", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_semi"),
    method = list(y_semi = "ranger"),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "semicontinuous case should return a data.table.")
  check(!anyNA(result$y_semi), "semicontinuous target still contains missing values.")
})

# Checks classification imputation and verifies that factor targets preserve
# their factor type.
run_case("factor target", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_fac"),
    method = list(y_fac = "ranger"),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "factor case should return a data.table.")
  check(is.factor(result$y_fac), "factor target should remain a factor.")
  check(!anyNA(result$y_fac), "factor target still contains missing values.")
})

# Checks multiclass classification with three factor levels.
run_case("multiclass factor target", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_multi"),
    method = list(y_multi = "ranger"),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "multiclass case should return a data.table.")
  check(is.factor(result$y_multi), "multiclass target should remain a factor.")
  check(!anyNA(result$y_multi), "multiclass target still contains missing values.")
  check(identical(levels(result$y_multi), c("low", "mid", "high")),
        "multiclass factor levels changed unexpectedly.")
})

# Checks the sequential loop, convergence bookkeeping, and prediction-history
# column structure.
run_case("sequential convergence with prediction history", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    sequential = TRUE,
    nseq = 3L,
    pred_history = TRUE,
    verbose = FALSE
  )

  check(is.list(result), "sequential pred_history should return a list result.")
  check(is.data.table(result$data), "sequential result$data should be a data.table.")
  check("pred_history" %in% names(result), "sequential prediction history is missing.")
  check(!anyNA(result$data$y_num), "sequential numeric target still contains missing values.")
  check(all(c("iteration", "variable", "index", "predicted_values") %in% names(result$pred_history)),
        "prediction history has unexpected columns.")
})

# Checks multiple imputation output, vimmi construction, and complete() methods.
run_case("multiple imputation returns vimmi and complete datasets", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    pmm = list(y_num = TRUE),
    pmm_k = list(y_num = 2L),
    sequential = FALSE,
    imp_var = FALSE,
    m = 2L,
    verbose = FALSE
  )

  check(inherits(result, "vimmi"), "m > 1 should return a vimmi object.")
  check(identical(result$m, 2L), "vimmi object should store m = 2.")

  completed_one <- complete(result, 1L)
  completed_all <- complete(result, "all")
  completed_long <- complete(result, "long")

  check(is.data.frame(completed_one), "complete(vimmi, 1) should return a data.frame.")
  check(length(completed_all) == 2L, "complete(vimmi, 'all') should return two datasets.")
  check(nrow(completed_long) == 2L * nrow(data), "complete(vimmi, 'long') has unexpected row count.")
  check(!anyNA(completed_one$y_num), "completed numeric target still contains missing values.")
})

# Checks stochastic uncertainty donor paths that are independent from explicit
# pmm = TRUE settings.
run_case("uncertainty pmm and midastouch", {
  data_pmm <- make_smoke_data()
  result_pmm <- vimpute(
    data = copy(data_pmm),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    pmm = FALSE,
    uncert = "pmm",
    sequential = FALSE,
    verbose = FALSE
  )
  check(!anyNA(result_pmm$y_num), "uncert = 'pmm' target still contains missing values.")

  data_midastouch <- make_smoke_data()
  result_midastouch <- vimpute(
    data = copy(data_midastouch),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    pmm = FALSE,
    uncert = "midastouch",
    sequential = FALSE,
    verbose = FALSE
  )
  check(!anyNA(result_midastouch$y_num), "uncert = 'midastouch' target still contains missing values.")
})

# Checks bootstrap refits together with normal-error and residual uncertainty.
run_case("bootstrap with normalerror and residual uncertainty", {
  data_normal <- make_smoke_data()
  result_normal <- vimpute(
    data = copy(data_normal),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    boot = TRUE,
    robustboot = "standard",
    uncert = "normalerror",
    sequential = FALSE,
    verbose = FALSE
  )
  check(!anyNA(result_normal$y_num), "bootstrap + normalerror target still contains missing values.")

  data_resid <- make_smoke_data()
  result_resid <- vimpute(
    data = copy(data_resid),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    boot = TRUE,
    robustboot = "standard",
    uncert = "resid",
    sequential = FALSE,
    verbose = FALSE
  )
  check(!anyNA(result_resid$y_num), "bootstrap + resid target still contains missing values.")
})

# Checks makeNA semantics: configured sentinel values are imputed, while regular
# NAs in the same variable remain untouched.
run_case("makeNA imputes sentinel values only", {
  data <- make_smoke_data()
  original_na_idx <- which(is.na(data$y_num))
  sentinel_idx <- which(!is.na(data$y_num))[seq_len(5L)]
  data[sentinel_idx, y_num := -999]

  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    makeNA = list(y_num = -999),
    sequential = FALSE,
    verbose = FALSE
  )

  check(!any(result$y_num[sentinel_idx] == -999, na.rm = TRUE), "makeNA sentinel values were not imputed.")
  check(all(is.na(result$y_num[original_na_idx])), "regular NAs should remain untouched when makeNA is used for the variable.")
})

# Checks donorcond parsing and donor filtering without changing donorcond logic.
run_case("donorcond restricts training donors without breaking imputation", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    donorcond = list(y_num = "> quantile(x, 0.2, na.rm = TRUE)"),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "donorcond case should return a data.table.")
  check(!anyNA(result$y_num), "donorcond target still contains missing values.")
})

# Checks formula left-hand-side back-transformation for log-transformed targets
# when formula-capable GAM learners are available.
run_optional_case("formula with log-transformed target", c("mgcv"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_pos"),
    method = list(y_pos = "gam"),
    formula = list(y_pos = log(y_pos) ~ x_num + x_aux + x_fac),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "log formula case should return a data.table.")
  check(!anyNA(result$y_pos), "log-transformed target still contains missing values.")
  check(all(result$y_pos > 0, na.rm = TRUE), "log back-transformation produced non-positive values.")
})

# Checks formula left-hand-side back-transformation for square-root transformed
# targets when formula-capable GAM learners are available.
run_optional_case("formula with sqrt-transformed target", c("mgcv"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_pos"),
    method = list(y_pos = "gam"),
    formula = list(y_pos = sqrt(y_pos) ~ x_num + x_aux + x_fac),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "sqrt formula case should return a data.table.")
  check(!anyNA(result$y_pos), "sqrt-transformed target still contains missing values.")
  check(all(result$y_pos >= 0, na.rm = TRUE), "sqrt back-transformation produced negative values.")
})

# Checks custom PMM aggregation functions.
run_case("PMM with custom aggregation function", {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "ranger"),
    pmm = list(y_num = TRUE),
    pmm_k = list(y_num = 4L),
    pmm_k_method = list(y_num = function(x) mean(x) + 0),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "custom PMM aggregation case should return a data.table.")
  check(!anyNA(result$y_num), "custom PMM aggregation target still contains missing values.")
})

# Checks expected warnings and errors for invalid or conflicting user inputs.
run_case("expected warnings and errors", {
  data <- make_smoke_data()

  expect_warning(
    vimpute(
      data = copy(data),
      considered_variables = c("x_num", "x_aux", "x_fac", "y_fac"),
      method = list(y_fac = "ranger"),
      pmm = list(y_fac = TRUE),
      sequential = FALSE,
      verbose = FALSE
    ),
    "PMM not possible for non-numeric variable"
  )

  expect_warning(
    vimpute(
      data = copy(data),
      considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
      method = list(y_num = "ranger"),
      m = 2L,
      boot = FALSE,
      uncert = "none",
      pmm = FALSE,
      verbose = FALSE
    ),
    "m > 1 without 'boot', 'uncert', or 'pmm'"
  )

  expect_error(
    vimpute(
      data = copy(data),
      considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
      method = list(y_num = "ranger"),
      makeNA = list(unknown = -999),
      verbose = FALSE
    ),
    "Unknown variable name\\(s\\) in 'makeNA'"
  )

  expect_error(
    vimpute(
      data = copy(data),
      considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
      method = list(y_num = "ranger"),
      donorcond = list(y_num = ">> 0"),
      verbose = FALSE
    ),
    "Invalid donor condition"
  )
})

# Checks the GAM regression learner when mgcv is available.
run_optional_case("GAM numeric target", c("mgcv"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "gam"),
    formula = list(y_num = y_num ~ x_num + x_aux + x_fac),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "GAM case should return a data.table.")
  check(!anyNA(result$y_num), "GAM numeric target still contains missing values.")
})

# Checks the GAM classification learner and factor-level preservation when mgcv
# is available.
run_optional_case("GAM factor target", c("mgcv"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_fac"),
    method = list(y_fac = "gam"),
    formula = list(y_fac = y_fac ~ x_num + x_aux + x_fac),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "GAM factor case should return a data.table.")
  check(is.factor(result$y_fac), "GAM factor target should remain a factor.")
  check(!anyNA(result$y_fac), "GAM factor target still contains missing values.")
})

# Checks the robust regression learner when robustbase is available.
run_optional_case("robust numeric target", c("robustbase"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "robust"),
    formula = list(y_num = y_num ~ x_num + x_aux + x_fac),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "robust case should return a data.table.")
  check(!anyNA(result$y_num), "robust numeric target still contains missing values.")
})

# Checks the robust GAM regression path when both mgcv and robustbase are
# available.
run_optional_case("RobGAM numeric target", c("mgcv", "robustbase"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "robgam"),
    formula = list(y_num = y_num ~ x_num + x_aux + x_fac),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "RobGAM case should return a data.table.")
  check(!anyNA(result$y_num), "RobGAM numeric target still contains missing values.")
})

# Checks the regularized/glmnet path when glmnet is available.
run_optional_case("regularized numeric target", c("glmnet"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "regularized"),
    formula = list(y_num = y_num ~ x_num + x_aux + x_fac),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "regularized case should return a data.table.")
  check(!anyNA(result$y_num), "regularized numeric target still contains missing values.")
})

# Checks the xgboost regression path when xgboost is available.
run_optional_case("xgboost numeric target", c("xgboost"), {
  data <- make_smoke_data()
  result <- vimpute(
    data = copy(data),
    considered_variables = c("x_num", "x_aux", "x_fac", "y_num"),
    method = list(y_num = "xgboost"),
    sequential = FALSE,
    verbose = FALSE
  )

  check(is.data.table(result), "xgboost case should return a data.table.")
  check(!anyNA(result$y_num), "xgboost numeric target still contains missing values.")
})

cat("\nAll vimpute smoke tests passed.\n")
