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

source("R/helper_vimpute.R")
source("R/vimmi.R")
source("R/vimpute.R")

check <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
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
    y_semi = ifelse(runif(n) < 0.35, 0, exp(0.4 + x_num + rnorm(n, sd = 0.1))),
    y_fac = factor(ifelse(x_num + x_aux + rnorm(n, sd = 0.2) > 0.5, "yes", "no"))
  )

  data[sample(.N, 10L), y_num := NA_real_]
  data[sample(.N, 8L), y_semi := NA_real_]
  data[sample(.N, 8L), y_fac := NA]
  data[sample(.N, 6L), x_num := NA_real_]
  data[sample(.N, 6L), x_fac := NA]
  data
}

run_case <- function(label, expr) {
  cat("\n---", label, "---\n")
  force(expr)
  cat("ok\n")
}

set.seed(42)
future::plan(future::sequential)

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

cat("\nAll vimpute smoke tests passed.\n")
