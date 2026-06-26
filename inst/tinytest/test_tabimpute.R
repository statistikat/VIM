library(tinytest)
library(VIM)

# availability helper is safe without optional Python dependencies
available <- tabimpute_available()
expect_true(is.logical(available))
expect_identical(length(available), 1L)

# no missing values return the input unchanged before Python is checked
x_complete <- data.frame(a = 1:3, b = c("a", "b", "c"), stringsAsFactors = FALSE)
expect_equal(tabimpute(x_complete), x_complete)

# input validation happens before backend import
x_missing <- data.frame(a = c(1, NA, 3))
expect_error(tabimpute(x_missing, variables = "missing"))
expect_error(tabimpute(x_missing, id_vars = "missing"))
expect_error(tabimpute(x_missing, variables = "a", id_vars = "a"))

x_list <- x_missing
x_list$list_col <- I(list(1, 2, 3))
expect_error(tabimpute(x_list))

if (!available) {
  expect_error(tabimpute(x_missing))
}

overwrite_backend <- function(x, mask, verbose = FALSE, ...) {
  for (nm in names(x)) {
    if (is.numeric(x[[nm]]) || is.integer(x[[nm]])) {
      x[[nm]][] <- 100
    } else {
      x[[nm]][] <- "filled"
    }
  }
  x
}

# only originally missing cells in selected variables are replaced
x_select <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 3))
out_select <- tabimpute(
  x_select,
  variables = "a",
  tabimpute_module = overwrite_backend,
  imp_var = FALSE
)
expect_equal(out_select$a, c(1, 100, 3))
expect_equal(out_select$b, x_select$b)

# indicator variables follow VIM's *_imp convention
out_indicator <- tabimpute(
  x_select,
  variables = "a",
  tabimpute_module = overwrite_backend
)
expect_true("a_imp" %in% names(out_indicator))
expect_identical(out_indicator$a_imp, c(FALSE, TRUE, FALSE))
expect_false("b_imp" %in% names(out_indicator))

# id variables are not sent to the backend and remain unchanged
x_id <- data.frame(id = c(1, NA, 3), a = c(1, NA, 3))
out_id <- tabimpute(
  x_id,
  id_vars = "id",
  tabimpute_module = overwrite_backend,
  imp_var = FALSE
)
expect_equal(out_id$id, x_id$id)
expect_equal(out_id$a, c(1, 100, 3))

type_backend <- function(x, mask, verbose = FALSE, ...) {
  x$num[2] <- 2.5
  x$int[2] <- 2.2
  x$fac[2] <- "b"
  x$ord[2] <- "medium"
  x$logi[2] <- TRUE
  x$date[2] <- as.numeric(as.Date("2020-01-02"))
  x$time[2] <- as.numeric(as.POSIXct("2020-01-02 12:00:00", tz = "UTC"))
  x
}

x_types <- data.frame(
  num = c(1, NA, 3),
  int = as.integer(c(1, NA, 3)),
  fac = factor(c("a", NA, "b"), levels = c("a", "b")),
  ord = ordered(c("low", NA, "high"), levels = c("low", "medium", "high")),
  logi = c(TRUE, NA, FALSE),
  date = as.Date(c("2020-01-01", NA, "2020-01-03")),
  time = as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-01-03 00:00:00"), tz = "UTC")
)

out_types <- tabimpute(x_types, tabimpute_module = type_backend, imp_var = FALSE)
expect_true(is.numeric(out_types$num))
expect_true(is.integer(out_types$int))
expect_true(is.factor(out_types$fac))
expect_true(is.ordered(out_types$ord))
expect_true(is.logical(out_types$logi))
expect_true(inherits(out_types$date, "Date"))
expect_true(inherits(out_types$time, "POSIXct"))
expect_equal(out_types$num[2], 2.5)
expect_identical(out_types$int[2], 2L)
expect_identical(as.character(out_types$fac[2]), "b")
expect_identical(as.character(out_types$ord[2]), "medium")
expect_identical(out_types$logi[2], TRUE)
expect_identical(out_types$date[2], as.Date("2020-01-02"))
expect_identical(out_types$time[2], as.POSIXct("2020-01-02 12:00:00", tz = "UTC"))

# data.table inputs keep their broad type
x_dt <- data.table::data.table(a = c(1, NA, 3))
out_dt <- tabimpute(
  x_dt,
  tabimpute_module = overwrite_backend,
  imp_var = FALSE
)
expect_true(data.table::is.data.table(out_dt))

# vimpute can dispatch explicitly to tabimpute
out_vimpute <- vimpute(
  x_select,
  method = "tabimpute",
  variables = "a",
  tabimpute_module = overwrite_backend,
  imp_var = FALSE
)
expect_equal(out_vimpute$a, c(1, 100, 3))
expect_equal(out_vimpute$b, x_select$b)

run_real_backend <- isTRUE(tabimpute_available()) &&
  identical(tolower(Sys.getenv("VIM_RUN_TABIMPUTE_BACKEND_TESTS")), "true")

if (run_real_backend) {
  x_real <- data.frame(
    a = c(1, 2, NA, 4, 5),
    b = c(2, NA, 6, 8, 10)
  )

  out_real <- tabimpute(x_real, imp_var = FALSE)
  expect_true(inherits(out_real, "data.frame"))
  expect_identical(dim(out_real), dim(x_real))
  expect_identical(names(out_real), names(x_real))
  expect_false(anyNA(out_real$a))
  expect_false(anyNA(out_real$b))
  expect_equal(out_real[!is.na(x_real)], x_real[!is.na(x_real)])
} else {
  message("Skipping real TabImpute backend tests. Set VIM_RUN_TABIMPUTE_BACKEND_TESTS=true to enable them.")
}
