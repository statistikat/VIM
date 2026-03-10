library(VIM)

# --- Constructor ---

# vimmi constructor creates valid object
set.seed(1)
d <- data.frame(x = c(1, NA, 3, NA, 5), y = c(NA, 2, NA, 4, 5))
where <- is.na(d)
imp <- list(
  x = data.frame(m1 = c(2.1, 4.2), m2 = c(1.9, 3.8)),
  y = data.frame(m1 = c(1.0, 3.1), m2 = c(1.2, 2.9))
)

obj <- VIM:::new_vimmi(
  data = d, imp = imp, where = where, m = 2L,
  nmis = c(x = 2L, y = 2L),
  method = list(x = "ranger", y = "ranger"),
  boot = FALSE, uncert = "none", call = NULL
)

expect_true(inherits(obj, "vimmi"))
expect_equal(obj$m, 2L)
expect_equal(nrow(obj$data), 5)

# --- complete() single dataset ---

c1 <- complete(obj, action = 1)
expect_true(is.data.frame(c1))
expect_equal(nrow(c1), 5)
expect_equal(sum(is.na(c1)), 0)
# Check imputed values are from imputation 1
expect_equal(c1$x[2], 2.1)
expect_equal(c1$x[4], 4.2)
expect_equal(c1$y[1], 1.0)
expect_equal(c1$y[3], 3.1)

c2 <- complete(obj, action = 2)
expect_equal(c2$x[2], 1.9)
expect_equal(c2$x[4], 3.8)

# --- complete() all datasets ---

all_datasets <- complete(obj, action = "all")
expect_true(is.list(all_datasets))
expect_equal(length(all_datasets), 2)
expect_true(all(sapply(all_datasets, function(d) sum(is.na(d)) == 0)))

# --- complete() long format ---

long <- complete(obj, action = "long")
expect_true(".imp" %in% names(long))
expect_true(".id" %in% names(long))
expect_equal(nrow(long), 5 * 2)  # n_rows * m
expect_true(all(long$.imp %in% 1:2))

# --- complete() validates action ---

expect_error(complete(obj, action = 0))
expect_error(complete(obj, action = 3))
expect_error(complete(obj, action = "invalid"))

# --- with.vimmi ---

fits <- with(obj, lm(y ~ x))
expect_true(is.list(fits))
expect_equal(length(fits), 2)
expect_true(all(sapply(fits, inherits, "lm")))
# Coefficients should differ between imputations
expect_true(!identical(coef(fits[[1]]), coef(fits[[2]])))

# --- print.vimmi ---

out <- capture.output(print(obj))
expect_true(length(out) > 0)
expect_true(any(grepl("vimmi", out, ignore.case = TRUE)))
expect_true(any(grepl("m = 2", out)))

# --- summary.vimmi ---

s <- capture.output(summary(obj))
expect_true(length(s) > 0)
expect_true(any(grepl("m = 2", s)))
