# Regression for #115: validate public inputs and every native distance path.
x <- data.frame(a = c(0, 1), b = c(1, 0))
for (w in list(c(0, 0), c(-1, 2), c(NA, 1), c(Inf, 1), c(NaN, 1),
               c(1e308, 1e308), 1, c(1, 1, 1), c("1", "1"))) {
  expect_error(gowerD(x, weights = w), "weights")
}
expect_equal(gowerD(x, weights = c(0, 1)), matrix(c(0, 1, 1, 0), 2))
expect_equal(gowerD(x, x[, c("b", "a")]), gowerD(x))
expect_equal(gowerD(x, numerical = 1, weights = c(2, 10)),
             gowerD(x["a"], weights = 2))
expect_error(gowerD(x, numerical = 1, weights = c(0, 1)), "Weights")
expect_error(gowerD(x, x["a"]), "matching unique column names")
expect_error(gowerD(x, numerical = "unknown"), "existing columns")
expect_error(gowerD(x, numerical = 3), "existing columns")
expect_error(gowerD(x, numerical = "a", factors = "a"), "exactly one type")
expect_error(gowerD(x, numerical = character()), "nonempty")
for (constant in list(numeric(), c(0, 1), NA_real_, Inf, "0"))
  expect_error(gowerD(x, numerical = "a", mixed = "b", mixed.constant = constant),
               "mixed.constant")

native <- function(index = FALSE, dataX = as.matrix(x), dataY = as.matrix(x),
                   weights = c(1, 1), counts = c(2, 0, 0, 0),
                   levels = numeric(), constants = numeric()) {
  if (index)
    VIM:::gowerDind(dataX, dataY, weights, counts, levels, constants, 1L, 1L)
  else
    VIM:::gowerd(dataX, dataY, weights, counts, levels, constants)
}
for (index in c(FALSE, TRUE)) {
  for (w in list(c(0, 0), c(-1, 2), c(NA, 1), c(Inf, 1), c(1e308, 1e308),
                 numeric(), 1, c(1, 1, 1)))
    expect_error(native(index, weights = w), "Weights")
  expect_error(native(index, dataY = matrix(0, 1, 1)), "same positive number")
  expect_error(native(index, dataX = matrix(numeric(), 2, 0),
                       dataY = matrix(numeric(), 2, 0)), "same positive number")
  for (counts in list(numeric(), c(2, 0), c(2, 0, 0, 0, 0), c(1, 0, 0, 0),
                      c(-1, 3, 0, 0), c(.5, 1.5, 0, 0), c(NA, 0, 0, 0),
                      c(Inf, 0, 0, 0)))
    expect_error(native(index, counts = counts), "Type counts")
  expect_error(native(index, counts = c(1, 0, 1, 0)), "Ordinal level counts")
  for (levels in list(0, 1.5, NA_real_, Inf))
    expect_error(native(index, counts = c(1, 0, 1, 0), levels = levels),
                 "Ordinal level counts")
  expect_error(native(index, counts = c(1, 0, 0, 1)), "Mixed constants")
  expect_error(native(index, counts = c(1, 0, 0, 1), constants = Inf),
               "Mixed constants")
  expect_error(native(index, levels = 3), "Ordinal level counts")
  expect_error(native(index, constants = 0), "Mixed constants")
  bad <- as.matrix(x); bad[1, 1] <- NA_real_
  expect_error(native(index, dataX = bad), "finite values")
  bad[1, 1] <- Inf
  expect_error(native(index, dataY = bad), "finite values")
}
# Public missing values remain supported through the established sentinel.
expect_true(all(is.finite(gowerD(data.frame(a = c(NA_real_, 1))))))
for (value in c(NA_real_, Inf, -Inf, NaN))
  expect_error(VIM:::whichminN(c(0, value), 1L, 0L), "finite values")
