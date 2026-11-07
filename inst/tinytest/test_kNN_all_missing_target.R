# Regression for #113: skipped targets must not shift positional options.
d <- data.frame(a = NA_real_, b = c(1, NA, 3), x = 1:3)
expect_warning(out <- kNN(d, k = 1), "All observations of a")
expect_true(all(is.na(out$a)))
expect_equal(out$b, c(1, 1, 3))
for (targets in list(c("a", "b"), c("b", "a"))) {
  b <- match("b", targets)
  predictors <- rep(list("a"), 2)
  predictors[[b]] <- "x"
  conditions <- rep(list(NULL), 2)
  conditions[[b]] <- ">2"
  expect_warning(out <- kNN(d, variable = targets, k = 1,
    dist_var = predictors, weights = list(1, 1), donorcond = conditions,
    return_donor = TRUE), "All observations of a")
  expect_equal(out$b, c(1, 3, 3))
  expect_equal(out$b_donor_id, c(NA_character_, "3", NA_character_))
  expect_true(all(is.na(out$a_donor_id)))
  expect_true(all(is.na(out$a)))
}
expect_warning(out <- kNN(d, variable = "a", return_donor = TRUE))
expect_equal(out$b, d$b)
expect_true(all(is.na(out$a_donor_id)))
# RF paths must also skip the original target position without shifting lists.
d <- data.frame(a = NA_real_, b = c(1:8, rep(NA_real_, 4)), x = 1:12)
set.seed(113)
expect_warning(out <- kNN(d, variable = c("a", "b"), dist_var = "x",
                          addRF = TRUE, k = 1), "All observations of a")
expect_false(anyNA(out$b))
expect_true(all(is.na(out$a)))
expect_warning(out <- kNN(d, variable = c("a", "b"), dist_var = "x",
                          weights = "auto", k = 1), "All observations of a")
expect_false(anyNA(out$b))
