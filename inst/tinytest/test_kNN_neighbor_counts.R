# Regression for #112: reject invalid counts before allocation/selection.
x <- data.frame(x = c(0, 1))
y <- data.frame(x = .2)
for (n in list(0, -1, 1.5, NA_real_, Inf, numeric(), c(1, 2), "1")) {
  expect_error(gowerD(x, y, returnIndex = TRUE, nMin = n))
  expect_error(VIM:::whichminN(c(0, 1), n, 1L))
  expect_error(VIM:::gowerDind(as.matrix(x), as.matrix(y), 1,
                             c(1, 0, 0, 0), numeric(), numeric(), n, 0L))
  expect_error(kNN(data.frame(target = c(1, 2, NA), x = 1:3), k = n))
}
expect_warning(out <- kNN(data.frame(target = c(1, 2, NA), x = 1:3),
                          variable = "target", k = 2, donorcond = list(">1"),
                          return_donor = TRUE),
               "exceeds the number of eligible donors")
expect_identical(out$target_donor_id[3], "2;2")
expect_error(kNN(data.frame(target = c(1, 2, NA), x = 1:3),
                 variable = "target", k = 1, donorcond = list(">2")),
             "eligible donors")
expect_error(VIM:::whichminN(numeric(), 1L, 0L), "eligible donors")
expect_equal(as.vector(gowerD(x, y, returnIndex = TRUE, nMin = 2)$ind), c(1, 2))
expect_equal(VIM:::whichminN(c(1, 1), 2L, 0L)$which, c(1, 2))

# Once all distinct donors are used, the legacy selector repeats the first
# eligible row, even if it was not the nearest.
expect_warning(out <- gowerD(x, data.frame(x = .8), returnIndex = TRUE,
                            nMin = 4, returnMin = TRUE), "reusing the first")
expect_equal(as.vector(out$ind), c(2, 1, 1, 1))
expect_equal(as.vector(out$mins), c(.2, .8, Inf, Inf))
expect_warning(out <- VIM:::whichminN(c(.8, .2), 4L, 1L), "reusing the first")
expect_equal(out$which, c(2, 1, 1, 1))
expect_equal(out$mins, c(.2, .8, Inf, Inf))

# Padding must not produce NaN weights when all genuine distances are >= 1.
expect_warning(out <- kNN(data.frame(target = c(2, 4, NA), x = c(0, 1, 10)),
                          variable = "target", k = 4, methodStand = "iqr",
                          weightDist = TRUE,
                          numFun = function(x, weights) weighted.mean(x, weights)),
               "reusing the first")
expect_true(is.finite(out$target[3]))
