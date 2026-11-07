# Regression for #112: reject invalid counts before allocation/selection.
x <- data.frame(x = c(0, 1))
y <- data.frame(x = .2)
for (n in list(0, -1, 1.5, NA_real_, Inf, numeric(), c(1, 2), "1", 4)) {
  expect_error(gowerD(x, y, returnIndex = TRUE, nMin = n))
  expect_error(VIM:::whichminN(c(0, 1), n, 1L))
  expect_error(VIM:::gowerDind(as.matrix(x), as.matrix(y), 1,
                             c(1, 0, 0, 0), numeric(), numeric(), n, 0L))
  expect_error(kNN(data.frame(target = c(1, 2, NA), x = 1:3), k = n))
}
expect_error(kNN(data.frame(target = c(1, 2, NA), x = 1:3),
                 variable = "target", k = 2, donorcond = list(">1")),
             "target.*eligible donors")
expect_error(kNN(data.frame(target = c(1, 2, NA), x = 1:3),
                 variable = "target", k = 1, donorcond = list(">2")),
             "eligible donors")
expect_error(VIM:::whichminN(numeric(), 1L, 0L), "eligible donors")
expect_equal(as.vector(gowerD(x, y, returnIndex = TRUE, nMin = 2)$ind), c(1, 2))
expect_equal(VIM:::whichminN(c(1, 1), 2L, 0L)$which, c(1, 2))
