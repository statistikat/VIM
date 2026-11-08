# Regression for #114: constants follow selected mixed variables, not positions.
d <- data.frame(a = c(NA, 100, 200, 300), b = c(10, 30, 11, 12))
for (stand in c("range", "iqr")) {
  for (predictors in list("b", list("b"))) {
    out <- kNN(d, variable = "a", dist_var = predictors, mixed = c("a", "b"),
               mixed.constant = c(0, 10), k = 1, return_donor = TRUE,
               methodStand = stand)
    expected <- kNN(d, variable = "a", dist_var = predictors, mixed = "b",
                    mixed.constant = 10, k = 1, return_donor = TRUE,
                    methodStand = stand)
    expect_equal(out, expected)
    expect_equal(out$a[1], 100)
    expect_equal(out$a_donor_id[1], "2")
  }
}
expect_equal(kNN(d, variable = "a", mixed = c("a", "b"), k = 1),
             kNN(d, variable = "a", mixed = c("a", "b"),
                 mixed.constant = c(0, 0), k = 1))
# Added RF predictors inherit the mixed target's point mass.
d <- data.frame(a = c(rep(10, 6), 11:16, NA), b = c(rep(0, 6), 1:7))
set.seed(114)
out <- kNN(d, variable = "a", dist_var = "b", mixed = c("b", "a"),
           mixed.constant = c(0, 10), addRF = TRUE, onlyRF = TRUE, k = 1)
expect_false(anyNA(out$a))
