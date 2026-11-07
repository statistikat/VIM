# Regression for #111: categorical codes must be shared across populations.
nominal_distance <- function(x, y) {
  gowerD(data.frame(z = x), data.frame(z = y),
         numerical = character(), factors = "z")
}
expect_equal(as.vector(nominal_distance(c("a", "b"), "b")), c(1, 0))
expect_equal(as.vector(nominal_distance(
  factor(c("a", "b"), levels = c("a", "b", "unused")),
  factor("b", levels = c("b", "a")))), c(1, 0))
expect_equal(as.vector(nominal_distance(factor(c("a", "b")), "b")), c(1, 0))
expect_equal(as.vector(nominal_distance(c("a", "b", NA), c("b", NA))),
             c(1, 0, 1, 1, 1, 0))
expect_equal(as.vector(nominal_distance(c("a", "b"), "c")), c(1, 1))
d <- data.frame(z = c("a", "b", "b"), target = c(10, 20, NA))
expect_equal(kNN(d, variable = "target", dist_var = "z", k = 1)$target,
             c(10, 20, 20))
