# Check callback inputs as well as the result; the legacy coercion differs
# between weighted and unweighted paths and between nominal and ordinal data.
check_callback <- function(target, k, weighted, expected, expected_weights) {
  seen <- list()
  callback <- function(x, weights = NULL) {
    seen[[length(seen) + 1L]] <<- list(x, weights)
    x[[1L]]
  }
  d <- data.frame(target = target, x = 1:5)
  out <- kNN(d, variable = "target", dist_var = "x", k = k,
             weightDist = weighted, numFun = callback, catFun = callback,
             ordFun = callback, return_donor = TRUE)
  expect_identical(seen, Map(list, expected, expected_weights))
  expect_identical(out$target, target[c(1, 2, 2, 5, 5)])
  expect_identical(out$target_donor_id, if (k == 1L) {
    c(NA_character_, NA_character_, "2", "5", NA_character_)
  } else {
    c(NA_character_, NA_character_, "2;1", "5;2", NA_character_)
  })
}

numeric_target <- c(1L, 2L, NA, NA, 5L)
factor_target <- factor(c("a", "b", NA, NA, "e"), levels = letters[1:5])
ordinal_target <- ordered(factor_target, levels = levels(factor_target))
no_weights <- list(NULL, NULL)
weights <- rep(list(c(.75, .5)), 2)
expect_names <- function(a, b) {
  list(setNames(a, c("3.target1", "3.target2")),
       setNames(b, c("4.target1", "4.target2")))
}
for (weighted in c(FALSE, TRUE)) {
  check_callback(numeric_target, 1L, weighted, list(c(target = 2L), c(target = 5L)),
                 no_weights)
  check_callback(factor_target, 1L, weighted, list(c(target = "b"), c(target = "e")),
                 no_weights)
  check_callback(ordinal_target, 1L, weighted, list("b", "e"), no_weights)
}
check_callback(numeric_target, 2L, FALSE, list(c(2L, 1L), c(5L, 2L)), no_weights)
check_callback(factor_target, 2L, FALSE, list(c("b", "a"), c("e", "b")), no_weights)
check_callback(ordinal_target, 2L, FALSE,
               list(ordinal_target[c(2, 1)], ordinal_target[c(5, 2)]), no_weights)
check_callback(numeric_target, 2L, TRUE, expect_names(c(2L, 1L), c(5L, 2L)), weights)
for (target in list(factor_target, ordinal_target)) {
  check_callback(target, 2L, TRUE,
                 expect_names(factor_target[c(2, 1)], factor_target[c(5, 2)]), weights)
}

# A single donor still goes through the supplied aggregator.
d <- data.frame(target = c(1, NA, 3), x = 1:3)
out <- kNN(d, variable = "target", dist_var = "x", k = 1,
           numFun = function(x) x + 100)
expect_identical(out$target, c(1, 101, 3))
