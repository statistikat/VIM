library(VIM)

# Keep a shared three-level scale even when each subset observes fewer levels.
# A-to-B and C-to-B must both be half the full A-to-C distance.
ordinal_distance <- function(x, y = x, levels = 3, ...) {
  gowerD(data.frame(w = x), data.frame(w = y), numerical = character(),
         orders = "w", levOrders = levels, ...)
}
x <- ordered(c("A", "C"), levels = LETTERS[1:3])
y <- ordered("B", levels = LETTERS[1:3])
warnings <- character()
distance <- withCallingHandlers(ordinal_distance(x, y), warning = function(w) {
  warnings <<- c(warnings, conditionMessage(w))
  invokeRestart("muffleWarning")
})
expect_identical(warnings, character())
expect_equal(as.vector(distance), c(0.5, 0.5))

# Unused levels still determine scaling; NA does not add a declared level.
# The pooled maximum is rank 3, so the missing-value sentinel is rank 4.
x_na <- ordered(c("A", NA), levels = LETTERS[1:3])
y_na <- ordered(c("C", NA), levels = LETTERS[1:3])
expect_equal(unname(ordinal_distance(x_na, y_na)),
             matrix(c(1, 0.5, 1.5, 0), nrow = 2))
# C and D occur in neither input, but still belong to the four-level scale.
expect_equal(as.vector(ordinal_distance(
  ordered("A", levels = LETTERS[1:4]),
  ordered("B", levels = LETTERS[1:4]), levels = 4)), 1 / 3)

# Numeric ordinal inputs represent ranks on the full scale, even if some ranks
# never occur. Missing ranks are accepted and use the same sentinel convention.
expect_equal(as.vector(ordinal_distance(c(1, 3), 2)), c(0.5, 0.5))
expect_equal(unname(ordinal_distance(c(1, NA), c(3, NA))),
             matrix(c(1, 0.5, 1.5, 0), nrow = 2))
for (bad_rank in list(0, 4, 1.5, Inf, -Inf)) {
  expect_error(ordinal_distance(c(1, bad_rank), 2), pattern = "integer ranks")
}
for (bad_count in list(0, -1, 2.5, NA_real_, Inf, c(3, 3), numeric(), "3")) {
  expect_error(ordinal_distance(x, y, levels = bad_count), pattern = "levOrders")
}

# Equal numbers of levels are insufficient: labels and their ordering must
# match, or identical integer codes can incorrectly imply zero distance.
expect_error(ordinal_distance(x, ordered("B", levels = c("B", "C", "D"))),
             pattern = "identical ordered factor levels")
expect_error(ordinal_distance(x, ordered("B", levels = rev(LETTERS[1:3]))),
             pattern = "identical ordered factor levels")
expect_error(ordinal_distance(x, y, levels = 4), pattern = "declared levels")
expect_error(ordinal_distance(x, 2), pattern = "identical ordered factor levels")
expect_error(ordinal_distance(factor(c("A", "C")), factor("B")),
             pattern = "identical ordered factor levels")
expect_error(ordinal_distance(c("A", "C"), "B"), pattern = "numeric ranks")

# A genuine single-level scale must yield finite zero distances between
# observed values, while observed-to-missing distance remains one.
one <- ordered(c("A", "A"), levels = "A")
expect_equal(unname(ordinal_distance(one, levels = 1)), matrix(0, 2, 2))
one_na <- ordered(c("A", NA), levels = "A")
expect_equal(unname(ordinal_distance(one_na, levels = 1)), matrix(c(0, 1, 1, 0), 2))
expect_equal(unname(ordinal_distance(c(1, 1), levels = 1)), matrix(0, 2, 2))
nearest <- ordinal_distance(one_na, one[1], levels = 1,
                             returnIndex = TRUE, returnMin = TRUE, nMin = 1L)
expect_equal(as.vector(nearest$ind), 1)
expect_equal(as.vector(nearest$mins), 0)

# Verify kNN can use a constant ordinal predictor alongside a numeric one.
# Previously the ordinal 0/0 contribution made the total distance NaN.
d <- data.frame(x = c(10, NA, 30), position = c(0, 1, 4),
                w = ordered(rep("A", 3)))
imputed <- kNN(d, variable = "x", dist_var = c("position", "w"), k = 1,
               return_donor = TRUE)
expect_equal(imputed$x, c(10, 10, 30))
expect_identical(imputed$x_donor_id, c(NA_character_, "1", NA_character_))
