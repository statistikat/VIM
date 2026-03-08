library(VIM)
message("gowerD test")
# Targeted branch coverage for src/gowerD.cpp

weights <- c(2, 3, 5, 7)
ncol_var <- c(1, 1, 1, 1)
lev_orders <- 5
mixed_constants <- 0

# This layout exercises all four variable types and all mixed-variable branches:
# constant/constant, constant/non-constant, and non-constant/non-constant.
x <- matrix(
  c(
    0.1, 1, 2, 0.0,
    0.7, 2, 5, 0.8
  ),
  nrow = 2,
  byrow = TRUE
)

y <- matrix(
  c(
    0.4, 2, 5, 0.3,
    0.1, 1, 2, 0.0,
    0.9, 2, 1, 0.8
  ),
  nrow = 3,
  byrow = TRUE
)

expected_delta <- matrix(
  c(
    14.35 / 17, 0.00 / 17, 12.85 / 17,
    4.10 / 17, 14.95 / 17, 5.40 / 17
  ),
  nrow = 2,
  byrow = TRUE
)

expected_inds <- matrix(
  c(
    2, 1, 2,
    1, 2, 1
  ),
  nrow = 2,
  byrow = TRUE
)

expected_mins <- matrix(
  c(
    4.10 / 17, 0.00 / 17, 5.40 / 17,
    14.35 / 17, 14.95 / 17, 12.85 / 17
  ),
  nrow = 2,
  byrow = TRUE
)

delta <- VIM:::gowerd(
  x,
  y,
  weights,
  ncol_var,
  lev_orders,
  mixed_constants
)
expect_equal(unname(delta$delta), expected_delta)

ind_only <- VIM:::gowerDind(
  x,
  y,
  weights,
  ncol_var,
  lev_orders,
  mixed_constants,
  2L,
  0L
)
expect_equal(unname(ind_only$ind), expected_inds)

ind_and_min <- VIM:::gowerDind(
  x,
  y,
  weights,
  ncol_var,
  lev_orders,
  mixed_constants,
  2L,
  1L
)
expect_equal(unname(ind_and_min$ind), expected_inds)
expect_equal(unname(ind_and_min$min), expected_mins)

mins_with_ties <- VIM:::whichminN(c(4, 1, 2, 1), 3L, 1L)
expect_equal(unname(mins_with_ties$which), c(2, 4, 3))
expect_equal(unname(mins_with_ties$mins), c(1, 1, 2))

inds_with_ties <- VIM:::whichminN(c(4, 1, 2, 1), 2L, 0L)
expect_equal(unname(inds_with_ties$which), c(2, 4))
