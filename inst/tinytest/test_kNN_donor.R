library(VIM)

# Unequal distances make donor selection deterministic, with different missing
# rows and donor pools for each imputed variable.
# Only the complete 'distance' column is used to select neighbours, so imputing
# x cannot change which donors are selected for y.
d <- data.frame(id = paste0("record-", 11:16), distance = c(0, 1, 4, 10, 12, 30),
                x = c(10, NA, 30, 40, NA, 60),
                y = c(100, 200, NA, 400, 500, NA))
# Custom row names verify that id_var = NULL uses row positions, not row names.
rownames(d) <- paste0("row-", 6:1)
# Exercise all eight combinations: row numbers or explicit IDs, one or two
# imputed variables, and one donor or two semicolon-separated donors.
for (id_var in list(NULL, "id")) {
  ids <- if (is.null(id_var)) as.character(seq_len(nrow(d))) else d$id
  for (variables in list("x", c("x", "y"))) {
    for (k in c(1, 2)) {
      result <- kNN(d, variable = variables, dist_var = "distance", k = k,
                    id_var = id_var, return_donor = TRUE)
      # x is missing in rows 2 and 5. Their nearest available donor rows are
      # (1, 3) and (4, 3), respectively, in increasing distance order.
      # Observed rows retain NA donor IDs because they were not imputed.
      expected_x <- rep(NA_character_, nrow(d))
      expected_x[c(2, 5)] <- if (k == 1) ids[c(1, 4)] else
        c(paste(ids[c(1, 3)], collapse = ";"), paste(ids[c(4, 3)], collapse = ";"))
      expect_identical(result$x_donor_id, expected_x)
      if ("y" %in% variables) {
        # y has a different donor pool: rows 3 and 6 are missing, with nearest
        # donor rows (2, 1) and (5, 4). This checks IDs are tracked per variable.
        expected_y <- rep(NA_character_, nrow(d))
        expected_y[c(3, 6)] <- if (k == 1) ids[c(2, 5)] else
          c(paste(ids[c(2, 1)], collapse = ";"), paste(ids[c(5, 4)], collapse = ";"))
        expect_identical(result$y_donor_id, expected_y)
      } else {
        # A variable outside 'variable' must not get a donor ID column.
        expect_false("y_donor_id" %in% names(result))
      }
      # Reporting donors must leave imputed values and imputation flags intact.
      # Supplying id_var alone must not add columns when return_donor is FALSE.
      baseline <- kNN(d, variable = variables, dist_var = "distance", k = k)
      expect_identical(result[names(baseline)], baseline)
      expect_identical(kNN(d, variable = variables, dist_var = "distance", k = k,
                          id_var = id_var, return_donor = FALSE), baseline)
    }
  }
}

# One recipient, numeric IDs, and data.table input without imputation flags.
# Restricting to four rows leaves just one missing x value. Its donors have
# numeric IDs 101 and 309, which must become the character value "101;309".
single <- d[c(1, 2, 3, 4), ]
single$id <- c(101, 205, 309, 412)
dt <- data.table::as.data.table(single)
original <- data.table::copy(dt)
result <- kNN(dt, variable = "x", dist_var = "distance", k = 2,
              id_var = "id", return_donor = TRUE, imp_var = FALSE)
expect_identical(result$x_donor_id, c(NA_character_, "101;309", NA_character_, NA_character_))
# Preserve the data.table return type and avoid modifying the input by reference.
# Donor reporting must also work independently of the imp_var flag.
expect_true(data.table::is.data.table(result))
expect_identical(dt, original)
expect_false("x_imp" %in% names(result))

# Donor restrictions must be reflected in the returned IDs.
# Excluding x <= 20 removes row 1, so row 2 now uses row 3 as its donor;
# row 5 still uses row 4. Returned IDs must follow the restricted donor pool.
result <- kNN(d, variable = "x", dist_var = "distance", k = 1,
              donorcond = list(">20"), return_donor = TRUE)
expect_identical(result$x_donor_id[c(2, 5)], c("3", "4"))

# Apart from NULL, id_var must be a single non-missing character column name.
# Reject wrong types, missing/empty/multiple names, and nonexistent columns,
# even when donor reporting is disabled (the default in these calls).
for (bad_id in list(1, TRUE, NA_character_, character(), c("id", "x"), "absent")) {
  expect_error(kNN(d, id_var = bad_id), pattern = "id_var")
}
# return_donor accepts only one non-missing logical value: no numeric/string
# coercion, NULL, empty vectors, or vectors containing multiple flags.
for (bad_return in list(1, "TRUE", NA, NULL, logical(), c(TRUE, FALSE))) {
  expect_error(kNN(d, return_donor = bad_return), pattern = "return_donor")
}
