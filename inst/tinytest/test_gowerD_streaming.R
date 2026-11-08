# Compare the streaming search with the full matrix on all four distance types.
donors <- data.frame(
  x = c(7, 2, 2, NA, 5, 2, 7),
  group = c("b", "a", "a", NA, "c", "a", "b"),
  rank = ordered(c("high", "low", "low", NA, "mid", "low", "high"),
                 levels = c("low", "mid", "high")),
  mixed = c(21, 10, 10, NA, 12, 10, 21)
)
recipients <- donors[c(2, 4, 5, 1), ]
recipients$group[3] <- "d"
recipients$x[4] <- 4
for (scaling in c("range", "iqr")) {
  for (rows in list(1L, seq_len(nrow(recipients)))) {
    args <- list(data.x = donors, data.y = recipients[rows, ],
                 weights = c(1, 3, 2, 4), numerical = "x", factors = "group",
                 orders = "rank", levOrders = 3, mixed = "mixed",
                 mixed.constant = 10, methodStand = scaling)
    full <- do.call(gowerD, args)
    sorted <- vapply(seq_len(ncol(full)), function(j) order(full[, j]),
                     integer(nrow(donors)))
    for (k in c(1L, 2L, 5L, nrow(donors))) {
      expected <- sorted[seq_len(k), , drop = FALSE]
      for (threads in c(1L, 2L)) {
        old <- options(VIM.ncores = threads)
        with_min <- do.call(gowerD, c(args, list(returnIndex = TRUE,
                                                returnMin = TRUE, nMin = k)))
        indices <- do.call(gowerD, c(args, list(returnIndex = TRUE, nMin = k)))
        options(old)
        expect_identical(with_min$ind, matrix(as.double(expected), k))
        expect_identical(indices$ind, with_min$ind)
        expect_null(indices$mins)
        expected_min <- full[cbind(as.vector(expected), rep(seq_len(ncol(full)), each = k))]
        expect_identical(with_min$mins, matrix(expected_min, k))
      }
    }
  }
}

# Finite input can still overflow during distance arithmetic. The index path
# must reject it instead of treating the overflow as an ordinary candidate.
expect_error(VIM:::gowerDind(matrix(c(-1e308, 0), 2), matrix(1e308, 1),
                            1, c(1, 0, 0, 0), numeric(), numeric(), 2L, 0L),
             "Distances must contain finite values")
