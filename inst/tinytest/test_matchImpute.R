library(VIM)
message("matchImpute general")
setna <- function(d,i,col=2){
  d[i,col] <- NA
  d
}
d <- data.frame(x=LETTERS[1:6],y=as.double(1:6),z=as.double(1:6),
                w=ordered(LETTERS[1:6]), stringsAsFactors = FALSE)
dorig <- rbind(d,d)
# minimal example with one match var
d1 <- matchImpute(setna(dorig,7:12,1)[,1:2],match_var = "y", variable="x")
expect_identical(d1$x[d1$x_imp],d1$x[!d1$x_imp])

d1b <- matchImpute(setna(dorig,7:12,1)[,1:2],match_var = "y", variable="x", imp_var = FALSE)
expect_identical(d1b$x[d1$x_imp],d1b$x[!d1$x_imp])
expect_false("x_imp" %in% colnames(d1b))
expect_true("x_imp" %in% colnames(d1))


# all missing in x -> error
expect_error(matchImpute(setna(dorig,1:12,1)[,1:2],match_var = "y", variable="x"))


# example with two match vars
d1 <- matchImpute(setna(dorig,7:12,1)[,1:3],match_var = c("y","z"), variable="x")
expect_identical(d1$x[d1$x_imp],d1$x[!d1$x_imp])

# An existing indicator name can also be a matching key.
with_indicator <- data.table::data.table(x = c(10L, NA, 20L), x_imp = c("a", "a", "b"))
original <- data.table::copy(with_indicator)
expect_error(matchImpute(with_indicator, match_var = "x_imp"),
             "indicator columns already exist.*x_imp")
expect_identical(with_indicator, original)

renamed <- matchImpute(with_indicator, match_var = "x_imp", imp_suffix = "filled")
expect_identical(renamed$x, c(10L, 10L, 20L))
expect_identical(renamed$x_imp, original$x_imp)
expect_identical(renamed$x_filled, c(FALSE, TRUE, FALSE))
expect_identical(with_indicator, original)

without_indicator <- matchImpute(with_indicator, match_var = "x_imp", imp_var = FALSE)
expect_identical(names(without_indicator), names(original))
expect_identical(without_indicator$x_imp, original$x_imp)

# Validate matching columns even when there are no missing target values.
expect_error(matchImpute(d, variable = "x", match_var = "absent"),
             "Unknown columns in 'match_var'.*absent")
expect_error(matchImpute(d, variable = "absent", match_var = "y"),
             "Unknown columns in 'variable'.*absent")
expect_error(matchImpute(d, match_var = c("y", NA_character_)), "'match_var'")
duplicate_columns <- setNames(d, c("x", "y", "y", "w"))
expect_error(matchImpute(duplicate_columns, variable = "x", match_var = "y"),
             "Ambiguous columns in 'match_var'.*y")
expect_error(matchImpute(d, variable = c("x", "x"), match_var = "y"), "'variable'")
expect_error(matchImpute(d, variable = ncol(d) + 1L, match_var = "y"), "column positions")
expect_error(matchImpute(d, match_var = "y", imp_var = NA), "'imp_var'")
expect_error(matchImpute(d, match_var = "y", imp_suffix = c("imp", "filled")), "'imp_suffix'")

# Empty selections retain the input and record zero missing target cells.
matching_only <- d["y"]
no_targets <- matchImpute(matching_only, match_var = "y")
trace <- matrix(c(1, 0), nrow = 1L, dimnames = list(NULL, c("STEP", "NA_PRESENT")))
expect_identical(attr(no_targets, "count_missings"), trace)
attr(no_targets, "count_missings") <- NULL
expect_identical(no_targets, matching_only)

explicit_empty <- matchImpute(d, variable = character(), match_var = "y")
expect_identical(attr(explicit_empty, "count_missings"), trace)
attr(explicit_empty, "count_missings") <- NULL
expect_identical(explicit_empty, d)

empty_rows <- matchImpute(d[FALSE, ], variable = "x", match_var = "y")
expect_identical(empty_rows$x_imp, logical())
expect_identical(attr(empty_rows, "count_missings"), trace)

# Row names stay paired with the original rows across grouped assignments.
named_rows <- data.frame(group = c(2L, 1L, 2L, 1L), value = c(10L, NA, NA, 20L),
                         row.names = c("row d", "row c", "row b", "row a"))
expected <- c(10L, 20L, 10L, 20L)
for (input in list(named_rows, as.matrix(named_rows))) {
  result <- matchImpute(input, match_var = "group")
  expect_identical(rownames(result), rownames(input))
  expect_identical(result$value, expected)
}

# Column positions and literal matching names remain usable.
names(named_rows)[1L] <- "group,key"
result <- matchImpute(named_rows, variable = 2L, match_var = "group,key")
expect_identical(result$value, expected)
expect_identical(result$`2_imp`, is.na(named_rows$value))
