library(VIM)

## Regression test: irmi(mi > 1) with the default imp_var = TRUE must return a
## LIST of `mi` completed data.frames, not a single flattened data.frame
## (Wave 1 tail, audit P1.12). When mi > 1, x is a list of imputed data.frames,
## but the imp_var block ran x <- cbind(x, imp_vardf); cbind.data.frame
## flattened the list into one wide 62x25 frame with every column name
## duplicated, so documented multiple imputation via irmi was unusable with
## default arguments.

data(sleep, package = "VIM")

## --- mi = 2, default imp_var = TRUE: a list of two completed datasets --------
set.seed(2)
r <- suppressWarnings(irmi(sleep, mi = 2))
expect_true(is.list(r) && !is.data.frame(r),
            info = "irmi(mi = 2) should return a list, not a data.frame")
expect_equal(length(r), 2L)
for (d in r) {
  expect_true(is.data.frame(d))
  expect_false(any(duplicated(names(d))),
               info = "duplicated column names from a cbind-flattened list")
  expect_true(all(c("Sleep", "Dream", "Span") %in% names(d)))
  expect_true(any(grepl("_imp$", names(d))),
              info = "imputation-indicator columns missing from a list element")
  expect_equal(sum(is.na(d$Sleep)), 0L)
  expect_equal(nrow(d), nrow(sleep))
}

## --- mi = 2, imp_var = FALSE: already returned a list; keep that contract ----
set.seed(2)
r2 <- suppressWarnings(irmi(sleep, mi = 2, imp_var = FALSE))
expect_true(is.list(r2) && !is.data.frame(r2))
expect_equal(length(r2), 2L)
expect_false(any(grepl("_imp$", names(r2[[1]]))))
expect_false(any(duplicated(names(r2[[1]]))))

## --- mi = 1 still returns a single data.frame with indicators ---------------
set.seed(2)
r1 <- suppressWarnings(irmi(sleep, mi = 1))
expect_true(is.data.frame(r1))
expect_true(any(grepl("_imp$", names(r1))))
expect_false(any(duplicated(names(r1))))
