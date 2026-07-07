library(VIM)

## Regression test: considered_variables must NOT silently drop the other
## columns from the output (Wave 1 tail, audit P1.4). vimpute() subset data to
## considered_variables at vimpute.R:194 and returned only that subset, so
## `d <- vimpute(d, considered_variables = ...)` lost columns irreversibly --
## inconsistent with kNN/hotdeck/irmi (which return the full dataset) and mice.
##
## Fix (Matthias's call): default `keep_all_columns = TRUE` returns the full
## dataset (non-considered columns passed through untouched, original order,
## *_imp indicators appended); `keep_all_columns = FALSE` restores the old
## considered-only shape as an opt-in.

set.seed(1)
n <- 50
df <- data.frame(
  a = rnorm(n),
  b = rnorm(n),
  keep_num = 1:n,
  keep_chr = letters[(0:(n - 1)) %% 26 + 1]
)
df$a[c(2, 7, 15)] <- NA
df$b[c(3, 9)] <- NA
## a passthrough column may itself carry NAs -- it must survive UNTOUCHED
df$keep_num[5] <- NA_integer_

## --- m = 1, default keep_all_columns = TRUE: all columns preserved ----------
set.seed(1)
out1 <- vimpute(df, considered_variables = c("a", "b"),
                method = "ranger", sequential = FALSE, imp_var = TRUE, verbose = FALSE)
expect_true(all(c("a", "b", "keep_num", "keep_chr") %in% names(out1)),
            info = "default output dropped non-considered columns")
## considered columns imputed
expect_equal(sum(is.na(as.data.frame(out1)[, c("a", "b")])), 0L)
## passthrough columns untouched (values and their NA left as-is)
expect_equal(out1$keep_chr, df$keep_chr)
expect_true(is.na(out1$keep_num[5]))
expect_equal(out1$keep_num[-5], df$keep_num[-5])
## data columns keep original order; *_imp indicators appended at the end
expect_equal(head(names(out1), 4), c("a", "b", "keep_num", "keep_chr"))
expect_true(all(grepl("_imp$", tail(names(out1), sum(grepl("_imp$", names(out1)))))))

## --- m = 1, keep_all_columns = FALSE: old considered-only shape -------------
set.seed(1)
out1b <- vimpute(df, considered_variables = c("a", "b"),
                 method = "ranger", sequential = FALSE, imp_var = TRUE,
                 keep_all_columns = FALSE, verbose = FALSE)
expect_false(any(c("keep_num", "keep_chr") %in% names(out1b)),
             info = "keep_all_columns = FALSE should drop non-considered columns")
expect_true(all(c("a", "b") %in% names(out1b)))

## --- m > 1: complete() datasets also keep the passthrough columns ----------
set.seed(1)
out2 <- vimpute(df, considered_variables = c("a", "b"),
                method = "ranger", sequential = FALSE, m = 3,
                boot = TRUE, uncert = "normalerror", verbose = FALSE)
c1 <- complete(out2, 1)
expect_true(all(c("a", "b", "keep_num", "keep_chr") %in% names(c1)),
            info = "m>1 complete() dropped non-considered columns")
expect_equal(sum(is.na(c1$a)), 0L)
expect_equal(c1$keep_chr, df$keep_chr)

## opt-out also honoured on the MI path
set.seed(1)
out2b <- vimpute(df, considered_variables = c("a", "b"),
                 method = "ranger", sequential = FALSE, m = 3,
                 boot = TRUE, uncert = "normalerror",
                 keep_all_columns = FALSE, verbose = FALSE)
c1b <- complete(out2b, 1)
expect_false(any(c("keep_num", "keep_chr") %in% names(c1b)),
             info = "keep_all_columns = FALSE (m>1) should drop non-considered columns")

## --- no-op when considered_variables covers all columns --------------------
df2 <- df[, c("a", "b")]
df2$a[1:3] <- NA
set.seed(1)
out3 <- vimpute(df2, considered_variables = c("a", "b"),
                method = "ranger", sequential = FALSE, imp_var = FALSE, verbose = FALSE)
expect_equal(sort(names(out3)), c("a", "b"))
