# ==========================================================================
# Tests for cellGLoc: cellwise-robust estimation on a categorical mean structure
# ==========================================================================

set.seed(1)
d <- data.frame(x1 = rnorm(60), x2 = rnorm(60),
                f1 = factor(rep(c("a", "b", "c"), each = 20)),
                f2 = factor(rep(c("u", "v"), 30)))

U <- VIM:::.gloc_design(d, ~ ., c("f1", "f2"))
expect_equal(nrow(U), 60)
expect_equal(ncol(U), 4)                      # intercept + 2 (f1) + 1 (f2)
expect_true(all(U[, 1] == 1))

U1 <- VIM:::.gloc_design(d, ~ 1, c("f1", "f2"))
expect_equal(ncol(U1), 1)                     # intercept only

# a missing factor value becomes its own level, never a dropped row
d2 <- d; d2$f1[1:5] <- NA
U2 <- VIM:::.gloc_design(d2, ~ ., c("f1", "f2"))
expect_equal(nrow(U2), 60)
expect_equal(ncol(U2), 5)                     # intercept + 3 (f1 incl. NA) + 1 (f2)
