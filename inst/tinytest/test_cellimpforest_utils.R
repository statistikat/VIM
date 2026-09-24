# detection helpers
w <- VIM:::.cif_bisquare(c(0, 2.535, 4.685, 6, NA))
expect_equal(w[1], 1)
expect_equal(w[2], 0.5, tolerance = 1e-3)          # hard-flag boundary |z| = 2.535
expect_equal(w[3:4], c(0, 0))
expect_true(is.na(w[5]))
expect_equal(VIM:::.cif_surprise(0), 1)
expect_equal(VIM:::.cif_surprise(stats::qnorm(0.995)), 0.01, tolerance = 1e-8)
set.seed(1); r <- stats::rnorm(100); use <- rep(TRUE, 100)
expect_equal(VIM:::.cif_scale(r, use), stats::mad(r))
use2 <- use; use2[1:50] <- FALSE
expect_equal(VIM:::.cif_scale(r, use2), stats::mad(r[51:100]))
expect_equal(VIM:::.cif_scale(c(rep(0, 99), 5), use), stats::sd(c(rep(0, 99), 5)))  # MAD 0 -> SD
expect_equal(VIM:::.cif_scale(rep(0, 100), use), 1)                                   # degenerate -> 1
prob <- rbind(c(0.7, 0.2, 0.1), c(0.1, 0.1, 0.8), c(NA, NA, NA))
colnames(prob) <- c("a", "b", "c")
rho <- VIM:::.cif_rho(prob, factor(c("a", "a", "b"), levels = c("a", "b", "c")))
expect_equal(rho[1:2], c(1, 0.125))
expect_true(is.na(rho[3]))
expect_equal(VIM:::.cif_mode(factor(c("x", "y", "y", NA))), "y")
