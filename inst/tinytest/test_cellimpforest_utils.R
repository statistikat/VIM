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
# rho' = cross-fitted probability of the observed level / the level's base rate in the training rows
prob <- rbind(c(0.7, 0.2, 0.1), c(0.1, 0.1, 0.8), c(NA, NA, NA), c(0.02, 0.9, 0.08))
colnames(prob) <- c("a", "b", "c")
base <- c(a = 0.5, b = 0.4, c = 0.1)
lv <- c("a", "b", "c", "d")
rho <- VIM:::.cif_rho(prob, factor(c("a", "a", "b", "c"), levels = lv), base)
expect_equal(rho[c(1, 2, 4)], c(1.4, 0.2, 0.8))
expect_true(is.na(rho[3]))                            # no cross-fitted prediction: no score
expect_equal(VIM:::.cif_rho(rbind(base), factor("c", levels = lv), base), 1)   # no signal: 1
# a level with base rate 0 in the training rows (absent, or explicitly 0) scores 0
expect_equal(VIM:::.cif_rho(prob[1:2, ], factor(c("d", "a"), levels = lv), base), c(0, 0.2))
expect_equal(VIM:::.cif_rho(prob[1, , drop = FALSE], factor("c", levels = lv),
                            c(a = 0.6, b = 0.4, c = 0)), 0)
expect_equal(VIM:::.cif_mode(factor(c("x", "y", "y", NA))), "y")
