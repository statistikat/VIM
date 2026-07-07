library(VIM)

## Wave 2: vimpute() gains a documented `seed` argument (audit P1.31 / P2.63).
## Reproducibility previously required an ambient set.seed() before the call;
## there was no seed in the signature. Contract (mice-compatible): seed = NULL
## (default) leaves the RNG stream alone; a numeric seed makes the whole call
## reproducible. For m > 1 the seed is applied ONCE at entry -- the m runs must
## still differ from each other (a per-run re-seed would make all imputations
## identical and invalidate Rubin's rules).

data(sleep, package = "VIM")
d <- sleep[, c("Sleep", "Dream", "Span", "BodyWgt")]

## --- same seed => identical results (ranger is stochastic without it) -------
out1 <- vimpute(d, method = "ranger", sequential = FALSE, seed = 42,
                imp_var = FALSE, verbose = FALSE)
out2 <- vimpute(d, method = "ranger", sequential = FALSE, seed = 42,
                imp_var = FALSE, verbose = FALSE)
expect_identical(out1, out2)

## --- different seeds => different results ------------------------------------
out3 <- vimpute(d, method = "ranger", sequential = FALSE, seed = 43,
                imp_var = FALSE, verbose = FALSE)
expect_false(isTRUE(all.equal(out1, out3)))

## --- m > 1: reproducible as a whole, but the m runs differ from each other ---
mi1 <- vimpute(d, method = "ranger", sequential = FALSE, m = 3, boot = TRUE,
               uncert = "normalerror", seed = 7, imp_var = FALSE, verbose = FALSE)
mi2 <- vimpute(d, method = "ranger", sequential = FALSE, m = 3, boot = TRUE,
               uncert = "normalerror", seed = 7, imp_var = FALSE, verbose = FALSE)
expect_identical(mi1$imp, mi2$imp)
## the seed must NOT be re-applied inside the m-loop: imputations differ
imp_sleep <- mi1$imp[["Sleep"]]
expect_false(identical(imp_sleep[[1]], imp_sleep[[2]]),
             info = "per-run re-seeding collapsed the m imputations to identical values")

## --- validation: seed must be a single finite number -------------------------
expect_error(vimpute(d, method = "ranger", sequential = FALSE, seed = "abc"))
expect_error(vimpute(d, method = "ranger", sequential = FALSE, seed = c(1, 2)))
