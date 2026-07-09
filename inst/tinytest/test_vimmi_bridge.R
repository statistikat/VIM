library(VIM)

## Wave 2 (audit P2.65): harden the vimmi <-> mice bridge.
## - vim_as_mids() is the documented conversion (as.mids.vimmi is kept as the
##   historical alias -- there is no as.mids S3 generic, so the ".vimmi" suffix
##   was misleading dressing).
## - with() on a vimmi returns a mice-compatible "mira" object (call, call1,
##   nmis, analyses), so pool()/summary()/getfit() workflows run unchanged --
##   previously it returned an anonymous list.
## - vimmi stores per-iteration chain statistics (mean/var of the imputed
##   values per variable x iteration x imputation) plus the seed, and
##   plot(vimmi) draws mice-style convergence trace plots.

if (!requireNamespace("mice", quietly = TRUE)) {
  exit_file("mice not available")
}

set.seed(3)
n <- 45
dd <- data.frame(x = rnorm(n), z = rnorm(n))
dd$y <- 1 + 2 * dd$x + rnorm(n, sd = 0.4)
dd$y[sample(n, 8)] <- NA

mi <- suppressWarnings(vimpute(dd, method = "robust", m = 3, uncert = "normalerror",
                               sequential = TRUE, nseq = 3, seed = 11, verbose = FALSE))
expect_true(inherits(mi, "vimmi"))

## --- vim_as_mids(): primary name; as.mids.vimmi stays as alias ------------------
mids1 <- vim_as_mids(mi)
expect_true(inherits(mids1, "mids"))
mids2 <- as.mids.vimmi(mi)
expect_true(inherits(mids2, "mids"))
expect_equal(mice::complete(mids1, 2), mice::complete(mids2, 2))
expect_error(vim_as_mids(dd), pattern = "vimmi")

## --- with() returns a mira: the full mice pipeline runs unchanged ---------------
fits <- with(mi, lm(y ~ x + z))
expect_true(inherits(fits, "mira"))
expect_equal(length(mice::getfit(fits)), 3L)
expect_true(all(vapply(mice::getfit(fits), inherits, logical(1), "lm")))
pooled <- mice::pool(fits)
expect_true(inherits(pooled, "mipo"))
sm <- summary(pooled)
expect_true("x" %in% as.character(sm$term))

## --- chain diagnostics: mean/var arrays [nvar, niter, m] + seed -----------------
expect_true(is.list(mi$chain),
            info = "vimmi must store per-iteration chain statistics")
expect_true(is.array(mi$chain$mean) && length(dim(mi$chain$mean)) == 3L)
expect_equal(dim(mi$chain$mean)[3], 3L)
expect_true("y" %in% dimnames(mi$chain$mean)[[1]])
expect_equal(dim(mi$chain$mean), dim(mi$chain$var))
expect_true(any(is.finite(mi$chain$mean["y", , ])))
expect_equal(mi$seed, 11)

## the m chains must actually differ (stochastic uncertainty between runs)
chain_y <- mi$chain$mean["y", , ]
expect_false(identical(chain_y[, 1], chain_y[, 2]))

## --- plot.vimmi draws base-graphics trace plots ---------------------------------
pdf(NULL)
expect_silent(plot(mi))
dev.off()

## a vimmi without chains (hand-built, pre-7.3.0 shape) fails informatively
old_style <- structure(list(data = dd, imp = mi$imp, where = mi$where, m = 3L,
                            nmis = mi$nmis, method = mi$method, boot = FALSE,
                            uncert = "normalerror", call = NULL),
                       class = "vimmi")
expect_error(plot(old_style), pattern = "chain")

## --- Wave 3: density and strip panels (mice densityplot/stripplot analogues) ----
## Both work from the stored imputations (x$imp + x$data), so they also work
## for objects without chain statistics.
pdf(NULL)
expect_silent(plot(mi, "density"))
expect_silent(plot(mi, "strip"))
expect_silent(plot(old_style, "density"))
expect_silent(plot(old_style, "strip"))
dev.off()
expect_error(plot(mi, "nope"))
