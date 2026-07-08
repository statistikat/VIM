library(VIM)

## Wave 2 (audit P2.67): scale-normalized convergence criterion.
## The old criterion summed raw per-variable MSE changes, so eps = 0.005 was
## meaningless across data scales (a variable measured in thousands could
## never converge; standardized data converged instantly) and one variable
## could mask another. New contract: per variable,
##   numeric:  d_v = mean((new - old)^2) / var(observed_v)
##   factor:   d_v = disagreement rate of the imputed categories
## converged when max_v d_v < eps for two consecutive iterations, and the
## iterations x variables matrix of d_v is returned as attr(., "convergence").

set.seed(1)
n <- 60
d0 <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d0$y <- 2 * d0$x1 - d0$x2 + rnorm(n, sd = 0.1)
d0$g <- factor(ifelse(d0$x1 + rnorm(n, sd = 0.3) > 0, "a", "b"))
d0$y[sample(n, 10)] <- NA
d0$g[sample(n, 8)] <- NA

## --- sequential runs expose the per-variable convergence matrix ---------------
res <- suppressWarnings(vimpute(d0, method = "robust", sequential = TRUE, nseq = 5,
               uncert = "none", imp_var = FALSE, verbose = FALSE, seed = 42))
cm <- attr(res, "convergence")
expect_true(is.matrix(cm), info = "sequential result must carry a convergence matrix")
expect_true(setequal(colnames(cm), c("y", "g")),
            info = "one column per NA-variable")
expect_true(nrow(cm) >= 1L)
expect_true(all(cm >= 0, na.rm = TRUE), info = "d_v are squared/rate quantities")

## factor variables: disagreement rate must live in [0, 1]
expect_true(all(cm[, "g"] >= 0 & cm[, "g"] <= 1, na.rm = TRUE))

## --- scale invariance: y in thousands must not change the decisions -----------
d1 <- d0
d1$y <- d0$y * 1000
res1 <- suppressWarnings(vimpute(d1, method = "robust", sequential = TRUE, nseq = 5,
                uncert = "none", imp_var = FALSE, verbose = FALSE, seed = 42))
cm1 <- attr(res1, "convergence")
expect_equal(nrow(cm1), nrow(cm),
             info = "iteration count must not depend on the scale of a variable")
expect_equal(cm1[, "y"], cm[, "y"], tolerance = 1e-6,
             info = "relative changes must be scale-free")

## --- generous eps stops after two consecutive quiet iterations ----------------
res2 <- suppressWarnings(vimpute(d0, method = "robust", sequential = TRUE, nseq = 8, eps = 1e6,
                uncert = "none", imp_var = FALSE, verbose = FALSE, seed = 42))
cm2 <- attr(res2, "convergence")
expect_equal(nrow(cm2), 2L,
             info = "d_v is measured from iteration 2 on; two quiet iterations stop the run")

## --- non-sequential runs have no convergence history ---------------------------
res3 <- suppressWarnings(vimpute(d0, method = "robust", sequential = FALSE,
                uncert = "none", imp_var = FALSE, verbose = FALSE, seed = 42))
expect_true(is.null(attr(res3, "convergence")))
