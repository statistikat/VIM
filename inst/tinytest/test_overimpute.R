library(VIM)

## Wave 3 (audit P1.28): overimpute() -- imputation-model calibration
## diagnostic (Amelia::overimpute analogue). Observed cells of one variable
## are treated as missing fold by fold and imputed with `draws` multiple
## imputations; observed value vs imputed mean +/- interval answers "is my
## imputation model well calibrated?" for ANY vimpute method.

set.seed(31)
n <- 40
dd <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
dd$y <- 1 + 2 * dd$x1 + rnorm(n, sd = 0.5)
dd$y[sample(n, 6)] <- NA          # genuine missings stay untouched
n_obs <- sum(!is.na(dd$y))

ov <- suppressWarnings(
  overimpute(dd, "y", method = "robust", draws = 3, folds = 3,
             sequential = FALSE, seed = 5))

expect_true(inherits(ov, "vimpute_overimpute"))
expect_equal(nrow(ov), n_obs,
             info = "one row per observed cell of the target")
expect_true(all(c("row", "observed", "mean", "lower", "upper", "covered")
                %in% names(ov)))
expect_true(all(ov$lower <= ov$upper))
expect_true(all(ov$row %in% which(!is.na(dd$y))))
expect_true(is.logical(ov$covered))
expect_equal(attr(ov, "variable"), "y")
expect_equal(attr(ov, "level"), 0.9)

## the genuinely missing cells are not overimputed
expect_false(any(ov$row %in% which(is.na(dd$y))))

## coverage summary in print
out <- capture.output(print(ov))
expect_true(any(grepl("coverage", out, ignore.case = TRUE)))

## plot draws without error
pdf(NULL)
expect_silent(plot(ov))
dev.off()

## reproducible with a seed
ov2 <- suppressWarnings(
  overimpute(dd, "y", method = "robust", draws = 3, folds = 3,
             sequential = FALSE, seed = 5))
expect_equal(ov$mean, ov2$mean)

## --- validation ------------------------------------------------------------------
expect_error(overimpute(dd, "g"), pattern = "g")            # unknown column
dd$g <- factor(sample(c("a", "b"), n, replace = TRUE))
expect_error(overimpute(dd, "g"), pattern = "numeric")      # factor target
expect_error(overimpute(dd, "y", draws = 1), pattern = "draws")
expect_error(overimpute(dd, "y", m = 5), pattern = "draws") # m is controlled
