# uncert = "pmm" performs true predictive mean matching (Little 1988):
# donors are matched on their PREDICTED values, not their observed values.
# Factor targets receive a class-probability draw on every sweep whenever
# uncert != "none", so early stopping cannot strip them of
# between-imputation variability.

if (!requireNamespace("mlr3", quietly = TRUE) ||
    !requireNamespace("robustbase", quietly = TRUE)) {
  exit_file("mlr3 stack / robustbase not available")
}

suppressMessages(suppressWarnings(library(VIM)))

## --- decoy test: donors chosen by predicted value, not observed value -----
## Clean linear signal y = 2x plus three gross outliers ("decoys") whose
## observed y sits exactly where the missing row's prediction lands, but
## whose own predictions are far away. True PMM must never draw a decoy;
## value-matching draws one with probability 3/5 per run.
n <- 60
dat <- data.frame(x = c(seq_len(n), 2, 3, 4))
dat$y <- 2 * dat$x
dat$y[61:63] <- c(119, 121, 123)   # decoys: x tiny, observed y near 120
dat$y[60] <- NA                    # missing at x = 60; robust fit -> yhat = 120

decoys <- c(119, 121, 123)
imputed <- vapply(1:12, function(s) {
  res <- suppressWarnings(
    vimpute(dat, method = "robust", seed = s,
            sequential = FALSE, imp_var = FALSE, verbose = FALSE)
  )
  as.numeric(res$y[60])
}, numeric(1))

expect_true(all(imputed %in% na.omit(dat$y)),
            info = "pmm imputations are observed donor values")
expect_false(any(imputed %in% decoys),
             info = "donors matched on predictions, not observed values")

## --- factor targets: stochastic draw survives early stopping --------------
## Two incomplete variables keep the sequential loop alive, so it stops
## early (two quiet sweeps) well before nseq -- the spot where the old code
## fell back to deterministic argmax imputations. g depends strongly on the
## complete z (argmax stable across refits); the amputed w is unrelated
## noise, so the only across-imputation variability in g can come from the
## class-probability draw.
set.seed(7)
nf <- 150
z <- rnorm(nf)
w <- rnorm(nf)
g <- factor(ifelse(runif(nf) < plogis(3 * z), "a", "b"))
dat_f <- data.frame(z = z, w = w, g = g)
idx_g <- which(abs(z) > 0.3)[1:25]   # away from the decision boundary
dat_f$g[idx_g] <- NA
dat_f$w[1:12] <- NA

mi <- suppressWarnings(
  vimpute(dat_f, method = "robust", m = 4, boot = FALSE, seed = 3,
          eps = 1e6, verbose = FALSE)   # huge eps forces the early stop
)
imps <- sapply(1:4, function(i) as.character(complete(mi, i)$g[idx_g]))
expect_false(all(imps[, 1] == imps[, 2]) && all(imps[, 2] == imps[, 3]) &&
               all(imps[, 3] == imps[, 4]),
             info = "factor imputations differ across imputations (draws, not argmax)")

## --- uncert = "none" keeps factor imputation deterministic ----------------
## sequential = FALSE was the old code's unconditional-draw path; under
## uncert = "none" it must now be deterministic.
mi_none <- suppressWarnings(
  vimpute(dat_f, method = "robust", m = 2, boot = FALSE, uncert = "none",
          sequential = FALSE, seed = 3, verbose = FALSE)
)
i1 <- as.character(complete(mi_none, 1)$g[1:25])
i2 <- as.character(complete(mi_none, 2)$g[1:25])
expect_true(all(i1 == i2),
            info = "uncert = 'none' factor imputations are deterministic")
