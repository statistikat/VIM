# classif.glm_rob must attach the binomial probability to the correct
# factor level. glmrob(family = binomial()) models P(second factor level);
# before the fix the predict step assigned that probability to the FIRST
# level, inverting every binary classification -- worse-than-chance
# imputation even on strong signal (caught by the paper benchmark on the
# diabetes data: PFC 0.70 where flipping the predictions gives 0.30).
suppressMessages(library(VIM))

if (!requireNamespace("mlr3", quietly = TRUE) ||
    !requireNamespace("robustbase", quietly = TRUE)) {
  exit_file("mlr3 stack not available")
}

## binary: strong logistic signal, deterministic argmax imputation --------
set.seed(1)
n <- 400
x <- rnorm(n)
y <- factor(ifelse(runif(n) < plogis(3 * x), "yes", "no"),
            levels = c("no", "yes"))
d <- data.frame(x = x, y = y)

idx <- which(abs(x) > 1)[1:50]  # rows where P(correct class) >= plogis(3)
d_amp <- d
d_amp$y[idx] <- NA

imp <- suppressWarnings(
  vimpute(d_amp, method = "robust", imp_var = FALSE,
          uncert = "none", verbose = FALSE, seed = 1))
pfc_bin <- mean(as.character(imp$y[idx]) != as.character(d$y[idx]))
# correctly oriented robust logistic: PFC ~0.05 here; inverted: ~0.95
expect_true(pfc_bin < 0.25,
            info = sprintf("binary glm_rob PFC = %.2f", pfc_bin))

## multiclass one-vs-rest was oriented correctly -- pin it ----------------
set.seed(2)
z <- rnorm(n)
eta <- cbind(0, 2 * z, -2 * z)
pg <- exp(eta) / rowSums(exp(eta))
g <- factor(apply(pg, 1, function(p) sample(c("a", "b", "c"), 1, prob = p)),
            levels = c("a", "b", "c"))
dg <- data.frame(z = z, g = g)

idx_g <- which(abs(z) > 1)[1:50]
dg_amp <- dg
dg_amp$g[idx_g] <- NA

imp_g <- suppressWarnings(
  vimpute(dg_amp, method = "robust", imp_var = FALSE,
          uncert = "none", verbose = FALSE, seed = 1))
pfc_multi <- mean(as.character(imp_g$g[idx_g]) != as.character(dg$g[idx_g]))
expect_true(pfc_multi < 0.35,
            info = sprintf("multiclass glm_rob PFC = %.2f", pfc_multi))
