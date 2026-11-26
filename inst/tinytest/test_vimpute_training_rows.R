## vimpute() suggests the mlr3 stack; this file cannot run without it.
if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                  "paradox", "R6", "future", "ranger"),
                requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed (VIM only suggests it)")
## Wraps an internal function for the duration of a call; kept off CRAN.
if (!at_home()) exit_file("runs at home only")

library(VIM)

## Which rows may a variable's model be trained on? The rows whose target was
## OBSERVED in the input data -- all of them, and only them, in every sweep.
## Two defects used to break this:
##
## (1) First sweep: rows with a still-missing predictor were removed by
##     na.omit(), so the model saw the complete cases only (80 instead of 160
##     rows below) and degenerated to a constant when there were none.
## (2) Later sweeps: `data` then holds the previous sweep's imputations, so
##     `!is.na(target)` selected every row and each model was refitted on its
##     own imputed values (200 instead of 160 rows below).
##
## The training-set size is read off the task handed to the internal trainer.

training_rows <- function(...) {
  seen <- list()
  ns <- asNamespace("VIM")
  orig <- get("train_with_fallback", envir = ns)
  wrapper <- function(learner, task, var, ...) {
    seen[[length(seen) + 1L]] <<- data.frame(var = var, n = task$nrow)
    orig(learner, task, var, ...)
  }
  unlockBinding("train_with_fallback", ns)
  assign("train_with_fallback", wrapper, envir = ns)
  on.exit({
    assign("train_with_fallback", orig, envir = ns)
    lockBinding("train_with_fallback", ns)
  })
  out <- suppressWarnings(vimpute(..., imp_var = FALSE, verbose = FALSE))
  list(out = out, rows = do.call(rbind, seen))
}

set.seed(1)
n <- 200
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
d$y <- d$x1 - d$x2 + rnorm(n)
d$y[1:40] <- NA
d$x1[41:80] <- NA
d$x2[81:120] <- NA
n_obs <- colSums(!is.na(d))            # 160 for x1, x2, y

## --- (1) + (2): ranger, three sweeps ----------------------------------------
for (u in c("none", "pmm")) {
  tr <- training_rows(d, method = "ranger", nseq = 3, eps = 0, m = 1,
                      uncert = u, seed = 1)
  expect_equal(nrow(tr$rows), 9L, info = paste("3 sweeps x 3 variables,", u))
  expect_true(all(tr$rows$n == n_obs[tr$rows$var]),
              info = paste("training rows != observed-target rows, uncert =", u))
  expect_equal(sum(is.na(tr$out)), 0L)
}

## --- same rule for a learner fitted by a formula-free linear model ----------
tr <- training_rows(d, method = "robust", nseq = 2, eps = 0, m = 1,
                    uncert = "none", seed = 1)
expect_true(all(tr$rows$n == n_obs[tr$rows$var]),
            info = "robust: training rows != observed-target rows")

## --- bootstrap + PMM (the m > 1 default) ------------------------------------
tr <- training_rows(d, method = "ranger", nseq = 2, eps = 0, m = 2, seed = 1)
expect_true(all(tr$rows$n == n_obs[tr$rows$var]),
            info = "m > 1: training rows != observed-target rows")

## --- (1) no complete case at all: a model is still fitted -------------------
## Every row with an observed y lacks x1 or x2. The old code found no training
## row, warned "No observed values for target ..." and filled in one constant.
set.seed(2)
e <- data.frame(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
e$y <- 2 * e$x1 - 2 * e$x2 + rnorm(120, sd = 0.3)
truth <- e$y[1:20]
e$y[1:20] <- NA
e$x1[21:70] <- NA
e$x2[71:120] <- NA
msgs <- character(0)
res <- withCallingHandlers(
  vimpute(e, method = "ranger", sequential = FALSE, imp_var = FALSE,
          verbose = FALSE, uncert = "none", seed = 1),
  warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
expect_false(any(grepl("No observed values for target", msgs)),
             info = "first sweep still falls back to a constant")
expect_true(length(unique(res$y[1:20])) > 1L,
            info = "imputed y is a constant")
expect_true(cor(res$y[1:20], truth) > 0.5,
            info = "imputed y does not follow its predictors")

## --- factor target ----------------------------------------------------------
f <- d
f$g <- factor(ifelse(f$x3 + rnorm(n, sd = 0.5) > 0, "a", "b"))
f$g[121:150] <- NA
tr <- training_rows(f, method = "ranger", nseq = 2, eps = 0, m = 1, seed = 1)
n_obs_f <- colSums(!is.na(f))
expect_true(all(tr$rows$n == n_obs_f[tr$rows$var]),
            info = "factor target: training rows != observed-target rows")

## --- donorcond still narrows the training rows ------------------------------
tr <- training_rows(d, method = "ranger", nseq = 2, eps = 0, m = 1,
                    uncert = "none", seed = 1,
                    donorcond = list(y = ">= 0"))
y_rows <- tr$rows$n[tr$rows$var == "y"]
expect_true(all(y_rows == sum(d$y >= 0, na.rm = TRUE)),
            info = "donorcond: training rows of y != observed donors")

## --- (3) formula (model-matrix) path ----------------------------------------
## The design matrix of a formula model used to be built from the rows that
## are complete in ALL columns, so its first sweep saw 80 rows instead of 160.
tr <- training_rows(d, method = "robust", formula = list(x1 ~ x2 + x3 + y),
                    nseq = 3, eps = 0, m = 1, uncert = "none", seed = 1)
expect_equal(nrow(tr$rows), 9L, info = "formula path: 3 sweeps x 3 variables")
expect_true(all(tr$rows$n == n_obs[tr$rows$var]),
            info = "formula path: training rows != observed-target rows")
expect_equal(sum(is.na(tr$out)), 0L)

## donorcond narrows the training rows of a formula model in every sweep
tr <- training_rows(d, method = "robust", formula = list(x1 ~ x2 + x3 + y),
                    nseq = 3, eps = 0, m = 1, uncert = "none", seed = 1,
                    donorcond = list(x1 = ">= 0"))
x1_rows <- tr$rows$n[tr$rows$var == "x1"]
expect_true(all(x1_rows == sum(d$x1 >= 0, na.rm = TRUE)),
            info = "formula path + donorcond: training rows of x1 != observed donors")

## --- (3b) PMM donors of a formula model are scored on their own rows --------
## The design matrix doubles as the row reference of the donor scoring. While
## it held the complete cases only, the observed rows were looked up at the
## wrong positions and the donors were matched on scores of other rows. With
## y = 3 * x1 (almost exactly) a correctly matched donor is close to the truth.
set.seed(3)
g <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
g$y <- 3 * g$x1 + rnorm(n, sd = 0.01)
truth_g <- g$y[1:40]
g$y[1:40] <- NA
g$x2[41:120] <- NA
res <- suppressWarnings(
  vimpute(g, method = "robust", formula = list(y ~ x1), sequential = FALSE,
          m = 1, uncert = "pmm", imp_var = FALSE, verbose = FALSE, seed = 1))
expect_equal(sum(is.na(res)), 0L)
expect_true(cor(res$y[1:40], truth_g) > 0.95,
            info = "formula path: PMM donors not matched on their own scores")
