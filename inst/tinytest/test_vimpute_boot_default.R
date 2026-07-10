# vimpute(): adaptive boot default -- bootstrap refits (boot = TRUE) when
# m > 1 (approximately proper MI draws), single fit (boot = FALSE) when m = 1;
# an explicit boot = TRUE/FALSE always wins.

if (!requireNamespace("mlr3", quietly = TRUE) ||
    !requireNamespace("mlr3learners", quietly = TRUE)) {
  exit_file("mlr3 stack not available")
}

suppressMessages(suppressWarnings(library(VIM)))

set.seed(11)
n <- 60
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d$y <- 2 * d$x1 - d$x2 + rnorm(n, sd = 0.3)
d$y[sample(n, 12)] <- NA

# --- m > 1: default must equal explicit boot = TRUE (seed-identical) --------
mi_default <- vimpute(d, method = "robust", m = 2, seed = 42,
                      sequential = FALSE, verbose = FALSE)
mi_true    <- vimpute(d, method = "robust", m = 2, seed = 42, boot = TRUE,
                      sequential = FALSE, verbose = FALSE)
expect_equal(complete(mi_default, 1), complete(mi_true, 1),
             info = "m > 1 default matches boot = TRUE (run 1)")
expect_equal(complete(mi_default, 2), complete(mi_true, 2),
             info = "m > 1 default matches boot = TRUE (run 2)")

# --- m > 1: default must differ from explicit boot = FALSE ------------------
mi_false <- vimpute(d, method = "robust", m = 2, seed = 42, boot = FALSE,
                    sequential = FALSE, verbose = FALSE)
expect_false(isTRUE(all.equal(complete(mi_default, 1), complete(mi_false, 1))),
             info = "m > 1 default is not the old boot = FALSE behaviour")

# --- explicit boot = FALSE at m > 1 is respected (old behaviour reachable) --
mi_false2 <- vimpute(d, method = "robust", m = 2, seed = 42, boot = FALSE,
                     sequential = FALSE, verbose = FALSE)
expect_equal(complete(mi_false, 1), complete(mi_false2, 1),
             info = "explicit boot = FALSE is reproducible")

# --- m = 1: default must equal explicit boot = FALSE (seed-identical) -------
si_default <- vimpute(d, method = "robust", seed = 7,
                      sequential = FALSE, verbose = FALSE)
si_false   <- vimpute(d, method = "robust", seed = 7, boot = FALSE,
                      sequential = FALSE, verbose = FALSE)
expect_equal(as.data.frame(si_default), as.data.frame(si_false),
             info = "m = 1 default stays boot = FALSE")

# --- m = 1: boot = TRUE changes the fit (resolution is m-dependent) ---------
si_true <- vimpute(d, method = "robust", seed = 7, boot = TRUE,
                   sequential = FALSE, verbose = FALSE)
expect_false(isTRUE(all.equal(as.data.frame(si_default), as.data.frame(si_true))),
             info = "m = 1 default differs from boot = TRUE")

# --- pure defaults at m > 1 must not trigger the properness warnings --------
w_caught <- tryCatch({
  vimpute(d, method = "robust", m = 2, seed = 1,
          sequential = FALSE, verbose = FALSE)
  NULL
}, warning = function(w) w)
expect_null(w_caught, info = "no properness warning with m > 1 defaults")
