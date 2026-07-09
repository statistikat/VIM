library(VIM)

## Wave 2 (audit P2.62): unify the per-variable specification.
## Layer 1 -- spec objects: vimpute(data, spec = list(y = vs_ranger(num.trees
## = 150, tune = TRUE), g = vs_robust(), .default = vs_ranger())). The vs_*()
## constructors validate learner parameters eagerly against the method's
## param set (fail at call time, not at iteration 7).
## Layer 2 -- formula grammar as sugar compiling to the same specs:
## vimpute(data, y ~ x1 + x2 | ranger(tune = TRUE), g ~ . | robust(),
## .default = vs_ranger()). A plain-column RHS lowers to `predictors` (works
## for every method incl. ranger/xgboost); an RHS with transformations lowers
## to `formula` (formula-capable methods only).
## The nine flat per-variable arguments keep working unchanged; spec/grammar
## compile to exactly them, so results are identical for identical seeds.

## --- constructors --------------------------------------------------------------
s <- vs_ranger(num.trees = 150, tune = TRUE, pmm = TRUE, pmm_k = 3)
expect_true(inherits(s, "vimpute_spec"))
expect_equal(s$method, "ranger")
expect_equal(s$params$num.trees, 150)
expect_true(s$tune)
expect_true(s$pmm)
expect_equal(s$pmm_k, 3)

## eager validation: unknown learner parameters fail at call time, named
err <- tryCatch(vs_ranger(num.treez = 300), error = function(e) conditionMessage(e))
expect_true(grepl("num.treez", err, fixed = TRUE))

## unknown methods are rejected (registry check)
expect_error(vimpute_spec("no_such_method"), pattern = "no_such_method")

## formula capability is checked eagerly against the registry
expect_error(vs_ranger(formula = ~ x1), pattern = "formula")
## a spec takes either formula or predictors, not both
expect_error(vs_gam(formula = ~ x1, predictors = "x2"))
## uncert is a call-level argument, not a spec knob
expect_error(vs_ranger(uncert = "pmm"), pattern = "uncert")

## print method exists and names the method
expect_true(any(grepl("ranger", capture.output(print(s)))))

## --- data ----------------------------------------------------------------------
set.seed(21)
n <- 40
dd <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
dd$y <- 1.5 * dd$x1 + rnorm(n, sd = 0.3)
dd$g <- factor(ifelse(dd$x2 > 0, "a", "b"))
dd$y[sample(n, 7)] <- NA
dd$g[sample(n, 6)] <- NA

## --- spec = : end-to-end, exactly equivalent to the flat arguments --------------
res_spec <- vimpute(dd, spec = list(y = vs_ranger(num.trees = 150),
                                    g = vs_ranger()),
                    sequential = FALSE, uncert = "none", imp_var = FALSE,
                    verbose = FALSE, seed = 7)
expect_equal(sum(is.na(res_spec)), 0L)

res_flat <- vimpute(dd, method = list(y = "ranger", g = "ranger"),
                    learner_params = list(y = list(num.trees = 150)),
                    sequential = FALSE, uncert = "none", imp_var = FALSE,
                    verbose = FALSE, seed = 7)
expect_equal(res_spec, res_flat,
             info = "spec must compile to exactly the flat-argument behaviour")

## .default supplies unspecified variables, silently
res_def <- vimpute(dd, spec = list(y = vs_ranger(num.trees = 150),
                                   .default = vs_ranger()),
                   sequential = FALSE, uncert = "none", imp_var = FALSE,
                   verbose = FALSE, seed = 7)
expect_equal(res_def, res_spec)

## validation of the spec list itself
expect_error(vimpute(dd, spec = list(y = vs_ranger()), method = "ranger"),
             pattern = "spec")
expect_error(vimpute(dd, spec = list(zzz = vs_ranger())), pattern = "zzz")
expect_error(vimpute(dd, spec = list(y = "ranger")), pattern = "vimpute_spec")

## --- formula grammar: sugar over the same specs ---------------------------------
res_gram <- vimpute(dd, y ~ . | ranger(num.trees = 150), g ~ . | ranger(),
                    sequential = FALSE, uncert = "none", imp_var = FALSE,
                    verbose = FALSE, seed = 7)
expect_equal(res_gram, res_spec,
             info = "grammar must compile to exactly the spec behaviour")

## plain-column RHS lowers to `predictors` -- identical to a predictors spec
res_g2 <- vimpute(dd, y ~ x1 + x2 | ranger(num.trees = 150),
                  .default = vs_ranger(),
                  sequential = FALSE, uncert = "none", imp_var = FALSE,
                  verbose = FALSE, seed = 7)
res_s2 <- vimpute(dd, spec = list(y = vs_ranger(num.trees = 150,
                                                predictors = c("x1", "x2")),
                                  .default = vs_ranger()),
                  sequential = FALSE, uncert = "none", imp_var = FALSE,
                  verbose = FALSE, seed = 7)
expect_equal(res_g2, res_s2)

## an RHS with transformations lowers to `formula` (formula-capable method)
res_f <- suppressWarnings(
  vimpute(dd, y ~ x1 + I(x2^2) | robust(), .default = vs_ranger(),
          sequential = FALSE, uncert = "none", imp_var = FALSE,
          verbose = FALSE, seed = 7))
expect_equal(sum(is.na(res_f)), 0L)

## ...and is rejected eagerly for methods without formula support
expect_error(
  vimpute(dd, y ~ I(x2^2) | ranger(), sequential = FALSE, verbose = FALSE),
  pattern = "formula")

## donorcond rides along in the method call
res_dc <- suppressWarnings(
  vimpute(dd, y ~ . | robust(donorcond = ">= 0"), .default = vs_ranger(),
          sequential = FALSE, uncert = "none", imp_var = FALSE,
          verbose = FALSE, seed = 7))
expect_equal(sum(is.na(res_dc)), 0L)

## --- grammar error handling ------------------------------------------------------
expect_error(vimpute(dd, "oops", method = "ranger"), pattern = "formula")
expect_error(
  vimpute(dd, y ~ . | no_such_method(), sequential = FALSE, verbose = FALSE),
  pattern = "no_such_method")
expect_error(
  vimpute(dd, y ~ . | ranger(), spec = list(g = vs_ranger())),
  pattern = "spec")
