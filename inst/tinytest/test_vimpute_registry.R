library(VIM)

## Wave 2 (audit P2.64): register_vimpute_method() plugin registry.
## Adding an imputation method used to take ~12 coordinated edits across
## vimpute.R and helper_vimpute.R (supported_methods, learner construction,
## three copy-pasted modifyList dispatch chains, formula allow-lists, ...).
## New contract: methods live in a package-env registry; the built-ins are
## seeded through it and third parties extend VIM with ONE call, no patching:
##   register_vimpute_method(name, learner = list(regr = , classif = ), ...)

if (!requireNamespace("rpart", quietly = TRUE)) {
  exit_file("rpart not available")
}

builtin <- c("ranger", "xgboost", "regularized", "robust", "gam", "robgam")

## --- built-ins are registered through the same contract ------------------------
expect_true(is.character(vimpute_methods()))
expect_true(setequal(vimpute_methods(), builtin),
            info = "the six built-in methods must be seeded in the registry")

## --- one-call registration of a new method (mlr3's rpart learners) -------------
register_vimpute_method("cart",
  learner  = list(regr = "regr.rpart", classif = "classif.rpart"),
  packages = "rpart")
expect_true("cart" %in% vimpute_methods())

## double registration is refused unless overwrite = TRUE
expect_error(
  register_vimpute_method("cart", learner = list(regr = "regr.rpart")),
  pattern = "already registered")
register_vimpute_method("cart",
  learner  = list(regr = "regr.rpart", classif = "classif.rpart"),
  packages = "rpart", overwrite = TRUE)

## malformed registrations fail fast
expect_error(register_vimpute_method("bad1", learner = list()),
             info = "a learner for at least one task type is required")
expect_error(register_vimpute_method("bad2", learner = "regr.rpart"),
             info = "learner must be a named list(regr = , classif = )")

## --- end-to-end: vimpute() runs the registered method --------------------------
set.seed(42)
n <- 40
dd <- data.frame(
  y  = rnorm(n, mean = 10),
  g  = factor(sample(c("a", "b"), n, replace = TRUE)),
  x1 = rnorm(n),
  x2 = runif(n)
)
dd$y[sample(n, 6)] <- NA
dd$g[sample(n, 5)] <- NA

res <- vimpute(dd, method = "cart", sequential = FALSE, uncert = "none",
               imp_var = FALSE, verbose = FALSE, seed = 7)
expect_equal(sum(is.na(res)), 0L)
expect_true(is.numeric(res$y))
expect_true(is.factor(res$g))

## PMM works on top of a registered method (donor draw from its predictions)
res_pmm <- vimpute(dd, method = "cart", sequential = FALSE, uncert = "pmm",
                   imp_var = FALSE, verbose = FALSE, seed = 7)
expect_equal(sum(is.na(res_pmm)), 0L)
expect_true(all(res_pmm$y[is.na(dd$y)] %in% dd$y[!is.na(dd$y)]),
            info = "pmm must draw imputations from observed donor values")

## per-variable mixing of registered and built-in methods
res_mix <- vimpute(dd, method = list(y = "cart", g = "robust"),
                   sequential = FALSE, uncert = "none",
                   imp_var = FALSE, verbose = FALSE, seed = 7)
expect_equal(sum(is.na(res_mix)), 0L)

## --- unsupported-method errors name the offender and the registry --------------
err <- tryCatch(vimpute(dd, method = "no_such_method", sequential = FALSE),
                error = function(e) conditionMessage(e))
expect_true(grepl("no_such_method", err, fixed = TRUE),
            info = "error must name the unsupported method")
expect_true(grepl("cart", err, fixed = TRUE),
            info = "error must list the registered methods")

err2 <- tryCatch(vimpute(dd, method = list(y = "nope", g = "cart"),
                         sequential = FALSE),
                 error = function(e) conditionMessage(e))
expect_true(grepl("nope", err2, fixed = TRUE),
            info = "named-list error must name the offending method")

## --- formula capability comes from the registry ---------------------------------
## cart is registered without supports_formula, so a formula must be refused
expect_error(
  vimpute(dd, method = "cart", formula = list(y ~ x1), sequential = FALSE,
          uncert = "none", verbose = FALSE),
  pattern = "formula")

## --- validate hook: precheck falls back with the hook's reason ------------------
register_vimpute_method("alwaysbad",
  learner  = list(regr = "regr.rpart", classif = "classif.rpart"),
  packages = "rpart",
  fallback = "robust",
  validate = function(y_obs, data, variable) {
    sprintf("Method 'alwaysbad' rejects '%s'. Falling back to 'robust'.",
            variable)
  })
expect_warning(
  res_fb <- vimpute(dd[, c("y", "x1", "x2")], method = "alwaysbad",
                    sequential = FALSE, uncert = "none", imp_var = FALSE,
                    verbose = FALSE, seed = 7),
  pattern = "alwaysbad")
expect_equal(sum(is.na(res_fb)), 0L)

## --- regression-only methods degrade gracefully on factor targets ---------------
register_vimpute_method("regonly",
  learner  = list(regr = "regr.rpart"),
  packages = "rpart")
expect_warning(
  res_ro <- vimpute(dd, method = "regonly", sequential = FALSE, uncert = "none",
                    imp_var = FALSE, verbose = FALSE, seed = 7),
  pattern = "regonly")
expect_equal(sum(is.na(res_ro)), 0L,
             info = "factor target must be imputed via the fallback method")

## --- search_space hook feeds the tuner ------------------------------------------
register_vimpute_method("cart",
  learner  = list(regr = "regr.rpart", classif = "classif.rpart"),
  packages = "rpart", overwrite = TRUE,
  search_space = function(learner_id, task) {
    list(space = paradox::ps(cp = paradox::p_dbl(0.001, 0.1)), n_evals = 4L)
  })
task <- mlr3::TaskRegr$new("t", backend = data.frame(y = rnorm(20), x = rnorm(20)),
                           target = "y")
ss <- VIM:::build_vimpute_search_space("regr.rpart", task, method = "cart")
expect_true(inherits(ss$space, "ParamSet"),
            info = "registry search_space must be honoured")
expect_equal(ss$n_evals, 4L)

## built-in search spaces are untouched by the hook
ss_ranger <- VIM:::build_vimpute_search_space("regr.ranger", task, method = "ranger")
expect_true(inherits(ss_ranger$space, "ParamSet"))

## --- unregister: user methods removable, built-ins protected --------------------
expect_true(unregister_vimpute_method("alwaysbad"))
expect_true(unregister_vimpute_method("regonly"))
expect_true(unregister_vimpute_method("cart"))
expect_error(unregister_vimpute_method("ranger"), pattern = "built-in")
expect_error(unregister_vimpute_method("never_registered"))
expect_true(setequal(vimpute_methods(), builtin))
