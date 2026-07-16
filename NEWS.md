# VIM 7.3.0

## Breaking changes
- **`with()` on a `vimmi` object returns a mice-compatible `mira`** (elements `call`, `call1`, `nmis`, `analyses`) instead of an anonymous list, so `mice::pool()`, `summary(pool(fits))` and `mice::getfit()` run unchanged. Code that indexed the old list directly (`fits[[i]]`, `lapply(fits, ...)`) should use `mice::getfit(fits)` or `fits$analyses`.
- **`vimpute()` returns are type-stable**: the result is always the imputed data, classed like the input (data.frame in, data.frame out; data.table in, data.table out). With `tune = TRUE` or `pred_history = TRUE` the diagnostics are attached as `attr(result, "tuning_log")` / `attr(result, "pred_history")` instead of switching the return to a bare list.
- **`vimpute()`'s default `uncert` is now `"pmm"`** (random draw among the 5 nearest donors): default numeric imputations are observed donor values with an honest distribution, instead of deterministic conditional means. Set `uncert = "none"` for the previous behaviour. Default `m > 1` runs are now stochastic between imputations. `rangerImpute()`, `xgboostImpute()` and `regressionImp()` keep their deterministic behaviour (`uncert = "none"` pinned internally).
- **`vimpute(m > 1)` now bootstraps by default**: `boot` defaults to `TRUE` for multiple imputation -- each of the `m` imputations refits its models on a bootstrap sample, so parameter uncertainty is propagated into the draws (approximately proper multiple imputation in combination with the default PMM) -- and to `FALSE` for single imputation. Set `boot = FALSE` explicitly for the previous single-fit behaviour.
- **`uncert = "pmm"` and `"midastouch"` now perform true predictive mean matching** (Little 1988): donors are matched on their *predicted* values -- scored by the trained model on the observed rows, out-of-bag for a no-bootstrap ranger fit -- instead of on their observed values. Matching on observed values selected donors whose values happened to lie near the prediction (including gross outliers from other covariate regions) and its donor spread shrank with n; predicted-value matching carries residual-scale variability, as in mice and `Hmisc::aregImpute()`.
- **Factor targets now receive a class-probability draw whenever `uncert != "none"`**: the imputed category is sampled from the predicted class probabilities on every sweep (the convergence criterion tracks the noise-free most-probable class), so early stopping no longer strips factor variables of between-imputation variability; under `uncert = "none"` factor imputation is now fully deterministic (previously `sequential = FALSE` drew stochastically regardless of `uncert`).

## New features
- **Method registry**: `vimpute()`'s imputation methods are resolved through a package-level registry, and `register_vimpute_method()` adds a user-defined method backed by any mlr3 learner pair in one call -- e.g. `register_vimpute_method("cart", learner = list(regr = "regr.rpart", classif = "classif.rpart"), packages = "rpart")`, or lightgbm via `mlr3extralearners` -- usable everywhere the built-in names work (global `method`, per-variable lists, method-keyed `learner_params`, `tune = TRUE` via an optional `search_space` hook). `vimpute_methods()` lists the registered methods, `unregister_vimpute_method()` removes user-added ones. The six built-ins are seeded through the same contract; unsupported-method errors now name the offending method and list what is registered.
- **Per-variable specs**: `vimpute(data, spec = list(Sleep = vs_ranger(num.trees = 300, tune = TRUE), NonD = vs_robust(donorcond = ">= 0"), .default = vs_ranger()))` bundles a variable's method, learner parameters, `formula`/`predictors`, `tune`, PMM settings, `makeNA` and `donorcond` into one object instead of nine parallel arguments. The `vs_*()` constructors (and `vimpute_spec()` for registered methods) validate learner parameters **eagerly** against the method's parameter set, so a typo fails at the constructor call, not mid-imputation. The flat per-variable arguments keep working unchanged; specs compile to exactly them.
- **Formula grammar**: per-variable settings can be written as bare formulas, `vimpute(data, Sleep ~ Dream + Span | ranger(tune = TRUE), NonD ~ . | robust(donorcond = ">= 0"), .default = vs_ranger(), m = 20, seed = 1)`. A plain-column right-hand side restricts the `predictors` (works for every method, including ranger/xgboost); a right-hand side with transformations (`s(x)`, `log(x)`, `I(x^2)`) becomes a model `formula` (formula-capable methods only). Grammar formulas compile to specs. Note: `vimpute()`'s signature now has `...` in second position for the grammar -- arguments after `data` must be passed by name (they always were in all documented usage).
- `vimpute()` gains `seed` for whole-run reproducibility (applied once at entry, as in mice; the `m` imputations still differ from each other).
- `vimpute()` gains `predictors`: per-variable predictor control -- the equivalent of mice's `predictorMatrix` (named list or 0/1 matrix), working for every method including `ranger` and `xgboost` (where `formula` is rejected). A variable's `formula` takes precedence over its `predictors` entry.
- `vimpute()` gains `visit_sequence`: `"asis"`, `"increasing.na"`, `"decreasing.na"`, or an explicit permutation of the NA-variables.
- `vimpute()` gains `tuned_params` to apply (and reuse) tuned hyperparameters without running the tuner; each `tuning_log` entry now carries its chosen parameters in `$params`.
- `vimpute()` gains `tune_control = vimpute_tune_control(budget = , folds = , tuner = , batch_size = )`: the tuning evaluation budget, resampling folds, tuner and batch size are user-controllable (defaults keep the built-in data-size heuristics); `tuning_log` entries record the budget and folds used. `vimpute_search_space()` exports the built-in per-learner search spaces (the starting point for `register_vimpute_method()` `search_space` hooks). Nested resampling is intentionally not offered -- the goal is good imputations, not unbiased generalisation estimates.
- With `m > 1` and `tune = TRUE`, tuning now runs **once** (in the first imputation) and the chosen parameters are shared by all `m` imputations -- previously every imputation re-tuned independently, conflating tuner noise with missing-data uncertainty. The `vimmi` object carries the tuning report in `$tuning_log`.
- A learner failure on one variable no longer aborts the whole imputation: `vimpute()` warns (naming the variable) and falls back to a featureless learner for that variable.
- **Convergence diagnostics for multiple imputation**: `vimmi` objects store per-iteration chain statistics (mean/variance of the imputed values per variable, iteration and imputation) plus the `seed`, and `plot(vimmi)` draws mice-style convergence trace plots. Single runs expose the same trace data as `attr(result, "chain")`.
- `vim_as_mids()`: the documented name for the vimmi-to-mids conversion (`as.mids.vimmi()` is kept as the historical alias; despite the dotted name it was never an S3 method -- `mice::as.mids()` is not a generic).
- **`makeMissing()`**: amputation generator for simulation studies (the variable-wise `mice::ampute()` counterpart) -- MCAR/MAR/MNAR mechanisms with exact per-variable proportions, driver `weights`, and an `attr(., "where")` indicator that plugs directly into `evaluation()`/`nrmse()`/`pfc()`.
- **`overimpute()`**: model-agnostic calibration diagnostic (the `Amelia::overimpute()` analogue) -- the observed cells of a variable are overimputed fold by fold with multiple draws; `print()` reports the empirical interval coverage and `plot()` draws the observed-vs-imputed calibration plot. Works with any method, spec, or grammar configuration.
- **Per-variable model quality by default** (missForest `OOBerror` analogue): every `vimpute()` run reports NRMSE (numeric) / PFC (factor) per variable as `attr(result, "model_error")` -- out-of-bag for ranger, honestly labelled in-sample for the other learners -- and `print(vimmi)` shows it.
- **`plot(vimmi, "density")` and `plot(vimmi, "strip")`**: observed-vs-imputed distribution diagnostics (the `mice::densityplot()`/`stripplot()` analogues), complementing the `"chains"` convergence traces.
- **New vignette** *Multiple imputation with vimpute: pooling, tuning and diagnostics*: an executed end-to-end workflow -- `makeMissing()` -> `vimpute(m = 5)` -> chain/density diagnostics -> Rubin pooling via `with()`/`mice::pool()` and `vim_as_mids()` -> tuning with `tune_control` -> `overimpute()` calibration -> `evaluation()` against the simulated truth.
- **New vignette** *Benchmarking imputation methods*: an executed, extensible benchmark harness (`makeMissing()` scenarios, NRMSE on the amputed cells, runtime) comparing vimpute ranger/robust and `kNN()` with mice and missRanger (both guarded Suggests); demo-scale replications at build time, paper-scale via one constant.
- **New vignette** *Validating multiple-imputation properness*: a known-truth coverage simulation of the pooled inference under the default (`boot` + `uncert = "pmm"`), the boot-free PMM variant, the textbook-proper (`boot + normalerror`), and a deliberately improper (bootstrap without residual noise) configuration -- demonstrating the anti-conservative pooled SEs the improper setting produces and the warning that guards against it.
- `?vimpute` states the missingness assumptions (MAR incl. MCAR; MNAR caveat with pointers to `makeMissing()` sensitivity simulation).

## Bug fixes
- **`uncert = "normalerror"` / `"resid"` no longer draw from an in-sample residual scale**: for learners that expose no scale of their own (ranger, xgboost), `sigma_hat` and the residual pool were derived from the model's predictions on its own *training* rows. A forest's in-sample predictions are near-interpolating, so the estimated scale came out at roughly half the true predictive spread (0.57 against a held-out 1.16 on a linear DGP with residual sigma 1): `"normalerror"` injected half the noise it should, `"resid"` sampled from a residual pool that was far too tight, and the pooled intervals of the textbook-proper `boot + normalerror` configuration under-covered badly (0.83 / 0.73 against a nominal 0.95 at 30% / 50% missingness). Where the fitted model exposes out-of-bag predictions -- ranger stores them for free, in training-row order -- both are now derived from those, which track the predictive spread to ~1%; every other learner keeps the in-sample fallback. Learners reporting a proper scale of their own (`lm`, `lmrob`, `glmrob`, `gam`, `robgam`) never took this path and are unchanged, as is the default `uncert = "pmm"`, which matches donors on predicted values and needs no scale estimate at all. The same residual pool feeds `robustboot = "stratified"` (the default) and `"residual"`, whose good/bad split was previously computed on the same over-tight residuals. A regression test pins both the scale and the residual pool against a held-out predictive SD.
- **`classif.glm_rob` no longer inverts binary classifications**: the robust logistic learner behind `vimpute(method = "robust")` attached the fitted binomial probability -- P of the *second* factor level, the `glm`/`glmrob` convention -- to the *first* level, so binary factor imputations drew from inverted class probabilities and the deterministic argmax picked the *less* likely class: worse-than-chance imputation exactly where the signal is strong (PFC 0.70 instead of 0.13 on the diabetes data). The multiclass one-vs-rest path was oriented correctly and is unchanged; a direction regression test now pins both paths.

## Minor improvements
- Corrected references across the documentation: the cellMCD paper is JASA 119(548), 2610--2621 (was 545, 576--588, with the first author's initial wrong); the cellGMM reference now cites Zaccaria, Garcia-Escudero, Greselin and Mayo-Iscar (2025), Technometrics 67(4), 643--654 (previously wrong co-authors and title); "Journal of Computational Statistics and Data Analysis" corrected to *Computational Statistics & Data Analysis* (on 5 help pages) and "Journal of Advances in Data Analysis and Classification" to *Advances in Data Analysis and Classification* (on 19 help pages; additionally the stale "Online first" was replaced by the final volume/pages).
- The vimpute vignette no longer describes regularisation as "robustness" (shrinkage stabilises against multicollinearity/overfitting; outlier resistance is what `"robust"`/`"robgam"` provide).
- `evaluation()` no longer returns `NaN` when one variable type has no missing cells (0/0 guarded).
- `evaluation()` gains `where`, the documented name for the amputed-cell mask (matching `makeMissing()`'s `"where"` attribute); the historical `m` keeps working, supplying both errors.
- **Scale-free convergence criterion** for sequential imputation: `eps` now bounds the per-variable *relative* change (numeric: mean squared change of the imputed values divided by the variance of the observed values; factors: share of changed categories), and the run stops when the *largest* per-variable change stays below `eps` -- previously the raw changes were summed, so `eps` was meaningless across data scales (a variable measured in thousands could block convergence forever, and one variable could mask another). The full iterations-by-variables change matrix is returned as `attr(result, "convergence")`.
- Hyperparameter tuning is reproducible across machines (`batch_size = 1` for the random search).
- After tuning, the `future` plan active at entry is restored instead of being forced to `"sequential"`.

# VIM 7.2.0
- `vimpute()` gains `keep_all_columns` (default `TRUE`): the full dataset is returned, with columns excluded via `considered_variables` passed through unchanged (matching `kNN()`/`hotdeck()`/`irmi()`); set `FALSE` for the previous considered-only shape.
- `vimpute()` warns when `m > 1` cannot produce between-imputation variability (no `boot`, `uncert`, or stochastic `pmm`), so improper multiple imputation is no longer silent.
- `vimpute()` returns ordered-factor columns as ordered factors (previously flattened to plain factors; the `m > 1` path also lost the level order).
- `vimpute(tune = TRUE)` now runs with `sequential = FALSE` (was a silent no-op).
- `vimpute()` handles per-variable method lists correctly: a named length-1 list validates the variable name, and an unnamed per-column list maps by column position.
- `rangerImpute()` and `xgboostImpute()` forward their hyperparameters to the backend learner.
- `regressionImp()` uses `lm`/`glm` as documented, falling back to regularized regression only when needed.
- `complete()` on a `vimmi` object works even when `mice` or `tidyr` is attached.
- `evaluation()` supports `vartypes = "guess"`.
- `irmi(mi > 1)` returns a list of imputations again (was a single mangled data.frame under the default `imp_var = TRUE`).
- `imputeRobust()`: `method = "gamRob"` and `uncert = "wresid"` now work (previously crashed); `uncert` is validated with a clear error; the PMM donor pool no longer includes initialised values.
- `imputeRobustChain()` repaired (previously imputed zeros or crashed on most paths).
- cellwise methods (`imputeCellIRMI()`, `imputeCellM()`, `imputeCellMCD()`) no longer scale the design matrix by cell weights, which had made them impute worse than the median on clean data; the default `init_weights` is now `"ddc"`.
- `imputeCellReg()` cell-weight computation fixed (could produce negative weights).
- `imputeCellMCD()`: documented that `boot = TRUE` does not yet propagate parameter uncertainty.
- `kNN()`/`gowerD()`: semi-continuous (`mixed`) distance variables are now range-scaled like numeric ones, so a large-scale mixed variable no longer dominates the neighbour search.
- `kNN(weightDist = TRUE)` no longer produces `NaN` imputations when distances exceed 1 (e.g. with `methodStand = "iqr"`).
- `?kNN` and `?gowerD` document the distance standardisation and the NA-sentinel convention.
- `car` moved from Imports to Suggests (Box-Cox implemented natively).

# VIM 7.1.0
- improve `vimpute()` compatibility and validation
- make `rangerImpute()`, `xgboostImpute()`, and `regressionImp()` delegate to `vimpute()`
- vimpute: fall back from regularized to robust models when too few predictor columns remain after preprocessing
- fix documentation and package check issues around `vimpute()`
- OpenMP is used in gowerD

# VIM 7.0.0
 - new function vimpute that uses `mlr3` backend for a flexible imputation method.
 
# VIM 6.2.4
 - fix infinite loop in matchImpute in case all observations of a variable are missing
 - remove parameter metric from kNN because it was not used
 - add function xgboostImpute for using a simple xgboostModel to impute
 - add imputeRobust function to impute numeric variables with robust methods (linear and non-linear ones)

# VIM 6.2.3
- default robust regression method for irmi for numeric variables changes from rlm to lmrob.

# VIM 6.1.1
- ordFun as parameter of kNN to control the function applied to ordinal variables
- methodStand option in gowerD and kNN to switch between range and interquartile range for the standardization of numerical variables
- donorcond in kNN and hotdeck extended so it also accepts NULL as list element and multiple conditions as character vector

# VIM 6.0.2
- error message in `hotdeck()` when ord_var and variable overlap
- family argument of class 'family' now work in regressionImp

# VIM 6.0.1

- add new vignettes explaining all remaining imputation methods (`irmi()`,
  `kNN()`, `hotdeck()` and `regressionImp()`). Thanks @wolfgangrannetbauer
  (#44, #45)
- Allow missing and imputed values in several visualization functions.
    - The new functionalities are showcased in the new [visualization vignette](http://statistikat.github.io/VIM/articles/VisualImp.html) (#46). Thanks
  @wolfgangrannetbauer!
- Add `tableMiss()`: A table that highlights missing and imputed values via
  colors (#47).
- Bug fix for kNN (#48) Thanks @torockel

# VIM 6.0.0

- extend documentation with new vignettes and pkgdown
- add rangerImpute() to impute values with `ranger::ranger()` (#35)
- remove support for survey objects (#36)
- remove exports for VIMGUI (#40)
- change data.table dependency from depends to imports (#41)
- bugfixes for `irmi()` with logical and integer columns (#42)

# VIM 5.1.1

* updates for `gowerD()`
* separate help pages for `maxCat()` and `sampleCat()`
* remove links to certain packages

# VIM 5.0.0

## New datasets

* bcancer
* brittleness
* colic
* diabetes
* food
* pulplignin
* toydataMiss
* wine

# VIM 4.9.1

* data set `collision` added

# VIM 4.8.0

* fixed a bug in the distance computation when different variable types are used

# VIM 4.7.18

* added `imp_var` and `imp_suffix` to `irmi()`, so it is more consistent with the other functions (#27)

# VIM 4.7.17

* added parameter addRF, only RF to `kNN()`, to use random forest in combination with `kNN()`

# VIM 4.7.12

* testthat package used for all tests
* added travis automatically building/checking
* covr for code coverage (vis functions are currently not covered by any tests)

# VIM 4.7.1
* new imputation function `matchImpute()` for imputing randomly within groups

# VIM 4.7.0

* remove handling of `impNA` in `hotdeck()`
* add regression tests
* bugfix for `irmi()` with factors (#13). Thanks @Deleetdk
* bugfix with colorspace package (#4)

# VIM 4.6.1

* use ordered logistic regression for ordinal variables in `irmi()` (#23)
* add support for ordered factors (#7)
* bugfixes (#8, #9)

# VIM 4.6.0

* bugfixes for `kNN()` and `data.table`
* bugfix for labelled vars in `hotdeck()`
* add JSS citation
* bugfix, if a `data.frame` is passed to `irmi()` (#6)
  
# VIM 4.5.0

* new option for `kNN()`: `weightDist` to use the distances for the k nearest neighbours as weights
* The R function `which.minN()` is not used anymore, instead there is a C++ function, `kNN()` is now about 1.6 times faster on a replication (100x) of the sleep dataset
* Bytecompile is enabled

# VIM 4.4.0

* bugfix  wrong observations marked as imputed in `hotdeck()`
* random sorting is now used in `hotdeck()` if no `ord_var` is defined

# VIM 4.3.0

* bugfix for `hotdeck()` with `makeNA`

# VIM 4.2.3

* bugfix for the computation of distances for ordered variables

# VIM 4.2.1

* new option for `kNN()` `useImputedDist` if the imputed values of a variable should be used in subsequent imputation of another variable.

# VIM 4.2.0

* bug fixed in `irmi()` with newer version of nnet (multinom) and if residual scale can not be computed (noise)
* Improvement Gower dist with only missing values in data.x or data.y

# VIM 4.1.0

* new parameter `modelFormula` in `irmi()`
* bug fixes in `irmi()`
* updated `hotdeck()` based on data.table -> faster and quite stable
* bug fix if range of a variable is 0 in gower.dist
* small fixed `kNN()`

# VIM 4.0.1

* small bugfix for using `makeNA` in `kNN()`
* "Nothing to impute"-Error is now a warning
* `imp_var` now updates existing TF `imp_vars` (with warning)
 
# VIM 4.0.0

* new pacakge VIMGUI contains all GUI functions
* vignettes moved to VIMGUI
* new imputation function `regressionImp()`
* roxygen style comments -> help files
