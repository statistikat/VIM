# VIM 7.3.0 (development)

## Breaking changes
- **`vimpute()` returns are type-stable**: the result is always the imputed data, classed like the input (data.frame in, data.frame out; data.table in, data.table out). With `tune = TRUE` or `pred_history = TRUE` the diagnostics are attached as `attr(result, "tuning_log")` / `attr(result, "pred_history")` instead of switching the return to a bare list.
- **`vimpute()`'s default `uncert` is now `"pmm"`** (random draw among the 5 nearest donors): default numeric imputations are observed donor values with an honest distribution, instead of deterministic conditional means. Set `uncert = "none"` for the previous behaviour. Default `m > 1` runs are now stochastic between imputations. `rangerImpute()`, `xgboostImpute()` and `regressionImp()` keep their deterministic behaviour (`uncert = "none"` pinned internally).

## New features
- **Method registry**: `vimpute()`'s imputation methods are resolved through a package-level registry, and `register_vimpute_method()` adds a user-defined method backed by any mlr3 learner pair in one call -- e.g. `register_vimpute_method("cart", learner = list(regr = "regr.rpart", classif = "classif.rpart"), packages = "rpart")`, or lightgbm via `mlr3extralearners` -- usable everywhere the built-in names work (global `method`, per-variable lists, method-keyed `learner_params`, `tune = TRUE` via an optional `search_space` hook). `vimpute_methods()` lists the registered methods, `unregister_vimpute_method()` removes user-added ones. The six built-ins are seeded through the same contract; unsupported-method errors now name the offending method and list what is registered.
- `vimpute()` gains `seed` for whole-run reproducibility (applied once at entry, as in mice; the `m` imputations still differ from each other).
- `vimpute()` gains `predictors`: per-variable predictor control -- the equivalent of mice's `predictorMatrix` (named list or 0/1 matrix), working for every method including `ranger` and `xgboost` (where `formula` is rejected). A variable's `formula` takes precedence over its `predictors` entry.
- `vimpute()` gains `visit_sequence`: `"asis"`, `"increasing.na"`, `"decreasing.na"`, or an explicit permutation of the NA-variables.
- `vimpute()` gains `tuned_params` to apply (and reuse) tuned hyperparameters without running the tuner; each `tuning_log` entry now carries its chosen parameters in `$params`.
- With `m > 1` and `tune = TRUE`, tuning now runs **once** (in the first imputation) and the chosen parameters are shared by all `m` imputations, as in mice -- previously every imputation re-tuned independently, conflating tuner noise with missing-data uncertainty. The `vimmi` object carries the tuning report in `$tuning_log`.
- A learner failure on one variable no longer aborts the whole imputation: `vimpute()` warns (naming the variable) and falls back to a featureless learner for that variable.

## Minor improvements
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
