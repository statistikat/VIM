# VIM 7.3.0

Feature release. Highlights (full list in NEWS.md): a method registry that
opens `vimpute()` to any mlr3 learner (`register_vimpute_method()`),
per-variable imputation specs and a formula grammar, multiple-imputation
diagnostics (convergence chains, density/strip plots, `overimpute()`
calibration, per-variable model quality), an amputation generator for
simulation studies (`makeMissing()`), user-controllable hyperparameter
tuning (`vimpute_tune_control()`), two new executed vignettes (benchmark
harness; multiple-imputation coverage validation), and corrected literature
references across the documentation.

## Resubmission

This is a resubmission of 7.3.0. The CRAN incoming pre-test (R-devel,
Windows) reported one WARNING and one NOTE; both are fixed:

* WARNING "namespace references in data files" (`data/lse_synthetic_rules.rda`
  referenced the Suggests-only `validate` namespace): the rule sets are now
  stored as plain data frames of rule text and `validate::validator(.data = )`
  rebuilds the validator objects on demand (vignette, tests and documentation
  updated accordingly).
* NOTE "(possibly) invalid URL" in `man/diabetes.Rd`: the Kaggle page answers
  404 to non-browser clients, so the source now names the Kaggle dataset
  without a link and cites the original publication (PMC) instead.
* In addition, the `diabetes` example data set is now a synthetic version
  (generated with the synvey package from `mlbench::PimaIndiansDiabetes2`):
  the team that collected the original Pima Indians Diabetes data has asked
  for redistribution to stop. Same variables, types and dimensions, so all
  examples run unchanged.
* NOTE "Examples with CPU time > 2.5 times elapsed time" (Debian pre-test):
  the OpenMP loops in the Gower-distance C++ code (`kNN()`, used by the
  flagged examples) ran on every core. They are now capped at 2 threads
  under `R CMD check` (`_R_CHECK_LIMIT_CORES_`; `options(VIM.ncores = )`
  otherwise), and the `xgboostImpute()` example was reduced to a single call
  fitting two models (its CPU time on the Debian pre-test was 5.2 s).
* NOTE "(possibly) invalid URL" in `inst/doc/irmi.html` (timeout reaching
  www150.statcan.gc.ca from the CRAN machine): the two hyperlinks in the
  `irmi` vignette -- a Statistics Canada PDF and an `http://` file-server copy
  of a CSDA paper -- were replaced by plain-text references.
* NOTE "Overall checktime 14 min > 10 min" (Windows pre-test; tests 331 s,
  of which the tinytest suite 280 s, and 282 s rebuilding the vignettes):
  the two simulation vignettes (benchmark harness, multiple-imputation
  coverage) are now precomputed -- their code is executed by
  `vignettes/precompute.R` in the source repository and the output is
  embedded, so the check only renders text; the comparison chunks of the
  vimpute vignette use single-sweep imputations and the `overimpute()` demo
  three folds; and the long-running tests (hyperparameter tuning, the
  multiple-imputation regression tests and the two vimpute integration
  scripts) run only when `NOT_CRAN=true` (CI and `devtools::check()`), CRAN
  running the fast core of the suite. On the same machine as before, the
  local `R CMD check --as-cran` now spends 41 s in the tinytest suite
  (127 s before) and 47 s rebuilding the vignettes (140 s before).

## Test environments

* local macOS (Darwin 25.0), R 4.5.2
* GitHub Actions: macOS-latest (R release), windows-latest (R release and
  devel), ubuntu-latest (R release and devel) — all green

## R CMD check results

0 errors | 0 warnings | 0 notes

(`R CMD check --as-cran` on the release tarball, local macOS.)

## Breaking changes

Documented in NEWS.md; they concern only the `vimpute()`/`vimmi` interface
introduced in VIM 7.0.0/7.1.0 (type-stable returns, default
`uncert = "pmm"`, bootstrap refits by default when `m > 1`, `with()` on a
`vimmi` now returns a mice-compatible `mira`). The long-standing
`kNN()`/`hotdeck()`/`irmi()` and visualization APIs are unchanged.

## Reverse dependencies

VIM has 23 CRAN reverse dependencies. R CMD check of the packages most
exposed to this release's changes passes against VIM 7.3.0: simputation
(wraps `VIM::kNN`), sdcMicro, robCompositions, deepImp, simPop. micemd and
clusterMI could not be checked locally because unrelated dependencies of
theirs are not available in the check environment.
