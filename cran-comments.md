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

This is a resubmission of 7.3.0 (third pre-test round). The 2026-09-01
pre-test passed all checks on Windows and Debian (Status: OK) except one
NOTE, which is addressed:

* NOTE "Overall checktime 11 min > 10 min" (r-devel-windows; tests 167 s,
  vignette rebuild 129 s): the two remaining executed vignettes (vimpute,
  vimpute-mi) are now precomputed like the simulation vignettes -- their
  code is executed by `vignettes/precompute.R` in the source repository and
  the output is embedded, so the check only renders text; thirteen further
  regression-test files moved behind the `NOT_CRAN`/`at_home()` gate (CRAN
  keeps the fast core, in particular the dependency-facing tests:
  `mice::complete()`/`tidyr::complete()` dispatch on `vimmi`, the mlr3
  issue-98 pin, the xgboost/ranger backends and the base-graphics smoke
  tests); and a dead all-comment test script was removed (one R process
  fewer). Local `R CMD check --as-cran` on the same machine as the previous
  round: tinytest suite 42 s -> 18 s, vignette rebuild 45 s -> 25 s. Scaled
  by the measured Windows/local ratios of the 2026-09-01 pre-test, the
  expected overall Windows checktime is ~8.5 min.

### Second round (2026-09-01)

The 2026-08-29 reverse-dependency report flagged MIGEE ("replacing previous
import 'VIM::complete' by 'mice::complete'"): 7.3.0 had introduced an
exported `complete()` generic that collided with the `mice`/`tidyr` ones in
packages importing both. VIM no longer exports a `complete()` generic: the
documented extractor is `vim_complete()`, and the `vimmi` method is
registered on `mice::complete()` and `tidyr::complete()` via delayed S3
registration (`S3method(mice::complete, vimmi)`), so `complete(obj, 1)`
keeps working for users of those packages. The CRAN version of MIGEE loads
warning-free against this tarball; no change is needed on their side.

### First round (2026-08-28)

The first pre-test reported one WARNING and one NOTE; both fixed:

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

(`R CMD check --as-cran` on the release tarball, local macOS; only the
machine-local "unable to verify current time" timestamp NOTE appears when
the world-clock service is unreachable.)

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
