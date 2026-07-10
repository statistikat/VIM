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
