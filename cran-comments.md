# VIM 7.3.1

Patch release. Its purpose is to remove a structural risk for VIM's twenty
reverse dependencies; there are no new features and no change in behaviour
when the suggested packages are installed.

## The change

The mlr3 stack -- `mlr3`, `mlr3pipelines`, `mlr3learners`, `mlr3tuning`,
`paradox`, plus `R6` and `future` -- moved from `Imports` to `Suggests`.

Those packages back `vimpute()` and nothing else: 6 of 58 R files reference
them. As hard dependencies they nevertheless tied VIM, and every package
importing VIM, to the fate of the mlr3 chain, since reverse dependencies are
archived recursively. None of VIM's reverse dependencies executes mlr3-backed
code -- they use `kNN()`, `hotdeck()`, `gowerD()` and `sampleCat()`, which are
mlr3-free -- so a single archived link would have taken down a chain of
packages that never needed it.

VIM's exported surface is unchanged (byte-identical NAMESPACE exports), so no
reverse dependency needs to change. What changes is availability: `vimpute()`,
`vimpute_spec()`/`vs_*()` and `vimpute_search_space()`, together with
`regressionImp()`, `rangerImpute()`, `xgboostImpute()` and `overimpute()`,
which delegate to `vimpute()`, now stop with one actionable message naming
every missing package. The visualisation, donor-based and IRMI machinery is
unaffected and runs without the stack installed.

Conditional use is enforced throughout: the three exported entry points that
reach mlr3 check the stack first, 34 tinytest files and the two runnable
examples that call the affected functions are gated, and the five vignettes
that exercise them skip their chunks rather than fail. `lgr::get_logger()`,
previously called unconditionally although `lgr` is suggested, is now guarded.

This release also carries the check-time reductions made after the 7.3.0 tag:
the `vimpute` and `vimpute-mi` vignettes are precomputed like the simulation
vignettes, and thirteen further long-running regression-test files run only
with `NOT_CRAN=true`.

## Test environments

* local macOS (Darwin 25.0), R 4.5.2
* GitHub Actions: macOS-latest (R release), windows-latest (R release and
  devel), ubuntu-latest (R release and devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked twice on the release tarball, local macOS:

* `NOT_CRAN=true R CMD check` (full test suite, mlr3 stack installed) --
  Status: OK
* `_R_CHECK_DEPENDS_ONLY_=true R CMD check` (the entire Suggests tree hidden,
  i.e. the noSuggests flavour) -- Status: OK

The second run is the one that matters for this release: it confirms that VIM
installs, loads, checks and runs its examples, tests and vignettes with no
part of the mlr3 stack present.
