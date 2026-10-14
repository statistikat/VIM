# Model-agnostic edit restrictions

The feature branch is based on `tabicl2-impute` in an isolated worktree so its
long-running benchmark is unaffected.

The existing restricted learner receives `learner_params[[variable]]$rules`,
filters edits to its target, constructs constraints on design-matrix
coefficients and solves its original ECOS problem during prediction. This
implementation and its uncertainty override are unchanged.

The new layer is inserted after `apply_imputations()` has run for every target
in a sweep, immediately before `check_convergence()`. It sees completed numeric
values rather than learners. The original masks, including `makeNA`, control
eligibility. All referenced fixed values, including excluded columns, are
available from the original input. Adjusted data update convergence, chain
statistics and the next sweep. Result construction is followed by validation.
Multiple-imputation recursion propagates both new arguments and stores per-run
summaries, with optional solver problems attached to reconstructed datasets.

`R/vimpute_edit.R` reuses expression extraction, conjunction expansion,
parenthesis/range normalization, `restricted_mip_rule`, and linear coefficient
parsing from `R/restricted_helpers.R`. It substitutes fixed values before
building row-sized matrices. The legacy regression parser/solver is not changed.
The centered ECOS norm epigraph minimizes the same positive weighted quadratic
distance, with status and residual checks. Feasible predictions bypass ECOS.

The new public API is `edit_rules`, `edit_control`, and
`vimpute_edit_control()`. Documentation and examples are in
`vignettes/vimpute-edit-restrictions.Rmd`. Standalone analytical/performance
tests are in `inst/tinytest/test_vimpute_edit_rules.R`; model, dual-validator,
MI, uncertainty, mask, class, spec/grammar and convergence checks are in
`inst/tinytest/test_vimpute_edit_integration.R`.

There are no new mandatory dependencies. `validate` and `ECOSolveR` are checked
only when generic rules are requested. NULL rules retain the previous path and
RNG behavior. Nonlinear, integer and nonconvex restrictions need separate future
solver support. Predictive-uncertainty weights, cross-record edits and relaxed
infeasibility policies would require separate methodological design and tests.

Validation: the initial targeted regression run passed 368 checks, including
legacy restricted regression and TabICL behavior. The new business-survey
projection check had maximum absolute residual 1.14e-13. Package-check results
are recorded below after checking the final source.

## Final verification

The final standalone and workflow suites pass 235 checks (46 analytical,
parsing, failure and performance checks; 189 integration/workflow assertions).
Together with 141 unchanged legacy regression checks, 376 core targeted checks pass. A further 36 legacy MI/interface checks pass,
bringing the targeted total to 412.
The combined restricted-regression and edit-restrictions vignette renders successfully. The new
vignette shows examples without executing Python during package builds.

The final package build succeeds. R CMD check was run with --no-manual and
--ignore-vignettes (the relevant vignettes were rendered separately),
NOT_CRAN=false and _R_CHECK_FORCE_SUGGESTS_=false. The initial check reports 1 ERROR, 1 WARNING,
1 NOTE: 2/894 tests fail at test_cellGLoc.R lines 343 and 504; an existing Clang
-Wfixed-enum-extension compiler warning; and an Rd reference to the unavailable
robCompositions package. An independent Git archive of tabicl2-impute checked
under the same configuration reports the identical failures and warning/note,
with 2/848 tests failing. A repeated feature check reports 1 ERROR and
1 WARNING with the same 2/894 failures; the unavailable cross-reference is
listed as INFO in that run. The 46 added package-check assertions all pass.
Optional packages cellWise, crmReg, mice and missRanger are unavailable in this
environment; their optional paths cannot be verified here.

## Changed files

- Core integration: R/vimpute.R, R/vimpute_edit.R, R/vimmi.R.
- Convenience wrappers: R/rangerImpute.R, R/xgboostImpute.R,
  R/regressionImp.R, R/tabiclImpute.R.
- Public API/documentation: NAMESPACE, man/vimpute.Rd,
  man/vimpute_edit_control.Rd, man/rangerImpute.Rd, man/xgboostImpute.Rd,
  man/regressionImp.Rd, man/tabiclImpute.Rd.
- Tests: inst/tinytest/test_vimpute_edit_rules.R,
  inst/tinytest/test_vimpute_edit_integration.R.
- Combined vignette: vignettes/vimpute-edit-restrictions.Rmd; replaces
  vignettes/vimpute-restricted.Rmd and includes the mathematical setup.
- Architecture and validation record: dev-notes/edit-restrictions.md.

R/restricted_helpers.R, the legacy coefficient optimizer, the registry,
DESCRIPTION, and the running TabICL benchmark configuration are unchanged.
