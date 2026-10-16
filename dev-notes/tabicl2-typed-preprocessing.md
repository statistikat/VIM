# Typed and consistent TabICL preprocessing

Implemented on edit-restriction_tabicl2-impute, 2026-10-16, following items 1
and 2 of tabicl2-accuracy-proposal.md. No tuning/ensemble policy was changed.

R/tabiclImpute.R now uses input_type = "dataframe" by default. It stores each
training factor's schema and passes an explicit pandas categorical column,
including unused levels and -1 missing codes. Unknown levels outside that
schema become categorical missings. Numeric columns retain NaN values for the
fitted upstream transformer. Ordered predictors use unordered categorical
storage in Python; output classes are restored by the existing result builder.

input_type = "matrix" retains the earlier numeric factor encoding as an
explicit compatibility option. This controls representation, not a separate
R-side missing-value imputer. Neither input_type nor other R-only controls are
forwarded to Python constructors. Pandas is now an explicit dependency in
inst/python/tabicl-requirements.txt.

R/helper_vimpute.R no longer fills TabICL predictor missings in the ordinary
prediction, PMM donor-scoring or semicontinuous regression paths. Task target
placeholders are still filled where mlr3 requires a nonmissing target. Other
learners and the semicontinuous zero classifier retain their preprocessing.
The older indicator-only-pipeline fix remains in place.

## Verification

472 checks passed across the preprocessing, existing TabICL, correlated-data,
benchmark, edit-restriction, restricted-regression, seed, return-type, wrapper,
and multiple-imputation suites. A further 32 opt-in checks verify direct Python
parity for pretrained/fine-tuned regression/classification, actual categorical
dtypes, unknown and unused levels, entirely missing numeric and categorical
predictors, and single-row prediction. Independent manual pandas frames are
used for the reference fits. All point/probability parity comparisons passed
at tolerance 1e-6. The real correlated experiment passed another 15 checks:
519 checks in total. Some upstream warnings occur for all-missing predictors,
but predictions remain finite and match direct Python.

New tests are inst/tinytest/test_tabicl_preprocessing.R and
inst/tinytest/test_tabicl_preprocessing_real.R. Real-model tests remain opt-in;
normal package checks do not initialize Python or download checkpoints.

## Restarted experiments

The small experiment retains 160 rows, Y-only 20% MCAR and a maximum of 20
fine-tuning epochs. It completed with Y RMSE 0.554 pretrained, 0.605 fine-tuned,
and 0.551 fine-tuned sequential (three sweeps). X/A/B remain unchanged. Results
are in .local/benchmarks/tabicl2-correlated-y-only-typed-epochs20/. These match
the prior Y-only run because that scenario has fully observed predictors.

The larger Adult/Bank protocol remains n = 250, 500, 1000, 2000, three
repetitions, MCAR/MAR and missingness rates 0.1/0.2/0.4, with all nine reference
and TabICL methods: 1296 method/scenario attempts. Its fine-tuning budget remains
five epochs. The learner default also remains five; the small experiment's
20-epoch override is separate.

Protocol makeMissing-evaluation-tabicl2-v3-typed-consistent-predictors uses a
fresh .local/benchmarks/tabicl2-typed-predictors-3reps/ directory. Previous
TabICL completions are not reused. The existing scenario checkpoints resume
only matching runs of this protocol. Existing raw caches are retained.

The local runner writes .local/benchmarks/tabicl2-typed.status, its source Git
commit to tabicl2-typed-commit.txt, and output to tabicl2-typed-run.log. On
completion it precomputes and renders the benchmark vignette and copies compact
summaries into dev-notes/benchmarks/tabicl2-results/. The existing rendered
report remains superseded until those new results are complete.
