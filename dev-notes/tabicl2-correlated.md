# TabICL variants on a small correlated MCAR dataset

This diagnostic uses 160 rows and one predeclared seed (20261016). X is standard
normal; A is sampled independently from A/B and B from B/C, with replacement.
The conditional model is

    Y | X,A,B ~ Normal(mu[A,B] + 4*X, 0.5^2)

| A | B | mu |
|---|---|---:|
| A | B | -8 |
| A | C | -2 |
| B | B | 2 |
| B | C | 10 |

Consequently Y conditional on A,B alone is also normal, with mean mu[A,B]
and variance 16.25. The four group means include an interaction. A and B are
independent by design; the strong relationship is between Y and its predictors,
not an artificial correlation between independently sampled factors.

MCAR now masks exactly 32 cells (20%) in Y only. X, A and B remain fully
observed and unchanged in every completion. All methods receive the same data
and mask, with the same model seed. Numeric errors and pairwise differences
are evaluated only on the masked Y cells. Fully observed predictors are not
reported as imputation targets.

With no missing predictors, sequential passes have no updated covariates to
feed back into Y. A difference between sequential and single-pass results
would therefore reflect refitting behavior rather than recovery of missing
predictor information. The sequential variant retains its ten-sweep cap and
normal early stopping.

The three methods are vimpute_tabicl2, vimpute_tabicl2_finetune and
vimpute_tabicl2_finetune_seq10. The initial five-epoch run matched the UCI benchmark budget: eight inference
ensemble members, CPU, one thread, learning rate 1e-5,
patience three, and two ensemble members for fine-tuning and validation.
PMM and residual uncertainty are disabled. Sequential imputation is capped at
ten sweeps and can stop earlier under the existing convergence rule.

The experiment records constructor names and arguments for every fitted model.
It checks schema, finite numeric predictions, complete targets, factor levels,
and exact preservation of observations. The tests check fine-tuning and
sequential routing. They deliberately do not assert that fine-tuning must win:
that is an empirical outcome, not a software correctness requirement.

Run the fast data-design/routing test after loading the working package:

```r
pkgload::load_all(".")
tinytest::run_test_file("inst/tinytest/test_tabicl_correlated.R")
```

Run the real-model test in a configured TabICL 2.2 environment with cached
checkpoints. Routine package checks skip it and never initialize Python:

```r
Sys.setenv(VIM_TEST_TABICL = "true",
           VIM_TABICL_CORRELATED_RESULTS = ".local/benchmarks/tabicl2-correlated-y-only-epochs20")
pkgload::load_all(".")
tinytest::run_test_file("inst/tinytest/test_tabicl_correlated_real.R")
```

The real-model run uses cached checkpoints and offline mode.

Set RETICULATE_PYTHON and HF_HOME to the configured environment and checkpoint
cache, and HF_HUB_OFFLINE=1 to prevent checkpoint downloads. The standalone
experiment can also be sourced from inst/benchmarks/tabicl2-correlated.R and
run with tabicl_correlated_run(epochs = 20L). The correlated experiment now
uses 20 epochs by default; epochs = 5L reproduces the earlier budget. This is
a maximum: patience-based early stopping remains enabled. The underlying
TabICL learner keeps an explicit default of five epochs. It is exposed as
learner_params = list(tabicl2 = list(finetune = TRUE, epochs = 20L)) in vimpute(),
or learner_params = list(epochs = 20L) in tabiclImpute(..., finetune = TRUE).

The experiment returns the data, mask, completions,
metrics, pairwise differences and constructor diagnostics; output= saves them.

## Predictor-preprocessing bug found by this test

The initial real-model run returned one constant value per target in both
single-pass variants despite the strong data signal. The first-sweep feature
regression test failed on the original code: all three original predictors
had been replaced by binary missingness indicators. PipeOpMissInd generates
indicator columns in place of its input features; the pipeline did not retain
an original-feature branch. MCAR indicators carry no intended signal about Y.

The TabICL path now bypasses that indicator-only pipeline, as the XGBoost path
already did. Original predictor values, including missing values supported by
TabICL, reach the learner. The same exclusion applies to the semicontinuous
regressor path. Other methods retain their existing preprocessing.

A separate all-column-missing regression case checks that all three input
predictors reach each first sweep fit, that numeric signal values survive rather than only 0/1 indicators,
and that native training-time missings are retained. This test fails before
the fix and passes afterward.

The existing UCI benchmark was generated before this fix. Its report is marked
superseded, and the benchmark protocol is versioned so ordinary resume cannot
silently reuse it for the corrected implementation. A separate cached-only
precompute mode can reproduce the old report without Python while retaining
the visible superseded warning. Corrected UCI comparisons require a new run;
this diagnostic does not launch another large benchmark.

## Other reasons variants can look similar

Fine-tuning and sequential imputation change different parts of the workflow.
Fine-tuning adapts each target model's pretrained weights; sequential imputation
refits target models on updated covariates. More sweeps do not add fine-tuning
epochs to a persistent model: each fit creates a fresh estimator.

Five epochs at a learning rate of 1e-5 are a small adaptation budget. In the
installed TabICL 2.2 implementation, _finetune/base.py evaluates the initial
pretrained weights, includes them as the best validation state, and restores
the best state after training. A fine-tuning run can therefore legitimately
return the pretrained weights if validation does not improve. Equal hard-label
accuracy can also hide changes in probabilities or in which rows are correct.
This experiment establishes which variants actually ran and measures changes
in individual imputations; it does not prove why any particular fit selected
its best weights.

A single small sample is a diagnostic, not evidence of comparative superiority.
Other seeds, missingness rates and fine-tuning budgets need a separate,
predeclared experiment before drawing general performance conclusions.

## Current Y-only experiment

The current run keeps the 20-epoch maximum, learning rate 1e-5 and patience
three. Data generation and model seeds are unchanged; because only Y is now
masked, its MCAR draw uses the first random-sampling stream rather than the
second stream used when X was also masked. Earlier all-column-missing results
remain in their separate local cache directories and are not overwritten.

| Method | Y RMSE | Y NRMSE | Sweeps | Seconds |
|---|---:|---:|---:|---:|
| vimpute_tabicl2 | 0.554 | 0.0650 | 1 | 1.3 |
| vimpute_tabicl2_finetune | 0.605 | 0.0710 | 1 | 8.5 |
| vimpute_tabicl2_finetune_seq10 | 0.551 | 0.0645 | 3 | 11.6 |

All 39 data-design/routing checks and 15 real-model checks pass. Every
completed dataset preserves X, A and B exactly, and only the 32 masked Y
values contribute to the metrics. The sequential model stopped after three
sweeps. These results describe one small sample and do not establish a
general advantage for fine-tuning or sequential refitting.

Current raw results: .local/benchmarks/tabicl2-correlated-y-only-epochs20/.
