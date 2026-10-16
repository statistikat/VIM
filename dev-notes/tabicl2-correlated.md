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

MCAR masks exactly 32 cells (20%) in each of X, Y, A and B. All methods receive
the same data and mask, with the same model seed. Masking predictors as well as
Y gives sequential imputation an opportunity to use newly imputed covariates.
Numeric errors and classification accuracy are evaluated only on masked cells.
Pairwise differences compare individual imputations, including categorical
labels, rather than relying only on aggregate scores.

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
           VIM_TABICL_CORRELATED_RESULTS = ".local/benchmarks/tabicl2-correlated-epochs20")
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

The regression test checks that all three input predictors reach each first
sweep fit, that numeric signal values survive rather than only 0/1 indicators,
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

## Real-model results after the fix (five epochs)

The generated complete data have R-squared 0.99636 for Y ~ X + A*B.

| Method | X NRMSE | Y NRMSE | A accuracy | B accuracy | Fits | Sweeps | Seconds |
|---|---:|---:|---:|---:|---:|---:|---:|
| vimpute_tabicl2 | 0.604 | 0.552 | 84.4% | 84.4% | 4 | 1 | 5.2 |
| vimpute_tabicl2_finetune | 0.599 | 0.562 | 81.2% | 78.1% | 4 | 1 | 13.3 |
| vimpute_tabicl2_finetune_seq10 | 0.575 | 0.574 | 81.2% | 78.1% | 20 | 5 | 58.5 |

These measurements use the real cached TabICL 2.2 backend on CPU. The sequential
variant stopped under the existing convergence rule after five sweeps, not ten.
All 32 masked X and all 32 masked Y imputations differ between pretrained and
fine-tuned single-pass fits. Their mean absolute Y difference is 0.170; one A
classification and two B classifications also change. This confirms that
fine-tuning is executed and can affect results without guaranteeing improvement.

Before the fix, both single-pass variants produced constant imputations with
Y NRMSE 1.019 and A/B accuracy 40.6%. The sequential variant eventually recovered
actual predictors on later sweeps; it used all ten sweeps. The corrected
single-pass pretrained model has Y NRMSE 0.552 and A/B accuracy 84.4%.

The 35 data-design/routing/predictor checks and 12 real-model checks pass.
The 34 existing TabICL checks and 37 benchmark checks also pass: 118 checks
in total. The superseded UCI report was regenerated in cached-only mode with
RETICULATE_PYTHON=/nonexistent, confirming report generation needs no Python.

Raw generated data, masks, completed datasets and constructor arguments are
saved locally in .local/benchmarks/tabicl2-correlated/experiment.rds. The
before-fix diagnostic is retained separately in
.local/benchmarks/tabicl2-correlated-before-predictor-fix/.

## Twenty-epoch experiment

The same data, mask, seeds, learning rate and patience were used with epochs = 20L.
The learner default remains five epochs; twenty is the diagnostic experiment
override. Early stopping remains enabled, so twenty is a maximum.

| Method | X NRMSE | Y NRMSE | A accuracy | B accuracy | Sweeps | Seconds |
|---|---:|---:|---:|---:|---:|---:|
| vimpute_tabicl2 | 0.604 | 0.552 | 84.4% | 84.4% | 1 | 5.5 |
| vimpute_tabicl2_finetune | 0.605 | 0.567 | 81.2% | 78.1% | 1 | 15.6 |
| vimpute_tabicl2_finetune_seq10 | 0.575 | 0.579 | 81.2% | 78.1% | 5 | 60.7 |

Constructor diagnostics confirm epochs = 20 for every fine-tuned model.
The parameter-default, routing and benchmark suites pass 111 checks; the real
twenty-epoch experiment passes 12 checks. The saved results are in
.local/benchmarks/tabicl2-correlated-epochs20/.
