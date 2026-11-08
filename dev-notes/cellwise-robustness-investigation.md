# Cellwise contamination: diagnosis and changes

The numeric regression fit was reasonably robust to response contamination,
but two later steps defeated that robustness: raw contaminated predictor
values entered prediction, and PMM could copy contaminated observed responses.
Both problems need addressing. Merely disabling PMM does not solve predictor
contamination; merely cleaning predictors does not remove bad donor values.

## Controlled experiment

`benchmarks/cellwise-ablation.R` recreates the vignette's exact synthetic
population, sample rows, MCAR/MAR masks and 10% contamination plan. It runs
predictor-only (`x`, `z`), response-only (`continuous`), combined, and clean
conditions, with deterministic and PMM imputation. Four safeguard combinations
are tested: off, prediction guard only, donor guard only, and both. There are
192 fits: 2 mechanisms × 3 replications × 4 contamination sources × 2 uncertainty
settings × 4 safeguard combinations. No true contamination labels enter the
learner; those labels are used only to count copied contaminated donors.

Mean numeric NRMSE across the six paired replications:

| Contamination | Deterministic, guards off | PMM, guards off | Deterministic, both guards | PMM, both guards |
| --- | ---: | ---: | ---: | ---: |
| Clean | 0.370 | 0.526 | 0.370 | 0.526 |
| Predictors only | 2.098 | 0.796 | 0.447 | 0.594 |
| Response only | 0.369 | 2.999 | 0.369 | 0.509 |
| Both | 2.139 | 2.783 | 0.443 | 0.613 |

With response-only contamination and guards off, 13.9% of PMM imputations
copied contaminated donors. With both contamination sources it was 11.4%.
Both guards reduced this to zero in these runs. For combined contamination,
prediction guarding alone yielded PMM NRMSE 2.741, donor screening alone 1.243,
and both 0.613. Donor screening alone still copied contaminated donors in 0.8%
of imputations: distorted prediction scores can make a bad response look
plausible. This supports addressing both stages, not substituting one for the
other. S-estimation convergence warnings occurred in some diagnostic variants;
these are empirical finite-sample comparisons, not estimator guarantees.

Full per-run evidence is in `benchmarks/cellwise-ablation-results.csv`.
To reproduce from the package root:

```r
pkgload::load_all(".")
source("dev-notes/benchmarks/cellwise-ablation.R")
# `result` contains all individual scores and contaminated-donor fractions.
```

## Implementation

1. Both mlr3 cellwise learners now default to `clean_predictors = TRUE`.
   A numeric prediction cell farther than `predictor_cutoff = 4.5` robust
   scales from its training median is replaced internally with that median.
   Training centers/scales are frozen and do not depend on prediction batches,
   targets, or simulation truth. Scale is MAD with the existing IQR/unit fallback.
   The original weighted training design is unchanged, as are caller-supplied
   observed values. Factors are not recoded by this guard.
2. The regression learner stores the median and robust scale of its training
   residuals using the guarded predictions. VIM's uncertainty PMM, legacy
   `pmm = TRUE`, and midastouch reject donors more than `donor_cutoff = 4.5`
   scales from this residual center. The residual scale is MAD, then IQR,
   with a numerical floor for exact fits. Calibration is recomputed within
   each bootstrap fit. No eligible donors means a warning and point predictions,
   rather than silently reinstating the contaminated pool.
3. A dedicated cellwise model lookup finds calibration through mlr3 pipeline
   state. Preprocessing nodes can themselves contain a `model` field, so simply
   selecting the first nested model is insufficient. Generic model-unwrapping
   behavior for other methods is unchanged.

Set `clean_predictors = FALSE` and (for regression) `donor_cutoff = Inf` to
recover the previous scoring and donor-selection behavior. These settings
power the ablation's controls. Other vimpute methods retain their behavior.

## Recomputed full benchmark

All 144 fits were rerun with the original scenarios, seeds, method settings and
contamination levels. The generated Rmd, tables and figures were regenerated.
At 10% contamination:

| Mechanism | Numeric NRMSE before → after | Binary PFC before → after | Multiclass PFC before → after |
| --- | --- | --- | --- |
| MCAR | 2.336 → 0.585 | 0.367 → 0.317 | 0.528 → 0.489 |
| MAR | 3.229 → 0.640 | 0.411 → 0.350 | 0.556 → 0.572 |

Clean mixed-data scores were unchanged in this experiment. Improvements are
not universal: MAR multiclass PFC worsened slightly. Paired per-run before/after
values are in `benchmarks/cellwise-benchmark-comparison.csv`.

## Limits and validation

This is a transparent marginal gross-outlier guard, not a new conditional
cellwise detection algorithm. It can replace legitimate tail values and lose
predictive information; disable it or raise the cutoff when extrapolation is
intended. It can miss anomalous combinations of individually plausible values.
Likewise, donor rejection can narrow the imputation distribution. This change
does not establish proper multiple imputation or categorical mislabel detection.
The benchmark has three replications per mechanism and one synthetic shift
model, so it does not establish a general robustness ranking.

Regression tests cover prediction magnitude/batch invariance, serialization,
unchanged observed cells, raw-behavior opt-out, classifier probabilities,
training-only calibration, exact-fit scale floors, pipeline extraction,
bootstrap, PMM donor membership and both PMM APIs plus midastouch. Existing
cellwise, registry, specification and PMM tests are also run.

Final validation: 163 checks across seven test files passed. All 144 benchmark
runs and the 192-fit ablation completed; the vignette rendered to HTML and its
plots were inspected. Competing methods' per-run NRMSE and PFC values were
checked against the prior benchmark and were unchanged. Normal numerical
warnings from robust fitting and a Pandoc deprecation warning remain; a full
R CMD check was not run for this change.
