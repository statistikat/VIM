# Cellwise methods in vimpute

## Implemented

`vimpute(method = "cellwise")` uses the mlr3 learners
`regr.cellwise` and `classif.cellwise`. Register it for direct mlr3 use with
`VIM::register_cellwise_learners()`.

```r
vimpute(data, method = "cellwise",
        learner_params = list(cellwise = list(engine = "cellwise-mm")),
        uncert = "pmm", seed = 42)

vimpute(data, method = "cellwise",
        learner_params = list(cellwise = list(
          engine = "irwls", init_weights = "mcd",
          weight_function = "huber", alpha = 1.345)),
        uncert = "none", seed = 42)
```

The default engine is `crm`, matching `imputeCellReg()`, and requires `crmReg`.
Other engines use VIM and its existing robustbase dependency. Registration
itself does not require crmReg. The package's optional mlr3 stack stays optional.

| Engine | Implementation | Parameters |
| --- | --- | --- |
| `crm` | `.engine_crm()` from `imputeCellReg.R`; retains its lmrob fallback on CRM fit failure | `engine` |
| `cellwise-mm` | `.engine_cellwise_mm()`; initialize continuous cell weights on training rows and combine them into row weights for MM regression | `init_weights`, `weight_function`, `alpha` |
| `shooting-s` | `.engine_shooting_s()`; existing contribution-based cell flagging and repeated S fitting | `max_shooting` |
| `irwls` | `cellIRWLS()`, also used by `imputeCellM()` and `imputeCellIRMI()` | `init_weights`, `weight_function`, `alpha`, `maxit_irwls`, `eps_irwls` |

`init_weights` selects `mcd` or `univariate`; `weight_function` selects
`tukey` or `huber`. `alpha` defaults to the selected weight function's tuning
constant. Parameters apply only to the engines listed above; for example,
`weight_function` does not change CRM or shooting-S detection.

The existing regression helpers now return their fitted model or coefficients,
so standalone imputation and mlr3 reuse the same regression implementation.
The standalone functions' outer loops and return contracts remain unchanged.
The new learner fits only its mlr3 training rows; held-out targets and prediction
batches never enter estimation, cell-weight initialization, or refitting.
Factor encoding and contrasts are learned at training time and reused for new
rows. Constant predictors are dropped; a predictor-free or constant-response
fit returns the training response median. Missing predictors are completed by
vimpute, not by the learner. Direct mlr3 callers must provide complete predictors;
unseen factor levels are rejected.

This is an inductive per-target implementation, not numerical equivalence with
an entire standalone imputer. In particular, the old imputeCellReg engines fit
all rows of the initialized dataset, including provisional response values.
The new learner intentionally excludes those provisional targets. Independent
fits do not exchange the standalone routines' shared cell weights or damping
state. Prediction now replaces gross marginal numeric anomalies with training
medians (`clean_predictors = TRUE`, `predictor_cutoff = 4.5`). This changes
only the learner's internal prediction frame, not supplied observed cells.
Numeric donor selection screens residuals against training calibration
(`donor_cutoff = 4.5`; `Inf` disables). See `cellwise-robustness-investigation.md`
for the paired ablation, scope and limitations. Dummy-coded factor columns
receive initial cell weight one rather than entering continuous MCD estimation.
Shooting-S preserves the repository's approximation; it is not a claim to
implement the full published coordinate-wise shooting S algorithm.

Categorical targets use `classif.cellwise`, a weighted `nnet::multinom()`
classifier. Continuous predictor cell weights are estimated on training rows
only, using `init_weights = "mcd"` or `"univariate"`, `weight_function` and
`alpha`. Their geometric mean is the row weight (each cell weight is floored
at 1e-10 before taking logs, matching the standalone code). Categorical-only
predictors receive row weight one. Additional parameters are `maxit = 200`,
`decay = 0` and `MaxNWts = 10000`. Prediction supplies named class probabilities;
vimpute chooses classes or samples according to its uncertainty setting.
Missing training classes receive probability zero; a single-class training fold
returns that class, and featureless fits return weighted class proportions.
Nonconverged multinomial fits warn; fitting errors propagate to the caller's
existing fallback mechanism. No categorical EM or categorical outlier detection
is included. Use per-target parameters for regression-only settings such as
`engine`; method-wide weighting settings work for both learner types.

Plain predictor selection works through vimpute's `predictors` or grammar.
Transformed model formulas are not enabled for this method. Automatic default
tuning has no search space. Tune the learner directly with mlr3tuning, or
register a custom method with an engine-specific `search_space` hook. Deterministic predictions feed
vimpute's existing iteration, bootstrap, PMM, residual/normal uncertainty,
multiple-imputation and model-quality machinery. This integration alone does
not establish statistical properness of multiple imputation.

## Review of every imputeCell* entry point

| Entry point / source | Reusable part | Integration decision |
| --- | --- | --- |
| `imputeCellReg` / `R/imputeCellReg.R` | CRM, cell-weighted MM, shooting-S approximation | Implemented as three engines of one learner; separate vimpute method names would duplicate routing. |
| `imputeCellM` / `R/imputeCellwise.R` | Single-response `cellIRWLS`; weighted multinomial for categorical responses | Numeric fitting implemented as `engine = "irwls"`. Formula parsing, initialization, and uncertainty belong outside the learner. `classif.cellwise` now provides weighted multinomial fitting with training-only weights. |
| `imputeCellIRMI` / `R/imputeCellwise.R` | Same IRWLS fit with predictor-cell weights, iterative residual weights and damping across sweeps | Reuse the regression kernel as `irwls`. Do not nest its outer IRMI loop inside vimpute. Shared cross-variable weight updates would need a vimpute sweep-state extension. |
| `imputeCellMM` / `R/imputeCellMM.R` | MM fitting, MCD/univariate initialization, geometric row weights | Covered at the regression level by `cellwise-mm` and its initialization parameters. Its damped multivariate weight updates and final unweighted uncertainty refit are not reproduced. |
| `imputeCellMCD` / `R/imputeCellEM.R` | MCD/weighted/pairwise scatter initialization and updates, followed by per-target `cellIRWLS` regressions | The regression core is covered by `irwls` with MCD initialization. Full parity requires shared scatter/weights. `hard_threshold` can turn observed outliers into missing values: expose this as an explicit preprocessing option, never silently in a learner. The current `boot` option resamples scatter without effectively bootstrapping regression coefficients; retain vimpute's actual model bootstrap instead. |
| `imputeCellEM` / `R/imputeCellEM.R` | Latent-contamination EM for joint location, scatter, clean-cell probabilities, conditional predictions | Candidate for a distinct future joint-distribution method (`cellwise_em`), not an engine alias. Extract fit/predict with frozen training parameters; apply weighted/trusted conditioning to new rows without using their targets. Preserve conditional variance separately from prediction means so uncertainty is not added twice. |
| `imputeCellGLoc` / `R/imputeCellGLoc.R` | Joint categorical mean structure, scatter, soft/binary cell weights and categorical handling | Candidate for a distinct future method (`cellwise_gloc`) or a joint-model adapter. Separate estimation of B/Sigma from conditional prediction; retain categorical-level integration and trusted-peer rules. `weights = "soft"/"binary"`, `design`, and categorical handling are natural parameters of that adapter, not this regression learner. |
| `imputeCellwise` / `R/imputeCellwise.R` | Dispatcher to cellIRMI, cellM or cellEM | No additional statistical method; do not wrap the dispatcher as a learner. |

## Follow-up priority

1. Benchmark and assess calibration of the implemented weighted multinomial
   classifier, particularly with rare classes and contaminated predictors.
2. Benchmark the four current engines on contaminated training and prediction
   cells, including rank-deficient designs and mixed data. Assess the new prediction-time guard on legitimate heavy tails and
   conditional anomalies; marginal screening is not full cellwise detection.
3. Extract a frozen joint fit/predict interface for EM and GLoc before adding
   either as a vimpute method. Do not call the full imputer on a combined
   training/test dataset, which leaks held-out observations into estimation.
4. Consider shared cross-variable cell-weight state only as a deliberate
   vimpute extension with explicit missingness masks and bootstrap semantics.
