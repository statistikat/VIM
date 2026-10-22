# Proposal: improve TabICLv2 imputation accuracy in VIM

Review date: 2026-10-16. Code reviewed: combined branch
edit-restriction_tabicl2-impute, commit d23e719; installed Python TabICL 2.2.
This document records the original proposal. Items 1 and 2 were subsequently
implemented; see tabicl2-typed-preprocessing.md for the implementation,
verification and restarted experiments. The remaining sections are proposals.

## Evidence and interpretation

The paper reviewed is [Qu et al., TabICLv2, arXiv version 2](https://arxiv.org/html/2602.11139v2).
Its Section 8 says missing predictors use mean imputation; missingness-aware
pretraining, indicators and fine-tuning are not evaluated there. Appendix L.1
finds diminishing ensemble gains beyond eight members. Appendix I describes
quantile-based predictive distributions, with evaluation limited to synthetic
examples. Appendix L.3 reports an advantage for fold ensembling in its tiny,
16-training-sample setting. These results motivate experiments, not guarantees
for VIM or for our 128-observed-target example.

The [official implementation](https://github.com/soda-inria/tabicl#preprocessing)
and installed 2.2 source were checked separately. Its input transformer treats
numeric arrays differently from typed pandas dataframes. Fine-tuning uses
AdamW and held-out validation. The paper's Muon pretraining learning rates
should not be copied into this downstream optimizer.

Two implementation findings matter more than a larger epoch budget:

* R/helper_vimpute.R, predict_imputations(), unconditionally fills remaining
  prediction-backend missings with R median/mode values. The TabICL training
  path passes NaNs to Python. A temporary API-compatible probe confirmed that
  the first target's training feature matrix contained missing values while
  its actual missing-row prediction matrix did not. This is a different issue
  from the already-fixed indicator-only pipeline.
* R/tabiclImpute.R, .tabicl_matrix(), maps every factor to numeric codes and
  returns a numeric array. Python therefore cannot recognize the columns as
  categorical. Missing factor codes can enter the numeric mean-imputation
  path. Merely accepting NaN does not mean the pretrained model has learned a
  missingness mechanism.

The Y-only simulation currently has RMSE 0.554 pretrained, 0.605 fine-tuned,
and 0.551 fine-tuned sequential, on 32 withheld cells. An additional diagnostic
fit of Y ~ X + A*B using only observed Y has RMSE 0.492. The known conditional
mean has realized error 0.465 on this mask. The data-generating residual SD is
0.5. These are single-sample diagnostics, not configuration-selection scores
or universal bounds: the opportunity for gains on this easy example is modest.

## 1. First implementation: consistent, typed input preprocessing

Priority: correctness and comparability; highest confidence in the need to fix.

For method tabicl2, fill only the target placeholder required to construct an
mlr3 prediction task. Preserve missing predictor values through to the fitted
Python transformer. Do not change other learners' preprocessing. Apply the
same policy in ordinary and semicontinuous regression paths and donor scoring.

Introduce a typed-dataframe backend for TabICL: numeric columns remain numeric;
factors become categorical columns with a stored training schema. Reuse the
fitted encoding at prediction time. Unknown levels need a documented missing/
unknown representation, not numeric interpolation. Retain a compatibility mode
for the existing numeric-array backend until both pretrained and fine-tuned
regression/classification paths have been compared.

Tests must verify train/predict predictor values and missingness, immutable
observed cells, unknown and unused levels, ordered factors, entirely missing
predictor columns, single-row prediction batches, and mixed-method imputation.
Check native predictions against direct Python on identical inputs. A spy on
only the first predict() call is insufficient: model-quality scoring can call
predict() on the training rows before actual missing-row prediction.

Acceptance: equivalent observed/missing inputs receive one fitted preprocessing
policy in training and prediction; category missings never become fractional
category values. Whether typed preprocessing lowers RMSE is then measured.

## 2. Add optional missingness features without losing predictor values

Priority: experimental accuracy improvement when predictors are incomplete.

Append predictor indicators to the original features. Never chain an
indicator-only transform as a replacement for the feature matrix. Build the
indicators from original imputation masks, including makeNA, and keep them
fixed across sweeps even after predictor values are imputed. Exclude the
current target's own missingness indicator. Store names and order in the model
so training, prediction and donor scoring agree.

Compare indicators off/on under MCAR and MAR, with a stress-test MNAR scenario.
Default to off until validation supports changing it. On the current Y-only
experiment all predictor indicators are constant, so no accuracy gain should
be expected from this feature. This is not a solution to MNAR identification.

## 3. Select adaptation using observed-data validation

Priority: practical point-accuracy improvement; avoid assuming fine-tuning wins.

Add a TabICL search-space hook to the existing vimpute tuning infrastructure.
The current registry has no TabICL-specific search space. Keep epochs = 5 as
the learner default and allow explicit larger budgets.

Start with a fixed pretrained baseline and fine-tuned candidates at learning
rates 1e-5, 3e-5 and 1e-4, maximum 20 epochs, with early stopping. Use a common
validation ensemble size across candidates. Expose validation_split_ratio,
eval_metric and early_stopping as validated controls rather than relying only
on unstructured Python argument lists. Log selected epochs, baseline/best
validation scores, fitted-row counts, configuration and seeds.

Select configurations by repeated validation or small-fold CV among observed
target rows. Early stopping must happen inside each training fold; model
selection uses its held-out validation fold. The synthetic 20% hidden truth
is reserved for the final evaluation. Preprocessing, predictor imputation and
feature selection must be fitted within folds as well. Do not allow another
imputation model to leak a validation target through an imputed covariate.

Compare pretrained and fine-tuned predictions on the same validation rows and
retain pretrained unless the improvement is sufficiently stable. The upstream
best-weight safeguard is useful but does not guarantee better imputation on
new missing rows. Optionally test a convex prediction blend with its weight
chosen by validation. Preserve finetune = TRUE as an explicit existing mode;
any automatic selection must be a new opt-in behavior with visible diagnostics.

For the small synthetic example, include the observed-only interaction linear
model as a benchmark reference. Do not silently relabel predictions from a
different selected learner as TabICL predictions.

## 4. Test inference ensembles and preprocessing before enlarging them

Priority: controlled experiments with moderate cost.

Keep n_estimators = 8 by default. Compare 8/16/32 as an explicit accuracy-budget
experiment. Expose norm_methods, feat_shuffle_method and outlier_threshold in
the R parameter set so they can be validated and tuned. They can already be
forwarded through estimator_params/finetune_params, so this is an API and
tuning improvement rather than a new Python algorithm.

Test default normalization against selected robust/quantile alternatives on
skewed and outlier-heavy data. Retain upstream feature/class permutation
ensembles rather than reimplementing them in R. Test sensitivity to arbitrary
factor level order; do not assume invariance from ordinal encoding.

For very small observed-target samples, test a separate fold/context ensemble
against a single full-context fit. Fewer context rows can also hurt; this is
an optional candidate, not a proposed unconditional default.

## 5. Separate point accuracy from stochastic imputation quality

Priority: important for distributions and multiple imputation; later than input fixes.

The current R regressor always requests predict()'s default point output.
Expose point_estimate = mean or median. Keep mean for squared-error objectives;
compare median for absolute-error objectives. Do not assume median improves
RMSE. The existing pretrained wrapper already delegates its point prediction
to the Python estimator; there is no need to replace its internal quantile head.

Add a separate uncertainty mode based on corrected predictive quantiles. Use
upstream quantile/distribution handling, including monotonicity and tail
handling, and generate seeded row-wise draws. Integrate this at the uncertainty
stage, before generic edit projection. Avoid constructing an n-by-n quantile
matrix merely to obtain one random quantile per row.

Evaluate interval coverage, CRPS, subgroup variance, marginal distributions
and correlations alongside point RMSE. Predictive draws need not improve
point RMSE. They also do not automatically account for parameter uncertainty
or establish proper multiple imputation. Test combinations with bootstrap or
model ensembles and keep the existing vimmi interface.

Any future edit weights based on predictive variance require record-specific
weight support; current named static weights cannot represent this generally.
Evaluate raw and projected values separately because projection changes the
sampling distribution. This should be a separate extension, not part of the
first preprocessing patch.

## 6. Make sequential runs economical and validation driven

Priority: avoid spending the budget on redundant refits.

For a single missing target with fully observed predictors, there is no new
covariate information across sweeps. In an optional automatic mode, use one
pass and spend any additional budget on independently validated models.
Respect explicit sequential/nseq choices. With multiple incomplete targets,
compare 1/3/5/10 sweeps and visit orders using artificial validation masks.
Numerical convergence is not evidence that prediction error improved.

Use deterministic per-target/model seeds and record the actual number of fits.
Cache only when the training predictors, observed target values, encoding and
hyperparameters are identical. Never reuse a stale fitted model after a
predictor update. Caching improves the experiment budget, not accuracy by itself.

## Evaluation plan and order

1. Input-schema and missingness regression tests, then a direct-Python parity
   check on a small mixed dataset. No long benchmark is needed to establish
   these correctness properties.
2. Paired, predeclared small experiments: Y-only MCAR, jointly incomplete
   predictors, and an MAR mask. Use a small fixed set of seeds, a linear/group
   interaction example, a nonlinear example, and a heteroscedastic example.
   Do not change seeds until a preferred method wins.
3. Ablate one feature at a time: consistent preprocessing, typed categories,
   appended indicators, validated fine-tuning, and larger inference ensembles.
   Track RMSE/MAE, classification accuracy/log-loss, runtime and uncertainty in
   paired differences. Treat repeated masks of the same dataset as dependent.
4. Only after useful gains survive this stage, rerun the corrected UCI benchmark
   with a fixed compute budget and a new protocol/cache version. Its currently
   superseded results cannot validate the proposals.

Recommended first patch: consistent typed preprocessing plus parity tests.
Recommended second patch: observed-data validation and adaptation selection.
Missingness indicators are a separate ablation; quantile-based stochastic
imputation follows when distributional quality is the objective. None of these
changes promises a particular accuracy gain before the paired evaluation.
