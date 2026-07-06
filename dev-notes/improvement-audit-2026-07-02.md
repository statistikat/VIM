# VIM improvement audit — 2026-07-02

**Scope:** strategic audit of VIM 7.1.0 (`devel` @ `1d12d04`) with the explicit aim of modernising
the imputation side around `vimpute()`/`vimmi` so that it beats mice on **methods**, **flexibility**
(per-variable specification + hyperparameter tuning), and **usability**.

**How this was produced:** `suite-rpkg-improve` executed as a 44-agent workflow — 17 finder agents
(R CMD check, five deep code reviews, test/API/dependency audits, mice + competitor + literature
comparisons, live usability test, statistician docs review, adversarial framework-design review),
followed by adversarial verifiers that had to *reproduce or refute* every P0/P1/bug claim in live R
against an installed build of the working tree, plus codebase fact-checking of every claimed
feature gap. 94 claims were CONFIRMED by reproduction, 2 were REFUTED (noted below), the rest are
lower-priority items below the verification threshold. Full details for all 176 findings:
`dev-notes/improvement-audit-2026-07-02-appendix.md`.

**Caveats:** the R CMD check ran without vignette rebuild and without the PDF manual (vignette
health was assessed separately by the docs review). Two finder agents (classic-integration, mice)
ran while the safety-review model was unavailable; their headline claims were independently
re-verified (method whitelist at `helper_vimpute.R:997`, the `complete()` generic export, the
`imputeRobustChain` defect was found twice independently).

---

## Executive summary

`R CMD check`: **0 ERRORs / 0 WARNINGs / 1 benign NOTE** — the package *looks* healthy. The audit
shows that this masks substantial correctness debt concentrated in exactly the strategic surface:

1. **MI properness is broken in two default-adjacent configurations (P0).** `pmm = TRUE` with its
   defaults (`pmm_k = 1`, `pmm_k_method = "mean"`) is fully deterministic, so `m > 1` returns m
   *identical* imputations with zero between-imputation variance — and the "identical imputations"
   safeguard is explicitly bypassed because PMM counts as a variability source. Separately,
   `boot = TRUE, uncert = "none"` silently produces improper MI (measured: pooled SE 0.105 vs
   0.171 for mice `norm.boot` — a ~40 % SE underestimate). Nothing enforces that proper MI needs
   parameter draws *and* residual noise together. Rubin's rules are invalid in both cases, silently.
2. **The shared cellwise IRWLS engine has a design defect (P0).** `cellIRWLS` multiplies raw design
   values by cell weights (`X_tilde = sqrt(w_row) * w_cell * X`), destroying the regression signal:
   `imputeCellIRMI` imputes *worse than unconditional median* on clean correlated data (RMSE 3.54
   vs 3.46 median vs 0.96 lm) and 2× worse than plain `irmi` under actual cellwise contamination
   (3.22 vs 1.51). This currently defeats the purpose of 3 of the 6 cellwise methods (cellIRMI,
   cellM, cellMCD's imputation step). Downweighting must enter the *loss/estimator*, not the data.
3. **`imputeRobustChain()` is effectively broken (P0), `imputeRobust()` partially (P1).** The chain
   function imputes literal zeros in its default configuration and crashes on every other path
   (ghost functions like `robGAM`, wrong switch labels) — masked from R CMD check because the
   undefined symbols are declared in `zzz.R` `globalVariables()`. Found independently by two agents;
   zero tests exist for either function.
4. **The framework loses to mice today on its own three pillars** — not on methods (where VIM
   clearly leads) but on control and finish: no `predictorMatrix` equivalent for the flagship ML
   methods (formula is hard-rejected for ranger/xgboost), `tune = TRUE` is a *silent no-op* when
   `sequential = FALSE` (the trigger `i == round(nseq/2)` is never true when nseq = 1), VIM's own
   `complete()` generic is masked the moment the user loads mice — the exact package the docs tell
   them to load for pooling — and `vimmi` has no plot/diagnostic methods at all (ironic for *the*
   missing-data visualization package).
5. **Test and documentation debt on the new code.** 28 of 60 exports (47 %) are never called by any
   test; `evaluation()`'s default `vartypes = "guess"` silently returns error 0 for arbitrarily bad
   imputations (guessing was never implemented); `makeNA`/`donorcond` and `as.mids.vimmi` are
   untested; the vignette never actually runs the flagship MI-pooling or tuning paths (all
   `eval=FALSE`); two references in the new cellwise docs are materially wrong and five man pages
   cite a nonexistent journal.

**Strategic verdict.** The differentiating edge is real and verified: VIM is the *only* R package
with robust *and* cellwise-robust imputation, the only one with per-variable hyperparameter tuning
inside the imputation loop, per-variable granularity beyond every scanned rival, the deepest
missingness-visualization toolkit, and measured wall-clock parity with missRanger at n = 3000.
What blocks the "better than mice" claim is not methods — it is the MI analysis layer (pooling
surface, diagnostics), safe statistical defaults, predictor control for ML learners, and finish.
Most of these gaps map directly onto mlr3 primitives (`task$select()` for predictorMatrix,
`row_roles` for ignore, learner registry for new methods, future for parallel m), so the list is
cheaper than it looks. Positioning (from the novelty analysis): interoperate with mice's pooling,
don't chase its inference ecosystem — VIM is the choice **when data are contaminated, mixed, or
non-linear and methods need tuning**.

---

## Scorecard vs mice (the three pillars)

| Pillar | Today | What carries it | What blocks it |
|---|---|---|---|
| **Methods** | ✅ ahead | Robust family (irmi, robust/robgam learners), cellwise family (unique on CRAN), xgboost/GAM learners, kNN/hotdeck with Gower for mixed data, richer PMM | cellIRWLS P0 defect neutralises 3/6 cellwise methods; no ordinal (polr) / count targets in vimpute (irmi regression on deprecation path) |
| **Flexibility** | ⚠️ mixed | Per-variable method/params/pmm/tune (finer than mice), only package with in-loop tuning, makeNA/donorcond | No predictorMatrix equivalent for ranger/xgboost; tune silently no-ops (sequential=FALSE); 6-method whitelist wastes the mlr3 backend; wrappers ignore documented hyperparameters |
| **Usability** | ⚠️ mixed | One-liner works, print/summary.vimmi good, exemplary complete() errors, MI guardrail warning, verified mice-pooling bridge | complete() masked by mice/tidyr; zero MI diagnostics; ~45 raw warnings in the flagship example; silent column drops/coercions; marginplot fails on vimpute's own output |

---

## P0 — fix before anything else

### Correctness (all reproduced empirically)

- [x] **P0.1 Deterministic default PMM ⇒ degenerate MI.** `vimpute(..., pmm = TRUE, m > 1)` with
  defaults returns m identical imputations (verified zero between-variance on `sleep`); the m>1
  warning was bypassed because `has_any_pmm` counted as variability. **Done (warn-only, Wave 1):**
  the guard now recognises that PMM injects variability *only* with a random draw from ≥ 2 donors
  (`is_stochastic_pmm`); a deterministic PMM config with `m>1` now warns that imputations will be
  identical and Rubin's rules are invalid. Defaults/outputs unchanged per the warn-only policy.
  `R/vimpute.R`; regression test `inst/tinytest/test_vimpute_mi_properness.R`.
  *(vimmi-mi, vimpute-helpers, methodology-docs — three independent confirmations)*
- [x] **P0.2 Improper MI not prevented.** `boot=TRUE, uncert="none"` yields conditional-mean MI with
  ~40 % SE underestimate (measured against mice norm.boot). **Done (warn-only, Wave 1):** `m>1` with
  `boot=TRUE`, `uncert="none"` and no PMM now warns that between-imputation variance is
  underestimated and points to `uncert`/PMM. The combination is not silently "upgraded"; behaviour
  unchanged. `R/vimpute.R`; same regression test. Doc note in `?vimpute` still TODO.
  *(vimmi-mi)*
- [x] **P0.3 cellIRWLS weights-times-data defect.** **Done (Wave 1).** Two coupled fixes:
  (1) `.weighted_qr_solve` now solves genuine WLS on the *unweighted* design, with cell weights
  entering the loss as a per-row reliability (arithmetic mean of predictor cell weights), so β is a
  valid coefficient for the `cbind(1, X) %*% β` used at prediction (was `X_tilde = √w_row·w_cell·X`).
  (2) Default `init_weights` changed `"mcd" → "ddc"`: `mcd` downweights the high-leverage points that
  carry the regression signal, so it imputed *at or worse than median* on both clean and contaminated
  data; `ddc` (DetectDeviatingCells, the detect-then-impute method the audit recommended) is best on
  both. Benchmark (4 seeds, RMSE vs median≈2.2): clean 0.83, contaminated 0.75 — both now **beat plain
  irmi** (was ~2× worse). `R/cellwise_utils.R`, `R/imputeCellwise.R`; regression test
  `inst/tinytest/test_cellwise_irmi_quality.R` (clean + contaminated). Also fixed the stale
  `init_weights` roxygen (documented nonexistent `"marginal"`). **Follow-up (flag):** default changed
  away from the flagship `mcd` — the deeper `cellWeightsMCD` signal-over-flagging is a separate fix;
  `imputeCellMM` still defaults to `mcd` (untested for this, left as-is). *(cellwise, `cellwise_utils.R`)*
- [x] **P0.4 imputeRobustChain broken end-to-end.** **Repaired (Wave 1; user chose repair over
  deprecate).** Fixes in `R/imputeRobustChain.R`: (1) the `uncert="pmm"` donor loop assigned inside
  an inner function that was never called, so every donor stayed 0 and imputations collapsed to
  `pred + (0 - pred) = 0` — replaced with real PMM (match each missing case's prediction to the k
  nearest observed predictions, donate an observed value); (2) `useRobustNumeric` now takes `alpha`
  (was undefined in the `method="lm"` bootstrap → error); (3) the `method="gam"` path never assigned
  `mod` → `predict(mod)` failed — now assigns it; (4) `imp()` dispatched on `"bin"`/`"count"` but the
  loop passes the actual type names `"binary"`/`"integer"`/`"count"`, and called undefined helpers
  with out-of-scope args — dispatch realigned to the real type names; (5) implemented the missing
  `useLogistic` (binary) and `useGLMpoisson` (count) as robust `glmrob` GLMs with a `glm` fallback and
  outlier-aware bootstrap; (6) the nominal path's boot block fit `lm()` on a factor response (crash) —
  replaced with a `multinom` bootstrap refit, handling the single-missing-case shape; (7) the coda
  outlier step called `pivotCoord()` (robCompositions, not a dependency) — now `coda = FALSE`, guarded,
  degrades to no outlier flags. Regression test `inst/tinytest/test_imputeRobustChain.R` (numeric×3
  methods recover the signal / never impute zeros; binary → valid levels; count → non-negative
  integers): 10/10. **Follow-up (flag):** `zzz.R` `globalVariables()` left as-is (removing entries
  risks new NOTEs from the remaining dead `pivotCoord` branch; the masking is now moot since the bugs
  are fixed); the `familiy=` typo arg and placeholder roxygen (`PARAM_DESCRIPTION`) are P2 doc items;
  `useGLMpoisson` is only reached for explicit `class "count"` columns (plain integers take the numeric
  path), so it is implemented but not directly covered by the test. *(classic-integration + deps-arch)*

### Strategic gaps (the "beats mice" blockers, all fact-checked)

- [ ] **P0.5 MI diagnostics for vimmi.** `plot.vimmi()`: imputed-vs-observed density/strip overlays,
  FCS convergence traces (chain stats already computed in-loop, currently discarded), per-variable
  between-imputation views. This is the single most visible gap — mice, miceRanger, mixgb, Amelia
  all ship it, and VIM is the visualization package.
- [ ] **P0.6 Pooling surface.** Keep delegation to `mice::pool()` as the engine but make the path
  frictionless: fix the `complete()` masking (P1.1), return a proper `mira`-compatible object from
  `with.vimmi()`, and add `pool()` re-export or a thin `vim_pool()` so an MI analysis never *feels*
  like leaving VIM.
- [ ] **P0.7 predictorMatrix equivalent.** `predictors =` named list (or mice predictorMatrix
  accepted verbatim) implemented via `task$select()` — works uniformly for every learner including
  ranger/xgboost, where formula is currently hard-rejected. Plus `visit_sequence =`.
- [ ] **P0.8 Open the learner space.** Accept any mlr3 learner (mlr3extralearners: lightgbm, svm,
  nnet, …) per variable instead of the hard-coded 6-method whitelist (`helper_vimpute.R:997`) —
  the cheapest headline feature given the backend is already mlr3.

---

## P1 — correctness cluster (all CONFIRMED unless noted)

**vimpute core**
- [ ] `tune = TRUE` silent no-op when `sequential = FALSE` or early convergence (`i == round(nseq/2)`
  never fires); tune at a guaranteed iteration and warn if tuning was requested but never ran.
- [ ] Internal `<var>_zero_prob` working column leaks into the returned data *and* into predictors
  of subsequent variables (semicontinuous path).
- [ ] `considered_variables` silently drops all other columns from the output (data loss; return
  the full dataset with non-considered columns untouched).
- [ ] Length-1 named method list reinterpreted as global method — `list(Dream = "gam")` applies gam
  to *all* variables; typo'd names pass silently. Validate names against `names(data)`.
- [ ] Unnamed method lists aligned with all columns are positionally misassigned to NA-variables.
- [ ] Ordered factors: vimpute strips `ordered` from ALL considered columns (silent, also a methods
  regression vs mice `polr`); logical → numeric coercion equally silent.
- [ ] The commit-8cc3020 "identical imputations" warning is factually wrong for ranger (forest RNG
  differs) while the genuinely-identical case (deterministic PMM) escapes it — invert the logic:
  warn on *measured* zero between-variance, not on flag heuristics.
- [x] `complete(vimmi)` breaks once mice/tidyr is loaded (generic masking): **done (Wave 1).**
  `.onLoad` now registers `complete.vimmi` on the `mice` and `tidyr` generics (immediately if loaded,
  else via `setHook(packageEvent(..., "onLoad"))`), so `complete(vimmi)` dispatches regardless of load
  order. `R/zzz.R`; regression test `inst/tinytest/test_vimmi_complete_masking.R` (verified against
  both `mice::complete` and an attached `library(mice)`). (Unambiguous alias still a P2 doc nicety.)

**Cellwise family**
- [x] `imputeCellReg` treats crmReg's continuous deviations as binary flags → negative cell weights.
  **Done (Wave 1):** `.engine_crm` now binarises `crm_res$cellwiseoutliers` (`(x != 0) * 1`) so the
  downstream `1 - flag` stays in [0, 1] (was as low as ~-10). `R/imputeCellReg.R`; regression test
  `inst/tinytest/test_imputeCellReg_weights.R`.
- [ ] `imputeCellMCD(boot = TRUE)` fits the bootstrap model and discards it (no uncertainty
  propagation despite docs). **Deferred (Wave 1):** the correct fix (fit MCD/S params on the
  bootstrap sample, then impute the *original* rows with those params) needs a fit/predict split of
  `imputeCellMCD` — more than a P1 quick fix. `res_boot` is currently computed then thrown away and
  `res_orig` re-fits on the original data (`imputeCellEM.R:998-1013`); at minimum this should not
  waste the bootstrap fit. Left for a focused pass.
- [ ] Numerical guards: absolute 1e-10 ridge crashes on n<p/collinear data; `.robust_scale`
  fallback of 1 injects N(0,1) noise into constant columns.
- [ ] Register the (repaired) cellwise family in vimpute (detection pre-pass + learner wrapper).

**Classic methods**
- [ ] `imputeRobust()`: `method="gamRob"` and `uncert="wresid"` always crash; PMM donor pool is
  contaminated by kNN-initialised values.
- [ ] `irmi(mi > 1)` with default `imp_var=TRUE` returns one mangled data.frame instead of a list.
- [ ] `regressionImp()` silently fits glmnet (fixed lambda 0.01) instead of documented lm/glm
  whenever ≥ 2 predictors.
- [x] `rangerImpute`/`xgboostImpute` wrappers ignore their documented hyperparameters
  (`num.trees`, `nrounds`, … silently dropped since delegation to vimpute). **Done (Wave 1):** both
  now forward their hyperparameters (and `...`) to the ranger/xgboost learner via `vimpute()`'s
  `learner_params` instead of warning that they are ignored; `xgboostImpute` maps per-target
  `objective`. `R/rangerImpute.R`, `R/xgboostImpute.R`; regression test
  `inst/tinytest/test_wrapper_hyperparams.R` (verifies `num.trees`/`nrounds` actually change the
  imputations).

**Evaluation & distances**
- [x] `evaluation()` default `vartypes="guess"` returned error 0 for any input — **done (Wave 1):**
  `"guess"` now infers per-column types from `x` (numeric → `"numeric"`, else `"factor"`).
  `R/evaluation.R`; regression test `inst/tinytest/test_evaluation.R` (known-truth recovery,
  guess-vs-explicit agreement, perfect-imputation = 0).
- [ ] kNN/Gower: semi-continuous variables never range-scaled (unbounded contribution dominates
  distances); `weightDist=TRUE` produces Inf weights → NaN imputations with `methodStand="iqr"`;
  NA-sentinel convention makes kNN prefer donors sharing the recipient's missingness pattern
  (opposite of Gower 1971 omit-and-renormalize; undocumented).

**Infrastructure**
- [x] `tests/test_vimpute_new_features.R` hard-`stop()`s when Suggests missing → CRAN noSuggests
  ERROR. **Done (Wave 1):** `stop()` → `message()` + `quit(status = 0L)` so it skips gracefully under
  the noSuggests flavor. (Folding it into tinytest / de-duplication is the P3 cleanup, not done here.)
- [x] No minimum version pins on the mlr3 stack. **Done (Wave 1):** pinned `mlr3tuning (>= 1.0.0)`
  (for `TuningInstanceBatchSingleCrit`) and `paradox (>= 1.0.0)` (for the `ps()` search-space API) in
  DESCRIPTION.
- [x] `car` accounts for 55 of 105 recursive hard deps for one optional Box-Cox call — **done
  (Wave 1):** demoted `car` Imports → Suggests. `prepare()`'s Box-Cox now uses a native power
  transform (`.bcPower`, replaces `car::bcPower`); `car::powerTransform` is used only to *estimate*
  lambda, behind a `requireNamespace` guard with a clear message. Also fixed a latent bug in the
  matrix branch (it passed a non-matching `p=` arg to `mapply`, so multi-column Box-Cox never
  worked) by applying per-column lambdas. Recursive hard deps **105 → 58**. `R/prepare.R`,
  `DESCRIPTION`, `R/VIM-package.R`; regression test `inst/tinytest/test_prepare_boxcox.R`.
- [x] CI runs only on master/main — **done (Wave 1):** added `devel` to the push/PR branch filters in
  `.github/workflows/R-CMD-check.yaml`.

**mice-parity P1 gaps (fact-checked):** amputation generator for simulation studies
(`ampute()`-equivalent, natural extension of `makeNA`), parallel MI over m (future is already
imported; note tuned-params caching must land first so parallel m doesn't multiply tuning),
MNAR sensitivity (delta adjustment), ordinal imputation (polr or mlr3 ordinal learner),
post-processing hooks/bounds (`post`/`squeeze` equivalent; competitors: Amelia bounds).
**Competitor-parity P1 gaps:** per-variable quality feedback by default (missForest OOB-style),
overimputation calibration diagnostic (Amelia `overimpute`), model persistence + `predict()` on
new data (missRanger/mixgb/miceRanger all have it), reproducible parallel seeding.

---

## P2 — framework contracts (the redesign package)

The framework-design review confirmed the architecture is right (named-list per-variable spec beats
mice's method vector; compact vimmi + as.mids bridge is the correct posture) and proposed concrete
contracts — sketches in the appendix, headlines here:

- [ ] **Unify per-variable spec** behind spec objects: today 9 parallel per-variable arguments with
  three-way-ambiguous `learner_params` semantics. Proposal: `vs_ranger(num.trees=500, tune=TRUE,
  uncert="pmm")`-style constructors validated eagerly against the learner's param_set, with the
  current flat arguments kept as a compatibility layer.
- [ ] **Formula grammar as sugar** (the IDEA item worth doing): `vimpute(dat, Sleep ~ Dream + Span |
  ranger(tune=TRUE), NonD ~ . | robust(donorcond=">= 0"), .default = ranger(), m = 20, seed = 1)` —
  compiles to specs; RHS lowers to `task$select()` so ML methods get predictor control for free.
- [ ] **Tuning architecture:** tune once on observed cells *before* the m-loop and reuse tuned
  params across imputations (currently re-tunes m times and each imputation may pick different
  hyperparameters — conflating tuner noise with missing-data uncertainty, statistically wrong for
  Rubin); fixed `batch_size = 1` (today's `detectCores()-1` makes results machine-dependent); stop
  clobbering the user's `future::plan` (unconditional `plan("sequential")` after first tuned var);
  expose `vimpute_tune_control()` + surface a tuning report.
- [ ] **`register_vimpute_method()` plugin registry:** adding a method today takes ~12 coordinated
  edits across two files (three copy-pasted modifyList dispatch chains included). A package-env
  registry collapses them and lets third parties extend VIM without patching it.
- [ ] **Harden the vimmi bridge:** keep vimmi (don't return mids natively — would hard-couple to
  mice and drop metadata); make `as.mids.vimmi` a real generic method or rename `vim_as_mids()`;
  wrap `with.vimmi` returns as mira-compatible; store per-iteration chain stats + seed in vimmi.
- [ ] **Safe defaults:** default `uncert = "pmm"` (random-draw, k=5) for numeric single imputation
  — today's conditional-mean default loses any density-overlay comparison against mice; add a
  per-variable tryCatch fallback chain so one pathological column can't abort the whole run
  (mice's behavior is the bar).
- [ ] **Convergence criterion:** per-variable scale-normalized `d_v = MSE-change/var(obs)` with max
  (not sum) aggregation — today's unscaled sum means eps=0.005 is meaningless across data scales;
  return the convergence matrix for diagnostics.
- [ ] **Type-stable returns:** class `c("vimpute","data.table")` with diagnostics as attributes
  (`tune=TRUE` currently changes the return type to a bare list, breaking downstream code);
  restore logical/ordered types on exit; message once per coerced column.

**Plus P2 method/infra gaps (fact-checked):** `where`-matrix equivalent (arbitrary targets incl.
overimputation), passive imputation of derived variables, `quickpred`-style predictor pre-selection,
`ignore` vector (train/test separation via mlr3 row_roles), structured `loggedEvents`-style log,
grouped/by-domain imputation (simputation's `y ~ x | group`), benchmark + evaluation infrastructure
to *substantiate* the beats-mice claim, multilevel 2l.* (decide: support via lme4 learner or
explicitly position out-of-scope), dispatcher repair (imputeCellwise reaches 2 of 6 methods, cellM
is a stop() stub), API harmonisation (uncert vocabulary ×5, `method` meaning ×5, formula naming ×4,
MI container unification vimmi vs bare lists) — full list in the appendix.

**P2 test gaps:** MI end-to-end (m>1 → vimmi → as.mids → pool, seed-reproducibility), per-variable
method *mixing* (never tested; gam/robgam zero coverage anywhere), 4 of 7 cellwise exports untested,
imputeRobust(Chain) zero tests, degenerate-input matrix (all-NA column, zero variance, n<p, unseen
factor levels). The tests agent ships 10 concrete ready-to-write test specs (appendix).

---

## P3–P5 (selected; full lists in appendix)

- **P3:** test triplication cleanup (three vimpute test files in two frameworks; 26s duplicated
  check time; German messages + verbose=TRUE in shipped tests), test_vimmi.R 25s single call,
  306-line commented-out test file shipping to CRAN, `future::plan` clobber, glmnet candidate-CV
  re-run every iteration, zero-count factor levels trigger spurious method downgrade, noisy
  dropped-predictor warnings (once per var × iteration × m), visitSequence, CART single-tree
  method, MCAR test + influx/outflux, pooled prediction, PipeOp wrapper for leakage-free ML
  pipelines (NADIA lesson), `mids2spss`-style exporters.
- **P4:** cellwise `\donttest` examples exceed CRAN 5s limit; repo hygiene (`..Rcheck/` not
  gitignored, dead .Rbuildignore entry); duplicated learner chapters → own files; terminology
  drift (method/model/learner).
- **P5:** verified positive baseline worth *preserving* (first-run success, vimmi UX, MI guardrail
  warning, complete() error messages — regression-protect these).

**Documentation fixes (P2/P3, from the statistician pass):** wrong references in cellwise docs
(cellMCD cited as JASA 119(545), actually 119(548); Zaccaria et al. wrong co-authors/title);
"Journal of Computational Statistics and Data Analysis" (nonexistent) in 5 man pages; no
MAR/MCAR/MNAR statement anywhere in the package; vignette sells regularization as "robustness";
`?vimpute` contains invalid R in an example; vignette never demonstrates pooling/tuning (all
eval=FALSE); document the actually-firing imputeCellEM stopping rule (rolling-average, 10× looser
than documented eps_em).

---

## Verified wins — the paper's contribution list

The mice/competitor agents' *verified* wins double as the R Journal paper's selling points:

1. Only robust imputation family on CRAN (irmi, robust/robgam learners, robust bootstrap MI).
2. Only cellwise-robust imputation family in the R landscape (post-P0.3-fix).
3. Only package with per-variable hyperparameter tuning inside the imputation loop (post-P1 fix),
   incl. tuned-vs-default guardrail and data-size-adaptive search spaces.
4. Per-variable granularity beyond every rival (method, pmm, pmm_k, learner_params, tune, formula).
5. Broadest MI uncertainty menu (normalerror/resid/pmm/midastouch × boot/stratified/residual).
6. kNN/hotdeck with Gower distance, donorcond/makeNA — donor-based classics nobody else matches.
7. Richest missingness visualization suite (~20 functions) — once plot.vimmi exists, uniquely
   closing the loop from exploration → imputation → MI diagnostics in one package.
8. Memory-efficient vimmi container with verified loss-free mice pooling bridge.
9. Built-in imputation-quality metrics (evaluation/nrmse/pfc/msecor) — post-P1 evaluation() fix.
10. Measured wall-clock parity with missRanger at n=3000 (79.6s vs 82.8s, this audit).

REFUTED gap claims (VIM already has it): low-rank/factorial imputation (`impPCA` exists — though it
should be surfaced/modernised), count-data imputation (irmi has Poisson — the gap is only in
vimpute, see P1 irmi-regression item).

---

## Literature-scan candidates

Completed 2026-07-06 via a direct Consensus query (four searches; the original workflow agent had
failed on schema then on the spend cap). The papers below are **real, retrieved results with live
URLs** — but per the no-speculation-in-references rule they are *candidates to ingest into Zotero*,
not yet citeable: add each to the library (and confirm volume/page/DOI) before it enters the
paper's `.bib`. Organised by the gap each addresses. Reference numbers map to the list at the end.

### 1. Bootstrap-MI ordering — the authority for P0.2

Bartlett & Hughes [1] is the load-bearing reference: they show **impute-then-bootstrap is generally
invalid** under uncongeniality/misspecification while a specific **bootstrap-then-impute** variant
is valid. VIM's `vimmi` bootstrap ordering must be checked against this, and the P0.2 "proper MI
needs parameter draws *and* residual noise" guard should cite it. (The vimmi-mi agent independently
*measured* a ~40 % SE underestimate in the improper configuration.)

### 2. Cellwise family — related-work is essentially ready, and it dictates the P0.3 fix shape

The methods VIM already wraps are all here: **cellMCD** (Raymaekers & Rousseeuw, JASA 2022 [2]) is
what `imputeCellMCD` calls; **Štefelová et al. 2021** (ADAC [3]) is the crmReg cellwise-regression
that `imputeCellReg` wraps; the **"Challenges of cellwise outliers"** review (Raymaekers & Rousseeuw
[4]) frames the area. Crucially for P0.3: the field's dominant pattern is **detect-then-impute**
(cellFlagger / DDC detection-imputation [5]; conformal detect-then-impute [6]), which is exactly the
structure a corrected VIM cellwise pipeline should adopt. **cellRCov** [7] (joint cellwise + casewise
+ missing) is the frontier to benchmark the repaired engine against.

### 3. Substantive-model-compatible FCS / congeniality — P2 gap

smcfcs (Bartlett et al. [8], [9]) is the reference for the congeniality gap: vimpute's per-variable
models are FCS, so with a non-linear/interaction substantive model vimpute is currently uncongenial
(same limitation as plain mice). At minimum document this; ideally offer an smcfcs-style path. The
`smcfcs` R package is the interop target.

### 4. MNAR sensitivity — P1 gap

NAR-FCS delta-adjustment, and its substantive-model-compatible extension NAR-SMCFCS [10], are the
references for the MNAR-sensitivity P1 parity gap (delta adjustment layered on `makeNA`/donorcond).

### 5. Deep-learning imputation — evidence to **justify NOT chasing it**

This turns a potential "gap" into a defensible non-goal. Sun et al. [11] (careful comparative study)
find **conventional MICE/missForest beat GAIN and VAE for tabular data at n < 30,000**, with GAIN
only competitive under MCAR — precisely the mixed/tabular regime VIM targets. So VIM should *not*
rush deep imputers into `vimpute`; cite [11] in the novelty/"don't chase" argument. GAIN [12] and
tabular **diffusion** [13] are the references if a research bet is ever made (that's the separate
`[[deep_imputation]]` track, not a VIM feature).

### 6. Evaluation metrics — the richest, most actionable thread (P1 `evaluation.R`)

Directly damning for the current code: **Thurow et al. 2021 (GoodImpact)** [17] — on **German
official-statistics (DESTATIS) data** — shows that **NRMSE and PFC, the exact two metrics VIM's
`evaluation()` computes**, fail to capture distributional accuracy (low NRMSE/PFC does *not* imply a
good distributional match). The modern replacement is distributional/proper-scoring: **I-Scores**
(Näf et al., AoAS 2021 [14]; CRAN `Iscores`) and **proper scoring rules for imputation** [15], which
*don't* reward conditional-mean imputation the way RMSE does — this also explains *why* the P0.1
deterministic-PMM bug is dangerous: RMSE would rank the degenerate imputer highly. Add sliced-
**Wasserstein** discrepancy [18], energy-distance evaluation, the "how to rank imputations" score
[16], and follow the Oberman & Vink standardized-evaluation protocol [19] for the benchmark vignette
(Wave 4). This thread is what lets the paper *measure* "better than mice" credibly.

### References (Consensus-retrieved; ingest into Zotero before citing)

1. [Bootstrap inference for multiple imputation under uncongeniality and misspecification](https://consensus.app/papers/details/2d46dd63ece55f0a9226af0c9ad5ba0d/?utm_source=claude_code) — Bartlett & Hughes, 2019, Statistical Methods in Medical Research.
2. [The Cellwise Minimum Covariance Determinant Estimator](https://consensus.app/papers/details/4ccf365219a5597a8f6b783fb1d37659/?utm_source=claude_code) — Raymaekers & Rousseeuw, 2022, JASA.
3. [Robust regression with compositional covariates including cellwise outliers](https://consensus.app/papers/details/0d4428ee19815e6398c3b2f29b04cc39/?utm_source=claude_code) — Štefelová et al., 2021, Advances in Data Analysis and Classification.
4. [Challenges of cellwise outliers](https://consensus.app/papers/details/c4155b3aff1e58b9aee0db80df32c8d3/?utm_source=claude_code) — Raymaekers & Rousseeuw, 2023, Econometrics and Statistics.
5. [Handling Cellwise Outliers by Sparse Regression and Robust Covariance](https://consensus.app/papers/details/ef498eb4fbfe59dab29c0d3e6112b78a/?utm_source=claude_code) — Raymaekers & Rousseeuw, 2019 (cellFlagger / DDC detection-imputation).
6. [Conformal Prediction with Cellwise Outliers: A Detect-then-Impute Approach](https://consensus.app/papers/details/a21c6c97fb35599db56e0185ddc35bd8/?utm_source=claude_code) — Peng et al., 2025.
7. [Cellwise and Casewise Robust Covariance in High Dimensions](https://consensus.app/papers/details/db67c2f93da45010ab68d6fd6f4bb665/?utm_source=claude_code) — Centofanti et al., 2025 (cellRCov).
8. [Multiple Imputation of Covariates by Substantive-model Compatible FCS](https://consensus.app/papers/details/93677b512cbc5995abc9799d7c79c1f6/?utm_source=claude_code) — Bartlett et al., 2015, The Stata Journal (smcfcs).
9. [Multiple imputation of covariates by FCS: Accommodating the substantive model](https://consensus.app/papers/details/ed44f10e1e8f5ad4935fc8cb26596a11/?utm_source=claude_code) — Bartlett et al., 2012, Statistical Methods in Medical Research.
10. [Sensitivity analysis methods for outcome missingness using SMC multiple imputation](https://consensus.app/papers/details/5e34b7ce44ff59e783a12da0450ca889/?utm_source=claude_code) — Zhang et al., 2024 (NAR-SMCFCS / delta-adjustment).
11. [Deep learning versus conventional methods for missing data imputation: A review and comparative study](https://consensus.app/papers/details/ca8739a844ea54ccb9c5134d1df54855/?utm_source=claude_code) — Sun et al., 2023, Expert Systems with Applications.
12. [GAIN: Missing Data Imputation using Generative Adversarial Nets](https://consensus.app/papers/details/f7ffdef667715745b91671fd6a371aa3/?utm_source=claude_code) — Yoon et al., 2018.
13. [Diffusion Models for Tabular Data Imputation and Synthetic Data Generation](https://consensus.app/papers/details/d0e18120306d5bb396e7232ed788bce8/?utm_source=claude_code) — Villaizán-Vallelado et al., 2024, ACM TKDD.
14. [Imputation scores](https://consensus.app/papers/details/3100a9134ac65ea6b08bd305763e07eb/?utm_source=claude_code) — Näf et al., 2021, Annals of Applied Statistics (CRAN `Iscores`).
15. [Proper Scoring Rules for Missing Value Imputation](https://consensus.app/papers/details/c82d4b4fd4a251c5be2913eb15d0a8d9/?utm_source=claude_code) — Michel et al., 2021.
16. [How to rank imputation methods?](https://consensus.app/papers/details/10751464efb05acca2b640661752476d/?utm_source=claude_code) — Näf et al., 2025.
17. [Goodness (of fit) of Imputation Accuracy: The GoodImpact Analysis](https://consensus.app/papers/details/fdb5d1db541453d083735b238688fa01/?utm_source=claude_code) — Thurow et al., 2021 (DESTATIS; critiques NRMSE/PFC).
18. [The impact of imputation quality on machine learning classifiers for datasets with missing values](https://consensus.app/papers/details/1db812c377ab5d8ebfdf36d5456617ae/?utm_source=claude_code) — Shadbahr et al., 2022, Communications Medicine (sliced-Wasserstein discrepancy).
19. [Toward a standardized evaluation of imputation methodology](https://consensus.app/papers/details/7574bec1f1c7552aa9967b7642b457d1/?utm_source=claude_code) — Oberman & Vink, 2023, Biometrical Journal.

---

## Suggested sequencing on `devel`

1. **Wave 1 — correctness (small, high-leverage diffs).** P0.1–P0.4 + the P1 correctness cluster,
   each with a regression test written first. Nothing else is credible until MI is proper and the
   cellwise engine beats median imputation.
2. **Wave 2 — framework contracts.** predictors/visit_sequence, tuning rewire (guaranteed trigger +
   cross-m caching + seed), plugin registry, complete()-masking fix, open learner space, type-stable
   returns, safe defaults. This is the "flexibility beats mice" release.
3. **Wave 3 — diagnostics + docs.** plot.vimmi (chains/density/strip), mira-compatible with(),
   quality feedback by default, ampute-equivalent (extend makeNA), the vignette that actually runs
   MI + pooling + tuning end-to-end, reference fixes, MAR/MNAR statements.
4. **Wave 4 — evidence.** Benchmark vignette vs mice/missRanger/mixgb using the (fixed) evaluation
   metrics + amputation generator, coverage simulation validating Rubin-rules properness of the
   (fixed) MI defaults. This is simultaneously the R Journal paper's empirical section
   (→ `~/workspace26/vimpute-paper`).

---

## Audit blind spots

- Vignette *build* health unchecked this run (check ran `--ignore-vignettes`); content was reviewed.
- Performance/memory scaling beyond n=3000 not benchmarked; parallelism story only code-reviewed.
- pkgdown site, NEWS.md versioning policy, and the two pending `*-rebased` PR branches
  (restrictionRegression, transformerImpute) were out of scope — worth a follow-up pass before the
  7.2 release, since restrictionRegression adds edit-rule machinery adjacent to donorcond.
- The dedicated completeness-critic agent did not run (spend limit); the assessment below is the
  synthesizer's own pass over the 16 completed agents, in its place.

### Completeness assessment (in lieu of the critic agent)

**Is this enough to start the roadmap?** Yes. The correctness core (MI properness, cellwise engine,
robust-chain, vimpute internals) and the framework redesign are covered in depth, and every
P0/P1/bug was adversarially reproduced in live R — 94 claims CONFIRMED, 2 REFUTED. Wave 1 can begin
immediately without waiting on anything below.

**Genuine coverage gaps** (beyond the per-run blind spots above), in rough priority:

- **Backward-compatibility / migration story — not audited.** The 7.0→7.1 line deprecated `irmi`,
  switched `regressionImp` to glmnet, and made `rangerImpute`/`xgboostImpute` thin delegators. No
  agent checked whether the 2016 JSS paper's examples and existing user code still behave — a real
  regression risk for a CRAN package with a citing user base. Deserves an explicit pass before 7.2.
- **External-method literature scan — completed 2026-07-06** (direct Consensus query; see the
  Literature-scan section). Internal-method and competitor comparisons were already complete.
- **Visualization ↔ vimpute integration — only partially covered.** The usability agent found
  `marginplot(delimiter="_imp")` errors on vimpute's own data.table output (P2); whether the rest of
  the ~20-function viz suite understands vimmi/vimpute output was not tested systematically.

**Priority calls worth a second look:**

- The three strategic P0s (pooling surface, MI diagnostics, predictorMatrix) are P0 *for the
  beat-mice aim*, not correctness bugs — kept at P0 but tagged strategic, so they can be re-ranked
  against the paper timeline independently of the correctness P0s.
- `imputeRobustChain` (P0.4) is genuinely broken but low user-impact; it is P0 for **CRAN hygiene**
  (ships broken, masked by `globalVariables()`), not because users hit it. Reasonable to demote to
  P1 if the decision is to deprecate rather than repair.
- The cellIRWLS defect (P0.3) is correctly P0: nobody depends on 7.1.0's brand-new cellwise methods
  yet, but the paper cannot claim cellwise superiority with an engine that loses to median
  imputation.

**Single highest-value addition:** a Rubin-coverage simulation of the *fixed* MI defaults (Wave 4) —
it validates the P0.1/P0.2 fixes and doubles as the R Journal paper's headline evidence.
