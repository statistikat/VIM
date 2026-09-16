# VIM 7.5.0

## Changes
- **`imputeCellGLoc()` imputes missing categorical cells under its own model** (new argument
  `categorical = c("em", "level")`, default `"em"`, placed after `start` so positional calls keep
  working). Each categorical variable gets a multinomial logistic regression on the other
  categorical variables. A row with a missing categorical cell enters the estimation once per
  candidate level, weighted by the level's posterior probability, and the posterior combines that
  prior with the density of the row's unflagged continuous cells, so a contaminated cell does not
  steer the level. Missing categorical cells are now imputed (posterior mode); until 7.4.1 they
  stayed `NA`, and in the fit they formed an extra design level that pooled rows of different
  groups. The fit is an EM-type algorithm for a pseudo-likelihood. Rows missing several
  categorical cells use mean-field sweeps, an approximation, reported in `cat_multi_missing`.
- **New return values.** `cat_posterior` (per variable, the posterior probabilities of the missing
  cells) and `cat_prob_observed` (for each observed categorical cell, the probability of its own
  level as if it were missing; a small value points at a miscoded cell), and `cat_priors` (the fitted
  prior models). `U` now holds the
  expected design rows under `"em"`.
- **`$criterion` gains a fifth entry, `categorical`**, the largest change of a posterior probability
  in the last iteration; it enters the stopping rule, and the non-convergence warning names it. The
  first four entries are unchanged.
- **`categorical = "level"` reproduces 7.4.1**, and without a missing categorical cell `"em"` gives
  the same `B`, `Sigma`, `W` and `imputed` as `"level"`, bit for bit.

# VIM 7.4.1

## Changes
- **`imputeCellGLoc()` now starts the soft corner from a robust fit** (new argument
  `start = c("robust", "classical")`, default `"robust"`, placed after `trace` so positional calls
  keep working). Until 7.4.0 the iteration began with every observed cell at weight 1 and the mean
  structure by ordinary least squares, and a redescending weight function started there can settle
  on a masked solution. The robust start fits each continuous column on the categorical design
  alone, where no predictor cell can be contaminated, along `robustbase::lmrob`'s M-S path (see
  below), and takes the starting flags from `cellWise::cellMCD()` on those residuals. The median
  used to centre each column is put back through the design, so designs without an intercept
  column (`~ f - 1`) work. The start is deterministic. Degraded paths warn and print nothing to the
  console: too few rows per design column, a rank-deficient design (fitted on its non-aliased
  columns), `lmrob` failing or not converging, `cellMCD()` failing, `cellWise` unavailable.
- **The start selects the fixed point, not only the path.** The iteration can have several fixed
  points. On clean data (n = 200, 6 continuous and 6 categorical variables, 20% missing, tolerance
  1e-8) the two starts reached different fixed points in 7 of 10 fits with `design = ~ .` (relative
  scatter difference 0.009-0.032, 2-14 cells flagged differently) and in 4 of 9 converged fits with
  `~ 1`. Which one is reached depends on the starting mean and the starting weights together, and
  on how the weights are built, so no single part of the start is the cause. (The soft starting
  weights that reached the robust start's fixed point in 6 of the 7 differing `~ .` fits were
  computed from conditional residuals that used the start's hard flags twice: as the cell weights
  of the scatter and as the set of peers conditioned on.) (Corrected: an
  earlier draft said the hard starting flags were not the cause.) Under contamination the
  classical start can mask: in the pilot (`design = ~ .`, 20% of cells shifted by 10) its scatter
  error was 6.45 against 0.14.
- **The start's fit is `lmrob`'s M-S path: the L1 regression followed by an M-step at the L1
  residual scale.** This is what `lmrob(..., init = "M-S")` does when every predictor is
  categorical, and it gives the same coefficients. `lmrob`'s default S-estimator start failed to
  converge for some column in 42 of 90 pilot fits with `design = ~ .`, as often on clean data as
  under contamination, and each such column then started without its group means. It remains the
  fallback. (Corrected: earlier drafts of this entry called the fit "MM regression". There is no S
  step on this path, so it is not the MM estimator.)
- **The caller's random-number state is left as it was.** An existing stream is not advanced, and a
  session without a `.Random.seed` still has none afterwards. (Corrected: an earlier draft said the
  start "leaves the caller's random-number stream untouched" without qualification. But
  `cellWise::cellMCD()` creates `.Random.seed` when there is none, so the default soft fit and the
  binary corner left one behind, and two fresh sessions then drew identical numbers after one
  call. `imputeCellGLoc()` now removes a `.Random.seed` that it created.) A `.Random.seed` that the
  caller's own `data` argument creates while it is evaluated, as in
  `imputeCellGLoc({ set.seed(1); d })`, is the caller's and is kept.
- **A rank-deficient design no longer silently loses its mean structure** (present since 7.4.0).
  When the design was rank deficient on the rows a variable is fitted from, the mean step silently
  kept only that variable's weighted mean, assuming the first design column is the intercept. With a
  factor duplicated, the group means of x1 came back flat at 0.39 against 0.17, 3.92 and -3.15, and
  its variance at 8.5 against 0.87, in every corner. With a level that never records x1 (a survey
  skip pattern) the means came back flat at 15.44 under `~ f`, and 14.97, 0 and 0 under `~ f - 1`,
  where the level b and c cells were imputed near 0 (truth 20 and 30). Now the variable is fitted on
  a maximal set of independent design columns, as `qr.solve()` would for a full-rank design, whose
  results are unchanged bit for bit. Duplicated columns get coefficient 0, which leaves the fitted
  means unchanged, and a warning names them when every combination the data hold is identified. A
  level combination that the fitted rows do not identify warns once per variable, from the final
  iterate, naming the variables and the combinations (not the aliased design columns): the
  variable's mean there is not identifiable from the data. The fit fills such combinations in the
  null space of the fitted rows' design, which leaves every identified fitted mean as it is: first
  the unidentified combinations that occur in the data, by one least-squares step, then those
  absent from the data, only in the null-space directions that step leaves free, so an absent
  combination never moves a row that exists. Identification and targets are decided on level
  combinations, not on design rows: every combination of the design's levels (up to 4096,
  otherwise those in the data) with its pure design row, where a row whose categories are unknown
  can be marked so that it never counts as a combination. A combination absent from the data is
  filled without a warning. Identification is decided on the cells whose weight exceeds 1e-8 of
  the column's largest: a fit that is full rank on those cells keeps its full-rank result, tiny
  cells included, and a fit that is rank deficient on them is filled from them alone. The target
  is the variable's weighted mean over the rows it was estimated from (several such combinations
  meet it on average, with the identified effects kept between them), or, under a design with
  interaction terms, the main-effects fit wherever that identifies the combination; a warning says
  so when that design cannot be built. The fitted means do not depend on the coding or order of
  the factors, nor on whether absent combinations are enumerated. The robust start fills by the
  same rule, towards the column median, and takes main-effects targets only when its own fit of
  the column succeeded.

  (Corrected, three times. The first version of this fix filled design columns one at a time,
  recognised an unobserved level only as an all-zero column, and was described here as holding
  "under any coding of the factor". It missed a never-observed reference level under treatment
  coding, where level a came back 30.72 against a truth of 10 with only a warning about duplicate
  columns; it divided by zero under sum contrasts, returning non-finite fitted means and
  imputations; and it sent a combination missing only from an interaction to the grand mean, 14.03
  against a truth of 30. The second version fixed those, but a function call in an interaction
  term, as in `~ g1 * relevel(g2, ref = "B")` or `~ C(g1, contr.sum) * g2`, made the fit stop
  with "object not found", where the release before had fitted both. It also decided
  identification on design rows, so that 50 probability-weighted rows (0.1, 0.9, 0) sent an
  unobserved level to 71.02 against a target of 25.11. With a level at weight 1e-16 its means
  depended on the coding (20.07, 39.52 and 80.27 for that level under treatment, sum and Helmert
  coding). It let a robust start whose own fit had failed take main-effects targets, and it warned
  again whenever the unidentified set changed between iterations, the first time with a stale
  set. The third version read a factor name such as `g 1` without backticks, so the main-effects
  design failed silently and (c, C) was imputed at 13.98 against a truth of 30. It let
  combinations absent from the data move present ones within one least-squares step: under partial
  aliasing imputed values changed by up to 1.80, and (c, A) / (c, B) moved from 5.55 / 10.45 to
  8.05 / 12.94 when absent combinations were not enumerated. And it applied the weight floor only
  to fits that were already rank deficient, so a level at weight 1e-9 was fitted (20.05) or filled
  (35.06) depending on whether another level was observed.)
- **The start runs `cellMCD()` at its own tolerance, `alpha = 0.5`.** cellMCD refuses a column whose
  marginal outliers plus missing values exceed `1 - alpha`. At the default `alpha = 0.75` it did so
  in 74 of the 180 robust-start pilot fits with 20% missing cells, all at 10–20% contamination, 67
  of them with shifts of 6 or 10 and 7 with a shift of 3, and the start fell back to cruder MAD
  flags. (Corrected: earlier drafts said 74 of 360 fits, all with shifts of 6 or 10.) The `alpha`
  argument keeps governing the binary corner.
- **`cellWise::cwLocScat()`'s warning "There were rows with only zero weights, we dropped them" no
  longer reaches users.** A row whose cells are all missing or flagged carries no weight, so dropping
  it leaves the estimate unchanged (verified: identical location and scatter).
- **Imputation now conditions on unflagged cells only.** In 7.4.0 a missing continuous cell was
  imputed by its conditional expectation given every observed cell in its row, flagged cells
  included, so a grossly contaminated cell was carried into the imputation of its row-mates. It is
  now imputed from the cells that pass the same peer rule detection uses (`peer_w_min` with the
  peer band), in both weight corners. `B`, `Sigma` and `W` do not depend on this step and are
  unchanged; imputed values in rows with a flagged cell are not.
- **With `start = "classical"`, `B`, `Sigma` and `W` reproduce 7.4.0**: bit for bit on the platform
  that wrote the stored reference (Apple's Accelerate BLAS; the test checks this when
  `VIM_BITREF=true`), and to 1e-10 elsewhere, where R's reference BLAS moves the last digits by up
  to 4.4e-15 with identical iteration counts. (Corrected: an earlier draft claimed bit-identity
  without that qualifier, and the test asserted it on all six CI platforms, where it failed.)
  Imputed values differ from 7.4.0 in the rows with a flagged cell, and the test asserts that
  split. `start` has no effect with `weights = "binary"`, which already begins with
  `cellWise::cellMCD()`.

# VIM 7.4.0

## New features
- **`imputeCellGLoc()`**: cellwise-robust estimation of location and scatter on a *categorical mean
  structure*. Cell detection uses the conditional residual of a cell given the other continuous
  cells in its row **and** the row's categorical pattern, instead of pooling across groups. Pooling
  targets the marginal scatter when detection and conditional imputation need the within-group
  scatter; on NHANES the two differ by 31% in Frobenius norm. `design = ~ .` models main effects,
  `~ .^2` adds interactions, `~ 1` reproduces the continuous-only behaviour. With `design = ~ 1` the
  estimator reduces to `cellWise::cellMCD()` under `weights = "binary"` and to the cellwise weighted
  MLE under `weights = "soft"`; both reductions are covered by tests.

## Changes
- **`imputeCellGLoc()`'s peer-inclusion threshold is now a band, and the `design = ~ .` arm
  converges far more often.** "Condition on a peer only if its weight exceeds `peer_w_min`" made
  the weight map discontinuous, and a discontinuous self-map of the weight cube need not have a
  fixed point at all -- so the stopping rule could be asking for a state that does not exist, and
  on the categorical-mean-structure arm it usually was. A peer whose weight lies within
  `peer_band` of the threshold is now conditioned on with its information discounted, as though
  observed with measurement error, instead of switching in and out at a step. Confidently clean
  and confidently flagged peers are treated exactly as before, so nothing changes outside the
  ambiguous fringe.

  Measured over 260 fits of the mean-structure arm, `converged` goes from **131 to 245 (94%)**.
  The gain is *not* the band alone: raising the relaxation floor and adding the scatter condition,
  with the cut still hard, already gets 181, so about 40% of it is the floor. Convergence is not
  universal -- the remaining failures sit at correlations of 0.6 and above combined with 20% of
  cells contaminated, and simulation code must not assume otherwise. On the narrower pilot
  configuration (n = 200, 6 continuous and 6 categorical variables, 40 fits) it is 15/40 to 40/40
  with the mean time per fit falling from 15.7 s to 5.6 s, and with `design = ~ 1` 38/40 to 40/40
  and 3.3 s to 1.7 s.

  **The band changes the estimate and is not an accuracy improvement.** Over 520 paired fits
  against `peer_band = 0` on the same data, the scatter moves by up to about 20% per fit in either
  direction (largest relative changes -19.9% and +20.7%; 22% of fits move by more than 1%), with a
  mean effect near zero (relative error 0.8401 against 0.8389) and no measurable effect on
  detection (F1 0.4837 against 0.4842). The direction is a coin flip and no predictor of it was
  found. The reduction to `cellWise::cellMCD()`, the reduction to the Gaussian MLE, the
  Fisher-consistency check and the outlier-propagation rates are unchanged to machine precision.

  `peer_band = 0` restores the hard cut, but it does **not** reproduce a pre-7.4.0 fit, because
  the relaxation floor moved with it and the two interact: the hard cut converges 208 of 260
  pooled fits at the new floor of 0.5 against 245 at the old 0.25.
- **`imputeCellGLoc()` now tests the scatter for convergence as well**, not only the fitted means
  and the cell weights. `Sigma` was the one returned quantity with no stopping test of its own.
  Adding a condition can only make convergence harder, never easier. In practice it is a formal
  guard rather than an active one: it has not yet been observed to bind, standing at most a
  seventh of its tolerance when the other two conditions are first met.
- **`imputeCellGLoc()` returns `$criterion`**, the named vector
  `(means, scatter, weights, scatter_spread)`: the three stopping residuals plus the elementwise
  spread of `Sigma` over the last 20 iterations, relative to its largest variance. When
  `converged` is `FALSE` the last of these says how much the returned scatter depends on where
  `maxit` stopped, which the returned `Sigma` alone cannot show. It matters mainly for
  `weights = "binary"`, where the cell weights come from `cellWise::cellMCD()` and are binary by
  construction, so the band does not apply: of 10 non-converged binary fits, 8 are bit-identical
  at a raised `maxit` (a settled cycle) and 2 still move, by 0.008 and 0.096 relative. That
  is the published estimator's own discreteness and is not fixed here, but it is now detectable
  without parsing a warning string. `scatter_spread` was positive for all 10, which is the
  intended behaviour -- every one of them returns a scatter that depends on the stopping point --
  but it does not separate a cycle from a drift and must not be read as predicting how much the
  answer would change at a different `maxit`.
- **The relaxation floor `.gloc_damp` rises from 0.25 to 0.5.** The low floor existed to suppress
  the limit cycles that the band removes; at 0.25 the relaxed iteration is simply slow, and on the
  mean-structure arm it no longer fits inside `maxit` (35/40 against 40/40 at 0.5). It is the
  right floor only *with* the band -- under the hard cut the lower floor is better. Relaxation
  moves no fixed point, so this changes speed and not the answer -- verified: with
  `peer_band = 0` and `damp` pinned, the new code reproduces the old to 1e-7 in scatter and to an
  identical flagged set.
- **`imputeCellGLoc()`'s non-convergence warning had its advice backwards.** It suggested widening
  the conditioning set with a *larger* `peer_w_min`; larger values discard more peers.
- **`imputeCellGLoc()`'s convergence test is now independent of the relaxation
  factor.** The weight update moves `damp * (f(W) - W)`, so comparing the raw step
  against `eps` compared `damp` times the fixed-point residual, and a relaxed run stopped at a
  proportionally looser residual -- four times looser at the 0.25 floor. The step is now divided
  by `damp` first. This is a behaviour change: fits are slightly tighter and take a few more
  iterations than 7.3.1 at the same `eps`, and cell detection shifts accordingly. In the
  unsaturated regime (cell shifts of 2 to 4) the old rule gave the relaxed run a systematically
  lower recall, by up to 0.5 percentage points on average (paired p < 0.001); that systematic
  component is gone.
- **`imputeCellGLoc()` is faster: about 1.8x at 10 continuous variables and more at 20.** The
  scatter step is about 95% of an iteration (`Rprof`, n = 1000, p = 10, five draws; the
  figure was quoted as 97.5% and as 98.8% before it was re-measured), so `cwLocScat()`'s EM
  tolerance is now exposed as
  `cw_crit` and defaults to 1e-8 rather than that function's own 1e-12, which is five orders of
  magnitude tighter than `eps` can resolve; and the relaxation factor is adaptive
  (`damp = NULL`), starting unrelaxed and strengthening only when the iteration stops
  contracting. Measured end to end against 7.3.1 at n = 1000, 5% of cells contaminated: 1.83x
  pooled over three draws at p = 10 (range 1.69-1.90) and 4.7x pooled over two draws at p = 20
  (range 3.2-9.6, too few draws to pin down). Part of the gross saving is given back by the
  stricter convergence test above, which is the right trade.

## Deprecated
- **`imputeCellMCD()` is deprecated** in favour of `imputeCellGLoc()`. It continues to work
  unchanged; `imputeCellGLoc(design = ~ 1)` is the direct replacement. The old name referred to an
  estimator the function never called: it initialises with `robustbase::covMcd()`, estimates with
  `cellWise::cwLocScat()` and weights cells with a Tukey bisquare.

# VIM 7.3.1

- **The mlr3 stack moved from `Imports` to `Suggests`.** `mlr3`, `mlr3pipelines`,
  `mlr3learners`, `mlr3tuning`, `paradox`, `R6` and `future` back `vimpute()`
  and nothing else -- 6 of 58 R files -- yet as hard dependencies they tied
  VIM, and every package importing VIM, to the fate of the mlr3 chain: CRAN
  archives reverse dependencies recursively, so one archived link would have
  taken VIM down and sdcMicro, simPop, robCompositions, riskutility, deepImp
  and 15 further packages with it. None of those packages executes a line of
  mlr3-backed code; they use `kNN()`, `hotdeck()` and `gowerD()`, which are
  mlr3-free. VIM now installs, loads and runs its visualisation, donor and
  IRMI machinery without the stack present.
  `vimpute()` -- and `regressionImp()`, `rangerImpute()`, `xgboostImpute()`
  and `overimpute()`, which delegate to it -- stop with an actionable message
  naming every missing package when the stack is absent. No behavioural change
  when it is installed.
- `vimpute()` no longer calls `lgr::get_logger()` unconditionally; `lgr` is a
  suggested package and is now checked before use.

- CRAN check-time reductions for the r-devel-windows 10-minute budget: the
  `vimpute` and `vimpute-mi` vignettes are precomputed like the simulation
  vignettes (code runs in `vignettes/precompute.R`, the check renders text
  only), and thirteen further long-running regression-test files run only
  with `NOT_CRAN=true` (CI and `devtools::check()`). No user-facing changes.

# VIM 7.3.0

## Breaking changes
- **`with()` on a `vimmi` object returns a mice-compatible `mira`** (elements `call`, `call1`, `nmis`, `analyses`) instead of an anonymous list, so `mice::pool()`, `summary(pool(fits))` and `mice::getfit()` run unchanged. Code that indexed the old list directly (`fits[[i]]`, `lapply(fits, ...)`) should use `mice::getfit(fits)` or `fits$analyses`.
- **`vimpute()` returns are type-stable**: the result is always the imputed data, classed like the input (data.frame in, data.frame out; data.table in, data.table out). With `tune = TRUE` or `pred_history = TRUE` the diagnostics are attached as `attr(result, "tuning_log")` / `attr(result, "pred_history")` instead of switching the return to a bare list.
- **`vimpute()`'s default `uncert` is now `"pmm"`** (random draw among the 5 nearest donors): default numeric imputations are observed donor values with an honest distribution, instead of deterministic conditional means. Set `uncert = "none"` for the previous behaviour. Default `m > 1` runs are now stochastic between imputations. `rangerImpute()`, `xgboostImpute()` and `regressionImp()` keep their deterministic behaviour (`uncert = "none"` pinned internally).
- **`vimpute(m > 1)` now bootstraps by default**: `boot` defaults to `TRUE` for multiple imputation -- each of the `m` imputations refits its models on a bootstrap sample, so parameter uncertainty is propagated into the draws (approximately proper multiple imputation in combination with the default PMM) -- and to `FALSE` for single imputation. Set `boot = FALSE` explicitly for the previous single-fit behaviour.
- **`uncert = "pmm"` and `"midastouch"` now perform true predictive mean matching** (Little 1988): donors are matched on their *predicted* values -- scored by the trained model on the observed rows, out-of-bag for a no-bootstrap ranger fit -- instead of on their observed values. Matching on observed values selected donors whose values happened to lie near the prediction (including gross outliers from other covariate regions) and its donor spread shrank with n; predicted-value matching carries residual-scale variability, as in mice and `Hmisc::aregImpute()`.
- **Factor targets now receive a class-probability draw whenever `uncert != "none"`**: the imputed category is sampled from the predicted class probabilities on every sweep (the convergence criterion tracks the noise-free most-probable class), so early stopping no longer strips factor variables of between-imputation variability; under `uncert = "none"` factor imputation is now fully deterministic (previously `sequential = FALSE` drew stochastically regardless of `uncert`).
- **`diabetes` is now a synthetic data set**, generated by Matthias Templ from `mlbench::PimaIndiansDiabetes2` with the synvey package: the team that collected the original Pima Indians Diabetes data has asked for redistribution to stop. Column names, types, dimensions and the `Outcome` levels are unchanged, so code runs as before, but the values -- and any results computed from them -- differ from earlier VIM versions; 0 pregnancies is now a valid value rather than `NA`.

## New features
- **Method registry**: `vimpute()`'s imputation methods are resolved through a package-level registry, and `register_vimpute_method()` adds a user-defined method backed by any mlr3 learner pair in one call -- e.g. `register_vimpute_method("cart", learner = list(regr = "regr.rpart", classif = "classif.rpart"), packages = "rpart")`, or lightgbm via `mlr3extralearners` -- usable everywhere the built-in names work (global `method`, per-variable lists, method-keyed `learner_params`, `tune = TRUE` via an optional `search_space` hook). `vimpute_methods()` lists the registered methods, `unregister_vimpute_method()` removes user-added ones. The six built-ins are seeded through the same contract; unsupported-method errors now name the offending method and list what is registered.
- **Per-variable specs**: `vimpute(data, spec = list(Sleep = vs_ranger(num.trees = 300, tune = TRUE), NonD = vs_robust(donorcond = ">= 0"), .default = vs_ranger()))` bundles a variable's method, learner parameters, `formula`/`predictors`, `tune`, PMM settings, `makeNA` and `donorcond` into one object instead of nine parallel arguments. The `vs_*()` constructors (and `vimpute_spec()` for registered methods) validate learner parameters **eagerly** against the method's parameter set, so a typo fails at the constructor call, not mid-imputation. The flat per-variable arguments keep working unchanged; specs compile to exactly them.
- **Formula grammar**: per-variable settings can be written as bare formulas, `vimpute(data, Sleep ~ Dream + Span | ranger(tune = TRUE), NonD ~ . | robust(donorcond = ">= 0"), .default = vs_ranger(), m = 20, seed = 1)`. A plain-column right-hand side restricts the `predictors` (works for every method, including ranger/xgboost); a right-hand side with transformations (`s(x)`, `log(x)`, `I(x^2)`) becomes a model `formula` (formula-capable methods only). Grammar formulas compile to specs. Note: `vimpute()`'s signature now has `...` in second position for the grammar -- arguments after `data` must be passed by name (they always were in all documented usage).
- `vimpute()` gains `seed` for whole-run reproducibility (applied once at entry, as in mice; the `m` imputations still differ from each other).
- `vimpute()` gains `predictors`: per-variable predictor control -- the equivalent of mice's `predictorMatrix` (named list or 0/1 matrix), working for every method including `ranger` and `xgboost` (where `formula` is rejected). A variable's `formula` takes precedence over its `predictors` entry.
- `vimpute()` gains `visit_sequence`: `"asis"`, `"increasing.na"`, `"decreasing.na"`, or an explicit permutation of the NA-variables.
- `vimpute()` gains `tuned_params` to apply (and reuse) tuned hyperparameters without running the tuner; each `tuning_log` entry now carries its chosen parameters in `$params`.
- `vimpute()` gains `tune_control = vimpute_tune_control(budget = , folds = , tuner = , batch_size = )`: the tuning evaluation budget, resampling folds, tuner and batch size are user-controllable (defaults keep the built-in data-size heuristics); `tuning_log` entries record the budget and folds used. `vimpute_search_space()` exports the built-in per-learner search spaces (the starting point for `register_vimpute_method()` `search_space` hooks). Nested resampling is intentionally not offered -- the goal is good imputations, not unbiased generalisation estimates.
- With `m > 1` and `tune = TRUE`, tuning now runs **once** (in the first imputation) and the chosen parameters are shared by all `m` imputations -- previously every imputation re-tuned independently, conflating tuner noise with missing-data uncertainty. The `vimmi` object carries the tuning report in `$tuning_log`.
- A learner failure on one variable no longer aborts the whole imputation: `vimpute()` warns (naming the variable) and falls back to a featureless learner for that variable.
- **Convergence diagnostics for multiple imputation**: `vimmi` objects store per-iteration chain statistics (mean/variance of the imputed values per variable, iteration and imputation) plus the `seed`, and `plot(vimmi)` draws mice-style convergence trace plots. Single runs expose the same trace data as `attr(result, "chain")`.
- `vim_as_mids()`: the documented name for the vimmi-to-mids conversion (`as.mids.vimmi()` is kept as the historical alias; despite the dotted name it was never an S3 method -- `mice::as.mids()` is not a generic).
- **`makeMissing()`**: amputation generator for simulation studies (the variable-wise `mice::ampute()` counterpart) -- MCAR/MAR/MNAR mechanisms with exact per-variable proportions, driver `weights`, and an `attr(., "where")` indicator that plugs directly into `evaluation()`/`nrmse()`/`pfc()`.
- **`overimpute()`**: model-agnostic calibration diagnostic (the `Amelia::overimpute()` analogue) -- the observed cells of a variable are overimputed fold by fold with multiple draws; `print()` reports the empirical interval coverage and `plot()` draws the observed-vs-imputed calibration plot. Works with any method, spec, or grammar configuration.
- **Per-variable model quality by default** (missForest `OOBerror` analogue): every `vimpute()` run reports NRMSE (numeric) / PFC (factor) per variable as `attr(result, "model_error")` -- out-of-bag for ranger, honestly labelled in-sample for the other learners -- and `print(vimmi)` shows it.
- **`plot(vimmi, "density")` and `plot(vimmi, "strip")`**: observed-vs-imputed distribution diagnostics (the `mice::densityplot()`/`stripplot()` analogues), complementing the `"chains"` convergence traces.
- **New vignette** *Multiple imputation with vimpute: pooling, tuning and diagnostics*: an executed end-to-end workflow -- `makeMissing()` -> `vimpute(m = 5)` -> chain/density diagnostics -> Rubin pooling via `with()`/`mice::pool()` and `vim_as_mids()` -> tuning with `tune_control` -> `overimpute()` calibration -> `evaluation()` against the simulated truth.
- **New vignette** *Benchmarking imputation methods*: an extensible benchmark harness (`makeMissing()` scenarios, NRMSE on the amputed cells, runtime) comparing vimpute ranger/robust and `kNN()` with mice and missRanger (both guarded Suggests); precomputed at demo scale (`vignettes/precompute.R`), paper-scale via one constant.
- **New vignette** *Validating multiple-imputation properness*: a known-truth coverage simulation of the pooled inference under the default (`boot` + `uncert = "pmm"`), the boot-free PMM variant, the textbook-proper (`boot + normalerror`), and a deliberately improper (bootstrap without residual noise) configuration -- demonstrating the anti-conservative pooled SEs the improper setting produces and the warning that guards against it.
- `?vimpute` states the missingness assumptions (MAR incl. MCAR; MNAR caveat with pointers to `makeMissing()` sensitivity simulation).
- **`imputeCellM()` gains a chained-equations data.frame interface**: `imputeCellM(data)` now imputes every variable with missing values -- each is regressed on all remaining variables, sweeps run with deterministic predictions until the imputed values stabilise (`maxit`, `eps`), and the requested `uncert` step is applied once after convergence -- returning the same `data_imputed`/`cellweights`/`converged`/`iterations` list as `imputeCellIRMI()`. `imputeCellwise(data, method = "cellM")` dispatches to it instead of erroring ("cellM requires a formula"), and the formula interface gains `uncert = "none"` (deterministic predictions; most-probable category for factors).

## Bug fixes
- **`method = "restricted"` no longer has uncertainty draws layered over its constrained solutions**: the 7.3.0 default `uncert = "pmm"` replaced the solver's rule-satisfying value with a donor draw that knows nothing about the rules -- under a rule `y >= 4` with observed donors 1 and 2, the "restricted" imputation was 2 (CI-red on all platforms since the restrictionRegression merge, which predated the new default). Registry entries can now pin their variables' uncertainty mechanism (`uncert_override`): restricted pins `"none"`, silently for the default and with a warning when a draw mechanism was requested explicitly. The per-variable model-quality report is also skipped for restricted (`model_error = FALSE` in the registry entry) -- its in-sample predict re-ran the conic solver once more per variable for a metric of marginal value, so `save_optimization_problem = TRUE` again records exactly one problem per imputed variable.
- **`uncert = "normalerror"` / `"resid"` no longer draw from an in-sample residual scale**: for learners that expose no scale of their own (ranger, xgboost), `sigma_hat` and the residual pool were derived from the model's predictions on its own *training* rows. A forest's in-sample predictions are near-interpolating, so the estimated scale came out at roughly half the true predictive spread (0.57 against a held-out 1.16 on a linear DGP with residual sigma 1): `"normalerror"` injected half the noise it should, `"resid"` sampled from a residual pool that was far too tight, and the pooled intervals of the textbook-proper `boot + normalerror` configuration under-covered badly (0.83 / 0.73 against a nominal 0.95 at 30% / 50% missingness). Where the fitted model exposes out-of-bag predictions -- ranger stores them for free, in training-row order -- both are now derived from those, which track the predictive spread to ~1%; every other learner keeps the in-sample fallback. Learners reporting a proper scale of their own (`lm`, `lmrob`, `glmrob`, `gam`, `robgam`) never took this path and are unchanged, as is the default `uncert = "pmm"`, which matches donors on predicted values and needs no scale estimate at all. The same residual pool feeds `robustboot = "stratified"` (the default) and `"residual"`, whose good/bad split was previously computed on the same over-tight residuals. A regression test pins both the scale and the residual pool against a held-out predictive SD.
- **`imputeCellIRMI()` no longer errors with "subscript out of bounds" when DDC refuses columns**: `cellWise::DDC()`'s `checkDataSet()` drops constant, too-discrete and NA-heavy columns (on the `colic` data 11 of 19 continuous columns), so `stdResid` covers only the retained submatrix while the weight-initialisation loop indexed all continuous columns. The DDC weights are now mapped back through `colInAnalysis`/`rowInAnalysis`; refused rows and columns keep univariate weights, and DDC's console chatter ("The final data set we will analyze has ...") is captured instead of leaking to the console.
- **Duplicated or non-syntactic column names no longer corrupt the cellwise imputations**: all five data.frame-interface functions (`imputeCellEM()`, `imputeCellIRMI()`, `imputeCellMCD()`, `imputeCellMM()`, `imputeCellReg()`) built per-variable model formulas by pasting column names, so with duplicated names -- e.g. after `colnames(x) <- substr(colnames(x), 1, 15)`, which maps `mucous_membranes_col` and `mucous_membranes_group` to the same name -- the response resolved to the *first* column of that name: the model was fitted on the wrong variable, factor imputations drew from the wrong level set and were silently turned into `NA` by the levels-mismatch, with repeated "response appeared on the RHS" multinom warnings. The functions now fit under internal positional names and restore the user's names on exit.
- **The weighted multinomial fit in `imputeCellM()`'s formula interface never received its weights**: `model.frame()` evaluates the `weights` argument in `data` and then in `environment(formula)` -- the caller's environment, where the internal row-weight vector does not exist -- so the cell-weighted fit *always* errored and silently fell back to the unweighted model (with a "Multinomial model failed" warning). The row weights are now passed as a column of the fitting data.
- **`classif.glm_rob` no longer inverts binary classifications**: the robust logistic learner behind `vimpute(method = "robust")` attached the fitted binomial probability -- P of the *second* factor level, the `glm`/`glmrob` convention -- to the *first* level, so binary factor imputations drew from inverted class probabilities and the deterministic argmax picked the *less* likely class: worse-than-chance imputation exactly where the signal is strong (PFC 0.70 instead of 0.13 on the diabetes data). The multiclass one-vs-rest path was oriented correctly and is unchanged; a direction regression test now pins both paths.

## Minor improvements
- Corrected references across the documentation: the cellMCD paper is JASA 119(548), 2610--2621 (was 545, 576--588, with the first author's initial wrong); the cellGMM reference now cites Zaccaria, Garcia-Escudero, Greselin and Mayo-Iscar (2025), Technometrics 67(4), 643--654 (previously wrong co-authors and title); "Journal of Computational Statistics and Data Analysis" corrected to *Computational Statistics & Data Analysis* (on 5 help pages) and "Journal of Advances in Data Analysis and Classification" to *Advances in Data Analysis and Classification* (on 19 help pages; additionally the stale "Online first" was replaced by the final volume/pages).
- The vimpute vignette no longer describes regularisation as "robustness" (shrinkage stabilises against multicollinearity/overfitting; outlier resistance is what `"robust"`/`"robgam"` provide).
- `evaluation()` no longer returns `NaN` when one variable type has no missing cells (0/0 guarded).
- `evaluation()` gains `where`, the documented name for the amputed-cell mask (matching `makeMissing()`'s `"where"` attribute); the historical `m` keeps working, supplying both errors.
- **Scale-free convergence criterion** for sequential imputation: `eps` now bounds the per-variable *relative* change (numeric: mean squared change of the imputed values divided by the variance of the observed values; factors: share of changed categories), and the run stops when the *largest* per-variable change stays below `eps` -- previously the raw changes were summed, so `eps` was meaningless across data scales (a variable measured in thousands could block convergence forever, and one variable could mask another). The full iterations-by-variables change matrix is returned as `attr(result, "convergence")`.
- Hyperparameter tuning is reproducible across machines (`batch_size = 1` for the random search).
- The cellwise weight engines handle degenerate columns quietly: `cellWeightsMCD()` keeps exact-constant columns at weight 1 instead of feeding them to `covMcd()` (every subsample is singular there, which produced repeated warnings on each outer iteration), and `cellIRWLS()` skips the `lmrob.S` initialisation for rank-deficient designs (e.g. a constant column aliased with the intercept), using the ridge-regularised cell-weighted OLS init instead.
- After tuning, the `future` plan active at entry is restored instead of being forced to `"sequential"`.
- **OpenMP threads are capped**: the Gower-distance code behind `kNN()`/`gowerD()` used every core, which CRAN's incoming checks flag ("examples with CPU time > 2.5 times elapsed time"). `options(VIM.ncores = )` now sets the number of threads; without the option VIM uses at most 2 threads under `R CMD check` (CRAN's two-core policy) and OpenMP's default otherwise. Results do not depend on the thread count. The `xgboostImpute()` example was reduced to a single two-target call (CRAN's 5-second example limit).
- **CRAN check time**: the test suite and the vignettes were reorganised to keep `R CMD check` within CRAN's 10-minute budget (the incoming pre-test on Windows took 14--17 minutes, 330 s of it tests and 280 s vignette rebuilds). The two simulation vignettes (*Benchmarking imputation methods*, *Validating multiple-imputation properness*) are now precomputed -- their code is executed by `vignettes/precompute.R` in the source repository and the output is embedded, so CRAN only renders text; the comparison chunks of the vimpute vignette use single-sweep imputations (`sequential = FALSE`) and the `overimpute()` demo three folds. Long-running tests -- hyperparameter tuning, the multiple-imputation and vimpute-default regression tests, and the two vimpute integration scripts under `tests/` -- now run only when `NOT_CRAN=true` (set by `devtools::check()`/`test()` and by the GitHub Actions workflow); CRAN runs the fast core of the suite.

# VIM 7.2.0
- `vimpute()` gains `keep_all_columns` (default `TRUE`): the full dataset is returned, with columns excluded via `considered_variables` passed through unchanged (matching `kNN()`/`hotdeck()`/`irmi()`); set `FALSE` for the previous considered-only shape.
- `vimpute()` warns when `m > 1` cannot produce between-imputation variability (no `boot`, `uncert`, or stochastic `pmm`), so improper multiple imputation is no longer silent.
- `vimpute()` returns ordered-factor columns as ordered factors (previously flattened to plain factors; the `m > 1` path also lost the level order).
- `vimpute(tune = TRUE)` now runs with `sequential = FALSE` (was a silent no-op).
- `vimpute()` handles per-variable method lists correctly: a named length-1 list validates the variable name, and an unnamed per-column list maps by column position.
- `rangerImpute()` and `xgboostImpute()` forward their hyperparameters to the backend learner.
- `regressionImp()` uses `lm`/`glm` as documented, falling back to regularized regression only when needed.
- Completed datasets are extracted with `vim_complete()`. VIM exports no `complete()` generic of its own: `mice` and `tidyr` both export one, so an exported VIM generic would mask theirs (and be masked by them) and would make packages importing VIM and `mice`/`tidyr` wholesale emit "replacing previous import" at load time. The same function is registered as a method on `mice::complete()` and `tidyr::complete()`, so `complete(obj, 1)` keeps working whenever either package is attached.
- `evaluation()` supports `vartypes = "guess"`.
- `irmi(mi > 1)` returns a list of imputations again (was a single mangled data.frame under the default `imp_var = TRUE`).
- `imputeRobust()`: `method = "gamRob"` and `uncert = "wresid"` now work (previously crashed); `uncert` is validated with a clear error; the PMM donor pool no longer includes initialised values.
- `imputeRobustChain()` repaired (previously imputed zeros or crashed on most paths).
- cellwise methods (`imputeCellIRMI()`, `imputeCellM()`, `imputeCellMCD()`) no longer scale the design matrix by cell weights, which had made them impute worse than the median on clean data; the default `init_weights` is now `"ddc"`.
- `imputeCellReg()` cell-weight computation fixed (could produce negative weights).
- `imputeCellMCD()`: documented that `boot = TRUE` does not yet propagate parameter uncertainty.
- `kNN()`/`gowerD()`: semi-continuous (`mixed`) distance variables are now range-scaled like numeric ones, so a large-scale mixed variable no longer dominates the neighbour search.
- `kNN(weightDist = TRUE)` no longer produces `NaN` imputations when distances exceed 1 (e.g. with `methodStand = "iqr"`).
- `?kNN` and `?gowerD` document the distance standardisation and the NA-sentinel convention.
- `car` moved from Imports to Suggests (Box-Cox implemented natively).

# VIM 7.1.0
- improve `vimpute()` compatibility and validation
- make `rangerImpute()`, `xgboostImpute()`, and `regressionImp()` delegate to `vimpute()`
- vimpute: fall back from regularized to robust models when too few predictor columns remain after preprocessing
- fix documentation and package check issues around `vimpute()`
- OpenMP is used in gowerD

# VIM 7.0.0
 - new function vimpute that uses `mlr3` backend for a flexible imputation method.
 
# VIM 6.2.4
 - fix infinite loop in matchImpute in case all observations of a variable are missing
 - remove parameter metric from kNN because it was not used
 - add function xgboostImpute for using a simple xgboostModel to impute
 - add imputeRobust function to impute numeric variables with robust methods (linear and non-linear ones)

# VIM 6.2.3
- default robust regression method for irmi for numeric variables changes from rlm to lmrob.

# VIM 6.1.1
- ordFun as parameter of kNN to control the function applied to ordinal variables
- methodStand option in gowerD and kNN to switch between range and interquartile range for the standardization of numerical variables
- donorcond in kNN and hotdeck extended so it also accepts NULL as list element and multiple conditions as character vector

# VIM 6.0.2
- error message in `hotdeck()` when ord_var and variable overlap
- family argument of class 'family' now work in regressionImp

# VIM 6.0.1

- add new vignettes explaining all remaining imputation methods (`irmi()`,
  `kNN()`, `hotdeck()` and `regressionImp()`). Thanks @wolfgangrannetbauer
  (#44, #45)
- Allow missing and imputed values in several visualization functions.
    - The new functionalities are showcased in the new [visualization vignette](http://statistikat.github.io/VIM/articles/VisualImp.html) (#46). Thanks
  @wolfgangrannetbauer!
- Add `tableMiss()`: A table that highlights missing and imputed values via
  colors (#47).
- Bug fix for kNN (#48) Thanks @torockel

# VIM 6.0.0

- extend documentation with new vignettes and pkgdown
- add rangerImpute() to impute values with `ranger::ranger()` (#35)
- remove support for survey objects (#36)
- remove exports for VIMGUI (#40)
- change data.table dependency from depends to imports (#41)
- bugfixes for `irmi()` with logical and integer columns (#42)

# VIM 5.1.1

* updates for `gowerD()`
* separate help pages for `maxCat()` and `sampleCat()`
* remove links to certain packages

# VIM 5.0.0

## New datasets

* bcancer
* brittleness
* colic
* diabetes
* food
* pulplignin
* toydataMiss
* wine

# VIM 4.9.1

* data set `collision` added

# VIM 4.8.0

* fixed a bug in the distance computation when different variable types are used

# VIM 4.7.18

* added `imp_var` and `imp_suffix` to `irmi()`, so it is more consistent with the other functions (#27)

# VIM 4.7.17

* added parameter addRF, only RF to `kNN()`, to use random forest in combination with `kNN()`

# VIM 4.7.12

* testthat package used for all tests
* added travis automatically building/checking
* covr for code coverage (vis functions are currently not covered by any tests)

# VIM 4.7.1
* new imputation function `matchImpute()` for imputing randomly within groups

# VIM 4.7.0

* remove handling of `impNA` in `hotdeck()`
* add regression tests
* bugfix for `irmi()` with factors (#13). Thanks @Deleetdk
* bugfix with colorspace package (#4)

# VIM 4.6.1

* use ordered logistic regression for ordinal variables in `irmi()` (#23)
* add support for ordered factors (#7)
* bugfixes (#8, #9)

# VIM 4.6.0

* bugfixes for `kNN()` and `data.table`
* bugfix for labelled vars in `hotdeck()`
* add JSS citation
* bugfix, if a `data.frame` is passed to `irmi()` (#6)
  
# VIM 4.5.0

* new option for `kNN()`: `weightDist` to use the distances for the k nearest neighbours as weights
* The R function `which.minN()` is not used anymore, instead there is a C++ function, `kNN()` is now about 1.6 times faster on a replication (100x) of the sleep dataset
* Bytecompile is enabled

# VIM 4.4.0

* bugfix  wrong observations marked as imputed in `hotdeck()`
* random sorting is now used in `hotdeck()` if no `ord_var` is defined

# VIM 4.3.0

* bugfix for `hotdeck()` with `makeNA`

# VIM 4.2.3

* bugfix for the computation of distances for ordered variables

# VIM 4.2.1

* new option for `kNN()` `useImputedDist` if the imputed values of a variable should be used in subsequent imputation of another variable.

# VIM 4.2.0

* bug fixed in `irmi()` with newer version of nnet (multinom) and if residual scale can not be computed (noise)
* Improvement Gower dist with only missing values in data.x or data.y

# VIM 4.1.0

* new parameter `modelFormula` in `irmi()`
* bug fixes in `irmi()`
* updated `hotdeck()` based on data.table -> faster and quite stable
* bug fix if range of a variable is 0 in gower.dist
* small fixed `kNN()`

# VIM 4.0.1

* small bugfix for using `makeNA` in `kNN()`
* "Nothing to impute"-Error is now a warning
* `imp_var` now updates existing TF `imp_vars` (with warning)
 
# VIM 4.0.0

* new pacakge VIMGUI contains all GUI functions
* vignettes moved to VIMGUI
* new imputation function `regressionImp()`
* roxygen style comments -> help files
