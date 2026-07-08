# =============================================================================
# Tuning controls for vimpute() (audit P2.63 tail)
# =============================================================================

#' Control the hyperparameter tuning of `vimpute()`
#'
#' Bundles the tuning controls that `vimpute(tune = TRUE)` previously
#' hard-coded: the evaluation budget, the cross-validation folds of the
#' tuning resampling, the mlr3tuning tuner and its batch size. Pass the
#' result to `vimpute(tune_control = )`. With `m > 1` the control applies to
#' the single tuning run (run 1), whose parameters all `m` imputations share.
#'
#' Nested resampling is intentionally not offered: the goal of tuning inside
#' an imputation loop is good imputations, not an unbiased estimate of the
#' learner's generalisation error, and the tuned-vs-default comparison that
#' `vimpute()` runs on a fresh resampling already guards against tuning that
#' overfits the folds.
#'
#' @param budget `NULL` or a single positive integer: the number of
#'   configurations the tuner evaluates. `NULL` (default) keeps the
#'   per-learner, data-size-dependent budgets of the built-in search spaces
#'   (see [vimpute_search_space()]).
#' @param folds `NULL` or a single integer >= 2: the cross-validation folds
#'   used during tuning (capped by what the data supports). `NULL` (default)
#'   keeps the heuristic of 5 folds up to 3000 rows, 3 above.
#' @param tuner Single string: an mlr3tuning tuner id, e.g.
#'   `"random_search"` (default) or `"grid_search"`. Tuners beyond the
#'   defaults may require additional packages.
#' @param batch_size Single positive integer: configurations evaluated per
#'   tuner batch. The default `1` keeps the random-search RNG consumption
#'   machine-independent, so `seed =` reproduces tuning results everywhere;
#'   larger values evaluate in batches (faster with a parallel
#'   `future::plan`, still reproducible for a fixed value).
#' @return An object of class `vimpute_tune_control`.
#' @seealso [vimpute()], [vimpute_search_space()]
#' @export
#' @examples
#' \dontrun{
#' data(sleep)
#' res <- vimpute(sleep[, c("Sleep", "Dream", "Span")], method = "ranger",
#'                tune = TRUE, sequential = FALSE,
#'                tune_control = vimpute_tune_control(budget = 10, folds = 3))
#' attr(res, "tuning_log")
#' }
vimpute_tune_control <- function(budget = NULL, folds = NULL,
                                 tuner = "random_search", batch_size = 1L) {
  is_count <- function(x, min) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min && x == as.integer(x)
  }
  if (!is.null(budget) && !is_count(budget, 1)) {
    stop("'budget' must be NULL or a single positive integer (number of evaluations).")
  }
  if (!is.null(folds) && !is_count(folds, 2)) {
    stop("'folds' must be NULL or a single integer >= 2.")
  }
  if (!is.character(tuner) || length(tuner) != 1L || is.na(tuner) || !nzchar(tuner)) {
    stop("'tuner' must be a single mlr3tuning tuner id (e.g. \"random_search\").")
  }
  if (!is_count(batch_size, 1)) {
    stop("'batch_size' must be a single positive integer.")
  }
  structure(
    list(
      budget     = if (is.null(budget)) NULL else as.integer(budget),
      folds      = if (is.null(folds)) NULL else as.integer(folds),
      tuner      = tuner,
      batch_size = as.integer(batch_size)
    ),
    class = "vimpute_tune_control"
  )
}

#' The built-in tuning search space of a learner
#'
#' Returns the search space and default evaluation budget that
#' `vimpute(tune = TRUE)` uses for one of its built-in learners. The bounds
#' and budgets scale with the task size. Useful as the starting point for a
#' `search_space` hook in [register_vimpute_method()] and for inspecting
#' what the tuner explores.
#'
#' @param learner_id Single string: an mlr3 learner id used by vimpute's
#'   built-in methods, e.g. `"regr.ranger"`, `"classif.xgboost"`,
#'   `"regr.cv_glmnet"`, `"regr.lm_rob"`, `"regr.gam_imp"`.
#' @param task An mlr3 task (its number of rows scales the bounds and the
#'   budget).
#' @return A list with elements `space` (a `paradox::ParamSet`, or `NULL`
#'   if the learner has no built-in space) and `n_evals` (the default
#'   evaluation budget).
#' @seealso [vimpute_tune_control()], [register_vimpute_method()]
#' @export
#' @examples
#' \dontrun{
#' task <- mlr3::TaskRegr$new("t", backend = data.frame(y = 1:20, x = 1:20),
#'                            target = "y")
#' vimpute_search_space("regr.ranger", task)
#' }
vimpute_search_space <- function(learner_id, task) {
  build_vimpute_search_space(learner_id, task)
}
