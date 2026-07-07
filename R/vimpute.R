
#' Impute missing values with prefered model, sequentially, with hyperparametertuning and with PMM (if wanted)
#' @author Eileen Vattheuer, Matthias Templ, Alexander Kowarik

## PARAMETERS ##
#' @param data
#'  Dataset with missing values. Provide as a data.table.
#' @param considered_variables
#'  A character vector of variable names to be either imputed or used as predictors, excluding irrelevant columns from the imputation process. Excluded columns are still returned unchanged by default (see \code{keep_all_columns}).
#' @param method
#'  Specifies the imputation method for each variable.
#'  Can be provided either:
#'    - as a **single global method** (e.g. "ranger"), applied to all variables, or
#'    - as a **named list** (e.g. as.list(var1 = "xgboost", var2="robust")), assigning a method to each variable individually.
#'  Supported methods:
#'   - ranger (Random Forest)
#'   - xgboost (Gradient Boosting)
#'   - regularized (glmnet regression/classification)
#'   - robust (robustbase::lmrob / glmrob)
#'   - gam (Generalized Additive Model via mgcv::gam)
#'   - robgam (Robust GAM with outlier downweighting, simple or iterative reweighting)
#' @param pmm 
#'  Predictive Mean Matching (PMM) settings.
#'  Can be provided:
#'    - as a **single TRUE/FALSE** (global), or  
#'    - as a **named list**, assigning PMM per (numeric) variable.
#' @param pmm_k 
#'  Number of nearest neighbors used in PMM.
#'  Accepted forms:
#'    - single global integer (applies to all variables), or  
#'    - named list assigning values per variable, or  
#'    - NULL (default), meaning:
#'      - k = 1 automatically for variables using PMM,
#'      - k = NULL for variables without PMM
#' @param pmm_k_method
#'  Aggregation method used when `pmm_k > 1` in PMM.
#'  Default is `"mean"`.
#'  Accepted forms:
#'    - single global string (`"mean"`, `"median"`, `"random"`), or
#'    - single global function (called with the k nearest observed values), or
#'    - named list assigning methods per variable, or
#'    - `NULL` values inside such lists, which fall back to `"mean"`
#'  Semantics:
#'    - `"mean"`: mean of the k nearest neighbors
#'    - `"median"`: median of the k nearest neighbors
#'    - `"random"`: random draw of one among the k nearest neighbors
#'    - function: custom aggregator returning one numeric value
#' @param learner_params 
#'  Hyperparameters for the chosen methods.
#'  Can be provided in **three ways**:
#'    - **Per variable**  (e.g. list(mpg = list(num.trees = 500)))  
#'    - **Per method**    (e.g. list(ranger = list(num.trees = 600)))  
#'    - **Global**, applied to all variables using the same method
#' @param formula
#'  Optional modeling formula to restrict or transform predictor variables.
#'  Only supported for **regularized** (glmnet), **robust** (lmrob/glmrob),
#'  **gam** (mgcv::gam), and **robgam** (robust GAM) methods
#'  Provide as a named list, e.g.:
#'    - list(mpg = mpg ~ hp + drat)  
#'    - list(hp  = log(hp) ~ wt + cyl) 
#'  For X: follows the rules of model.matrix
#'  For Y: transformations supported are log(), exp(), sqrt(), I(1/..). Only applicable for numeric variables.
#' @param makeNA
#'  Optional named list that defines additional values to be treated as imputable missing
#'  values per variable, similar to \code{kNN()}. For variables listed in \code{makeNA},
#'  only the specified values are imputed; existing \code{NA} values are left untouched.
#'  Variables not listed in \code{makeNA} continue to impute regular \code{NA} values.
#' @param donorcond
#'  Optional named list of donor conditions per variable, similar to \code{kNN()}.
#'  Rows whose observed target values do not satisfy the condition are excluded from the
#'  donor pool for model fitting for that variable.
#' @param sequential
#'  If TRUE, all variables with missing data are imputed sequentially across iterations.
#' @param nseq
#'  Maximum number of iterations (if sequential is TRUE).
#' @param eps
#'  Convergence threshold: the imputation process stops early if predictions change less than this amount across iterations.
#' @param imp_var
#'  If TRUE, additional columns indicating imputed values (VAR_imp) are added.
#' @param keep_all_columns
#'  If TRUE (default), the full input is returned: columns excluded via
#'  \code{considered_variables} are passed through unchanged (original column
#'  order, with any \code{VAR_imp} indicators appended), matching \code{kNN()},
#'  \code{hotdeck()} and \code{irmi()}. Set FALSE to return only the considered
#'  columns (plus their indicators), dropping the rest.
#' @param pred_history
#'  If TRUE, all predicted values across all iterations are stored.
#' @param tune
#'  Hyperparameter tuning flag. Can be:
#'    - TRUE/FALSE globally
#'    - or a list specifying tuning per variable, e.g. list(var1 = TRUE)
#'  Tuning runs once per variable, early in the iteration sequence. With
#'  \code{m > 1}, tuning runs once in the first imputation and the chosen
#'  parameters are shared by all \code{m} imputations (as in \pkg{mice});
#'  the resulting \code{vimmi} object carries the tuning report in its
#'  \code{tuning_log} element.
#' @param verbose
#'  If TRUE additional debugging output is provided
#' @param boot
#'  If TRUE, bootstrap resampling is applied before model fitting to account
#'  for model uncertainty. The bootstrap strategy is controlled by \code{robustboot}.
#'  Most effective with method = "robust". Default: FALSE
#' @param robustboot
#'  Bootstrap strategy when \code{boot = TRUE}. Options:
#'  \code{"standard"} (classical bootstrap),
#'  \code{"stratified"} (good/bad residual split, default),
#'  \code{"residual"} (inverse residual weighting).
#' @param uncert
#'  Imputation uncertainty method applied to predictions:
#'  \code{"none"} (point prediction, default),
#'  \code{"normalerror"} (add N(0, sigma_hat)),
#'  \code{"resid"} (add sampled residual),
#'  \code{"pmm"} (predictive mean matching),
#'  \code{"midastouch"} (covariate-distance-weighted PMM, Siddique & Belin 2008).
#'  If \code{pmm = TRUE} is set, it takes precedence over \code{uncert}.
#' @param m
#'  Number of multiple imputations. Default: 1 (single imputation).
#'  When \code{m > 1}, returns a \code{\link{vimmi}} object storing the original
#'  data and imputed values efficiently. Use \code{\link{complete.vimmi}} to
#'  extract completed datasets.
#' @param seed
#'  Optional single number for reproducibility. Applied once via
#'  \code{\link{set.seed}} at the start of the call (as in \pkg{mice}), so the
#'  whole run -- including all \code{m} imputations -- is reproducible while the
#'  \code{m} imputations still differ from each other. Default \code{NULL}
#'  leaves the random-number stream untouched.
#' @param tuned_params
#'  Optional named list mapping variable names to learner parameter lists
#'  (e.g. \code{list(Sleep = list(num.trees = 300L))}). The parameters are
#'  applied to the variable's learner without running the tuner -- use this to
#'  reuse tuning results across calls (each entry of a previous run's
#'  \code{tuning_log} carries its chosen parameters in \code{$params}).
#'  Used internally by \code{m > 1} to share the first imputation's tuned
#'  parameters across all imputations.
#' @return
#'  Either:
#'    - the imputed dataset (default, when \code{m = 1}), or
#'    - a list containing the imputed dataset and prediction history (when \code{pred_history = TRUE} or \code{tune = TRUE}), or
#'    - a \code{\link{vimmi}} object (when \code{m > 1}).
#' @export
#'
#' @family imputation methods
#' @examples
#' \dontrun{
#' # Single imputation (default)
#' x <- vimpute(data = sleep, sequential = FALSE)
#'
#' # Sequential imputation with 3 iterations
#' y <- vimpute(data = sleep, sequential = TRUE, nseq = 3)
#'
#' # Impute only selected variables
#' z <- vimpute(data = sleep, considered_variables =
#'        c("Sleep", "Dream", "Span", "BodyWgt"), sequential = FALSE)
#'
#' # Multiple imputation (m = 5) with bootstrap and residual uncertainty
#' # Returns a vimmi object
#' result <- vimpute(data = sleep, method = "ranger", sequential = FALSE,
#'                   imp_var = FALSE, m = 5, boot = TRUE, uncert = "resid")
#' print(result)
#'
#' # Extract completed datasets
#' d1 <- complete(result, 1)         # first imputed dataset
#' all_d <- complete(result, "all")  # list of 5 datasets
#' long_d <- complete(result, "long") # long format with .imp column
#'
#' # Fit a model on each imputed dataset
#' fits <- with(result, lm(Sleep ~ Dream + Span))
#'
#' # Multiple imputation with robust method and residual uncertainty
#' result2 <- vimpute(data = sleep, method = "robust", m = 5,
#'                    boot = TRUE, robustboot = "stratified",
#'                    uncert = "normalerror")
#' }
#########################################################################################
#########################################################################################
#########################################################################################

vimpute <- function(
    data,
    considered_variables = names(data), 
    method = setNames(as.list(rep("ranger", length(considered_variables))), considered_variables),
    pmm = FALSE,
    pmm_k = NULL,
    pmm_k_method = "mean",
    learner_params = NULL,
    formula = FALSE, 
    makeNA = NULL,
    donorcond = NULL,
    sequential = TRUE,
    nseq = 10,
    eps = 0.005,
    imp_var = TRUE,
    keep_all_columns = TRUE,
    pred_history = FALSE,
    tune = FALSE,
    verbose = FALSE,
    boot = FALSE,
    robustboot = "stratified",
    uncert = "none",
    m = 1L,
    seed = NULL,
    tuned_params = NULL
) {

  # Reproducibility: apply the seed once at entry (mice-compatible). The m > 1
  # wrapper recurses with seed = NULL, so the m runs share one seeded stream
  # and still differ from each other (re-seeding per run would collapse them).
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
      stop("'seed' must be a single finite number (or NULL).")
    }
    set.seed(seed)
  }

  # Pre-tuned hyperparameters: named list (variable -> learner parameter list).
  # Pre-seeds the tuning cache so the parameters are applied without tuning.
  # Used by the m > 1 wrapper to share run 1's tuned parameters across all m
  # imputations; can also be supplied directly to reuse tuning across calls.
  if (!is.null(tuned_params)) {
    if (!is.list(tuned_params) || is.null(names(tuned_params)) ||
        any(!nzchar(names(tuned_params)))) {
      stop("'tuned_params' must be a fully named list (variable -> parameter list).")
    }
    unknown_tp <- setdiff(names(tuned_params), names(data))
    if (length(unknown_tp) > 0L) {
      stop(sprintf("Unknown variable name(s) in 'tuned_params': %s",
                   paste(unknown_tp, collapse = ", ")))
    }
  }

  # save plan
  old_plan <- future::plan()  # Save current plan
  on.exit(future::plan(old_plan), add = TRUE)  # Restore on exit, even if error

  # Silence mlr3 info logs unless explicitly requested.
  if (!isTRUE(verbose)) {
    mlr3_logger <- lgr::get_logger("mlr3")
    old_mlr3_threshold <- mlr3_logger$threshold
    mlr3_logger$set_threshold("warn")
    on.exit(mlr3_logger$set_threshold(old_mlr3_threshold), add = TRUE)
  }

  # dots <- list(...)

  # only defined variables
  data_all_variables <- as.data.table(data)
  data <-  as.data.table(data)[, considered_variables, with = FALSE]
  
  # implement makeNA
  makeNA_match <- setNames(vector("list", length(considered_variables)), considered_variables)
  donor_mask <- setNames(vector("list", length(considered_variables)), considered_variables)
  for (var in considered_variables) {
    makeNA_match[[var]] <- rep(FALSE, nrow(data))
    donor_mask[[var]] <- rep(TRUE, nrow(data))
  }

  # implement donorcond:  conditions for "donors", ie training data for model based approaches
  if (!is.null(donorcond)) {
    if (!is.list(donorcond) || is.null(names(donorcond)) || any(!nzchar(names(donorcond)))) {
      stop("'donorcond' must be a named list.")
    }
    unknown_donorcond <- setdiff(names(donorcond), considered_variables)
    if (length(unknown_donorcond) > 0L) {
      warning(sprintf(
        "Unknown variable name(s) in 'donorcond': %s",
        paste(unknown_donorcond, collapse = ", ")
      ))
    }
    for (var in names(donorcond)) {
      cond <- donorcond[[var]]
      if (is.null(cond) || length(cond) == 0L) next
      x <- data[[var]]
      condition_string <- paste0("x", cond, collapse = "&")
      donor_ok <- tryCatch(
        eval(parse(text = condition_string), envir = list(x = x)),
        error = function(e) {
          stop(sprintf("Invalid donor condition for '%s': %s", var, e$message), call. = FALSE)
        }
      )
      if (!is.logical(donor_ok) || length(donor_ok) != nrow(data)) {
        stop(sprintf(
          "Condition in 'donorcond' for '%s' must evaluate to a logical vector of length %d.",
          var, nrow(data)
        ))
      }
      donor_ok[is.na(donor_ok)] <- FALSE
      donor_mask[[var]] <- donor_ok
    }
  }

  #set missing values in makeNA to NA
  if (!is.null(makeNA)) {
    if (!is.list(makeNA) || is.null(names(makeNA)) || any(!nzchar(names(makeNA)))) {
      stop("'makeNA' must be a named list.")
    }
    unknown_makeNA <- setdiff(names(makeNA), considered_variables)
    if (length(unknown_makeNA) > 0L) {
      stop(sprintf(
        "Unknown variable name(s) in 'makeNA': %s",
        paste(unknown_makeNA, collapse = ", ")
      ))
    }
    for (var in names(makeNA)) {
      values <- makeNA[[var]]
      if (length(values) == 0L) next
      idx <- data[[var]] %in% values
      idx[is.na(idx)] <- FALSE
      makeNA_match[[var]] <- idx
      if (any(idx)) {
        set(data, i = which(idx), j = var, value = NA)
      }
    }
  }

  # find values to impute
  makeNA_vars <- if (is.null(makeNA)) character(0) else names(makeNA)
  impute_mask <- setNames(vector("list", length(considered_variables)), considered_variables)
  for (var in considered_variables) {
    impute_mask[[var]] <- if (var %in% makeNA_vars) makeNA_match[[var]] else is.na(data[[var]])
  }
  variables_to_impute <- names(impute_mask)[vapply(impute_mask, any, logical(1))]
  
  # save factor levels
  factor_levels <- list()
  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      factor_levels[[col]] <- levels(data[[col]])
    } else if (is.character(data[[col]])) {
      # factor_levels[[col]] <- unique(na.omit(data[[col]]))
      factor_levels[[col]] <- levels(as.factor(data[[col]])) # Nnew
    }
  }

  # Remember ordered-factor columns (with their level order) BEFORE precheck()
  # coerces them to plain factors, so the output can be restored to `ordered`.
  ordered_info <- list()
  for (col in names(data)) {
    if (is.ordered(data[[col]])) {
      ordered_info[[col]] <- levels(data[[col]])
    }
  }

### ***** Check Data Start ***** ###################################################################################################
  if(verbose){
    message(paste("***** Check Data"))  
  }
  
  
  method_vector <- unlist(method, use.names = FALSE)
  
  default_method <- NULL
  if (length(method_vector) > 0 && length(unique(method_vector)) == 1L) {
    default_method <- unique(method_vector)
  }
  
  checked_data   <- precheck(
    data = data,
    pmm = pmm,
    formula = formula,
    method = method,
    sequential = sequential,
    pmm_k = pmm_k,
    pmm_k_method = pmm_k_method,
    learner_params = learner_params,
    tune = tune,
    boot = boot,
    robustboot = robustboot,
    uncert = uncert,
    m = m,
    default_method = default_method,
    variables_NA = variables_to_impute,
    verbose = verbose
  )
  data           <- checked_data$data
  variables      <- checked_data$variables
  variables_NA   <- checked_data$variables_NA
  method         <- checked_data$method
  learner_params <- checked_data$learner_params
  pmm            <- checked_data$pmm
  pmm_k          <- checked_data$pmm_k
  pmm_k_method   <- checked_data$pmm_k_method
  tune           <- checked_data$tune
  boot           <- checked_data$boot
  robustboot     <- checked_data$robustboot
  uncert         <- checked_data$uncert
  m              <- checked_data$m
  
  if (!sequential && nseq > 1) {
    if (verbose) message ("'nseq' was set to 1 because 'sequential = FALSE'.")
    nseq <- 1
  }
### Check Data End ###

  # Reconcile pmm parameter with uncert parameter (PMM has priority)
  has_any_pmm <- any(unlist(pmm))
  if (has_any_pmm && uncert != "none") {
    warning("Both 'pmm' and 'uncert' specified. 'pmm' takes precedence; 'uncert' ignored.")
    uncert <- "none"
  }

  # PMM only injects between-imputation variability when it draws at random from
  # >= 2 donors (or via a user aggregator); pmm_k = 1 or pmm_k_method =
  # "mean"/"median" is deterministic and gives identical draws every imputation.
  pmm_vars <- names(pmm)[vapply(pmm, isTRUE, logical(1))]
  is_stochastic_pmm <- function(v) {
    km <- pmm_k_method[[v]]
    if (is.function(km)) return(TRUE)
    k <- pmm_k[[v]]
    is.numeric(k) && length(k) == 1L && k >= 2L && identical(km, "random")
  }
  has_stochastic_pmm <- any(vapply(pmm_vars, is_stochastic_pmm, logical(1)))

  # Warn if m > 1 cannot produce proper between-imputation variability, so users
  # do not apply Rubin's rules to degenerate/variance-underestimating draws.
  if (m > 1L && !boot && uncert == "none" && !has_stochastic_pmm) {
    if (has_any_pmm) {
      warning("m > 1 but PMM is deterministic here (pmm_k = 1, or pmm_k_method = ",
              "'mean'/'median'): all imputations will be identical and Rubin's rules ",
              "are invalid. For proper MI set pmm_k >= 2 with pmm_k_method = 'random', ",
              "or add boot = TRUE and/or uncert (e.g. 'normalerror').", call. = FALSE)
    } else {
      warning("m > 1 without 'boot', 'uncert', or 'pmm': all imputations will be ",
              "identical and Rubin's rules are invalid. Set boot = TRUE and/or ",
              "uncert to a method like 'normalerror' for proper MI variability.", call. = FALSE)
    }
  }
  # boot alone imputes conditional means (no residual noise) -> between-imputation
  # variance is underestimated (improper MI); PMM or uncert supplies the noise.
  if (m > 1L && boot && uncert == "none" && !has_any_pmm) {
    warning("m > 1 with boot = TRUE but uncert = 'none' and no PMM: imputations use ",
            "conditional means only, so the between-imputation variance is ",
            "underestimated and pooled standard errors will be too small. Add a ",
            "residual-noise mechanism (uncert = 'normalerror' or 'resid') or PMM ",
            "for proper multiple imputation.", call. = FALSE)
  }

### ***** Learner START ***** ################################################################################################### 
  
  # Possible extension
  # LightGBM via mlr3extralearners:
  # classif.lightgbm, regr.lightgbm
  
  no_change_counter <- 0
  robust_required <- any(unlist(method) == "robust")
  gam_required <- any(unlist(method) %in% c("gam", "robgam"))  # enable GAM-based learners when needed
  
  if (robust_required) {
    register_robust_learners()
  }
  if (gam_required) {
    register_gam_learners()
  }
  
  learner_ids <- c(
    "regr.cv_glmnet", "regr.glmnet", "classif.glmnet",
    "regr.ranger", "classif.ranger",
    "regr.xgboost", "classif.xgboost"
  )
  
  if (robust_required) {
    learner_ids <- c(learner_ids, "regr.lm_rob", "classif.glm_rob")
  }
  if (gam_required) {
    learner_ids <- c(learner_ids, "regr.gam_imp", "classif.gam_imp",
                     "regr.robgam_imp", "classif.robgam_imp")
  }
  
  learners <- lapply(learner_ids, function(id) lrn(id))
  names(learners) <- learner_ids
  ensure_robust_learners <- function(learners) {
    if (is.null(learners[["regr.lm_rob"]]) || is.null(learners[["classif.glm_rob"]])) {
      register_robust_learners()
      learners[["regr.lm_rob"]] <- lrn("regr.lm_rob")
      learners[["classif.glm_rob"]] <- lrn("classif.glm_rob")
    }
    learners
  }
### Learner End ###
  
### ***** Def missing indices Start ***** ###################################################################################################
  if(verbose){
    message(paste("***** Find Missing Indices"))
  }
  missing_indices <- setNames(lapply(variables, function(var) {
    na_idx <- which(impute_mask[[var]])
    if (length(na_idx) > 0) return(na_idx) else return(integer(0))
  }), variables)
  missing_indices <- missing_indices[!sapply(missing_indices, is.null)]
  ### Def missing indices End ###
  
  po_ohe <- NULL # set ohe to zero, becomes true if ohe is needed
  data_new <- copy(data)
  original_data <- copy(data)  # Saves original structure of data

  # Multiple imputation: run m times, collect imputed values, return vimmi
  if (m > 1L) {
    if (verbose) {
      message(paste0("***** Multiple imputation enabled (m = ", m, ")"))
      message("***** Running repeated single imputations and collecting imputed values")
    }
    where_matrix <- matrix(FALSE, nrow = nrow(data), ncol = length(variables), dimnames = list(NULL, variables))
    for (v in variables_NA) {
      where_matrix[missing_indices[[v]], v] <- TRUE
    }

    imp_collector <- setNames(
      lapply(variables_NA, function(v) vector("list", m)),
      variables_NA
    )

    # Tune ONCE: run 1 tunes (if requested) and its chosen hyperparameters are
    # shared by runs 2..m via tuned_params. Re-tuning per imputation would
    # multiply the tuning cost by m and let each imputation pick different
    # hyperparameters, conflating tuner noise with missing-data uncertainty
    # (invalid for Rubin pooling; mice tunes once).
    any_tune_requested <- any(unlist(tune))
    tuned_params_runs <- tuned_params
    mi_tuning_log <- NULL

    for (mi in seq_len(m)) {
      if (verbose) message(sprintf("=== Multiple imputation run %d of %d ===", mi, m))

      # Recurse with m = 1 to produce one completed dataset per run
      single_result <- vimpute(
        data = data_all_variables,
        considered_variables = considered_variables,
        method = method,
        pmm = pmm,
        pmm_k = pmm_k,
        pmm_k_method = pmm_k_method,
        learner_params = learner_params,
        formula = formula,
        makeNA = makeNA,
        donorcond = donorcond,
        sequential = sequential,
        nseq = nseq,
        eps = eps,
        imp_var = FALSE,
        keep_all_columns = FALSE,
        pred_history = FALSE,
        tune = if (mi == 1L) tune else FALSE,
        boot = boot,
        robustboot = robustboot,
        uncert = uncert,
        m = 1L,
        tuned_params = tuned_params_runs,
        verbose = verbose
      )

      imputed_dt <- if (is.list(single_result) && !is.data.frame(single_result) && "data" %in% names(single_result)) {
        single_result$data
      } else {
        as.data.table(single_result)
      }

      # Harvest run 1's tuning results for runs 2..m (and for the vimmi report).
      if (mi == 1L && any_tune_requested &&
          is.list(single_result) && !is.null(single_result$tuning_log)) {
        mi_tuning_log <- single_result$tuning_log
        harvested <- list()
        for (entry in mi_tuning_log) {   # last entry per variable wins
          if (!is.null(entry$params)) harvested[[entry$variable]] <- entry$params
        }
        if (length(harvested) > 0L) {
          tuned_params_runs <- utils::modifyList(
            if (is.null(tuned_params_runs)) list() else tuned_params_runs,
            harvested
          )
        }
      }

      for (v in variables_NA) {
        miss_idx <- missing_indices[[v]]
        imp_collector[[v]][[mi]] <- imputed_dt[[v]][miss_idx]
      }
    }

    imp_list <- setNames(
      lapply(variables_NA, function(v) {
        df <- as.data.frame(imp_collector[[v]])
        names(df) <- paste0("m", seq_len(m))
        df
      }),
      variables_NA
    )

    nmis_vec <- setNames(
      vapply(variables_NA, function(v) length(missing_indices[[v]]), integer(1)),
      variables_NA
    )

    # Return a compact MI object (original data + imputed values only). By
    # default store the full input so complete() returns all columns; when the
    # user opts out, store only the considered subset (ordered class restored,
    # since precheck flattened it -- data_all_variables is pristine and needs
    # no restore).
    vimmi_data <- if (keep_all_columns) {
      as.data.frame(data_all_variables)
    } else {
      restore_ordered_factors(as.data.frame(original_data), ordered_info)
    }
    if (verbose) {
      message("***** Constructing compact 'vimmi' result object")
    }
    return(new_vimmi(
      data   = vimmi_data,
      imp    = imp_list,
      where  = where_matrix,
      m      = m,
      nmis   = nmis_vec,
      method = method,
      boot   = boot,
      uncert = uncert,
      call   = match.call(),
      tuning_log = mi_tuning_log
    ))
  }

  if (pred_history == TRUE) {
    history <- list() # save history of predicted values
  }

  convergence_data <- copy(data)
  
  count_tuned_better <- 0
  count_default_better <- 0
  
  hyperparameter_cache <- setNames(vector("list", length(variables_NA)), variables_NA)
  tuning_status <- setNames(rep(FALSE, length(variables_NA)), variables_NA)

  # Apply externally supplied tuned parameters: pre-seeding the cache marks the
  # variable as tuned, so the "already tuned" branch applies the parameters and
  # the tuning gate never fires for it.
  if (!is.null(tuned_params)) {
    for (v in intersect(names(tuned_params), variables_NA)) {
      if (!is.null(tuned_params[[v]])) {
        hyperparameter_cache[[v]] <- list(params = tuned_params[[v]], is_tuned = TRUE)
        tuning_status[[v]] <- TRUE
      }
    }
  }
  
  tuning_log <- list()
  
  # Iterative Imputation for nseq iterations
  for (i in seq_len(nseq)) {
    if(verbose){
      message(paste("ITERATION", i, "von", nseq))
    }
    iteration_times <- list()
    convergence_prev <- copy(convergence_data)
    
    for (var in variables_NA) {
      if(verbose){
        message(paste("***** Impute variable:", var))
      }
      var_start_time <- Sys.time()
      variables <- checked_data$variables
      
      # If only NAs -> Stop
      if (all(is.na(data[[var]]))) {
        stop(sprintf(
          "Variable '%s' contains only missing values. No model can be estimated. Please remove it from 'considered_variables' or impute it externally.",
          var
        ))
      }
      
      data_before <- copy(data)
      if(verbose){
        message(paste("***** Select predictors"))
      }
      if (!isFALSE(formula)) {
        selected_formula <- select_formula(formula, var)  # formula on left handsite
        if (verbose) {
          message(paste("Selected formula for variable", var, ":", selected_formula))
        }
      }
      
      # Extract method-specific-learner
      var_learner_params <- learner_params[[var]]
      if (is.null(var_learner_params)) var_learner_params <- list()
      
### ***** Formula Extraction Start ***** ###################################################################################################
      if (!isFALSE(formula) && (!isFALSE(selected_formula))) {
        identified_variables <- identify_variables(selected_formula, data, var)
        target_col <- var
        feature_cols <- identified_variables$predictor_variables 
        selected_cols <- c(target_col, feature_cols)
        
        rewrite_formula <- function(formula, target_variable) {
          formula_str <- as.character(formula)
          new_formula_str <- paste0(target_variable, " ~ ", formula_str[3])
          as.formula(new_formula_str)
        }
        
        rewrited_formula <- rewrite_formula (selected_formula, target_col) # write formula in the correct way
        
        # Remove missing values (na.omit)  -> for Training
        # remove donors not fullfilling donorcond
        data <- enforce_factor_levels(data, factor_levels)  
        data_clean <- na.omit(data[donor_mask[[target_col]]])
        
        check_all_factor_levels(data_clean, factor_levels)
        
        is_target_numeric <- is.numeric(data[[target_col]])
        
        if (is_target_numeric) {
          task_mm_na_omit <- TaskRegr$new(
            id = "imputation_task_na_omit",
            backend = data_clean,
            target = target_col
          )
        } else {
          task_mm_na_omit <- TaskClassif$new(
            id = "imputation_task_na_omit",
            backend = data_clean,
            target = target_col
          )
        }
        
        # modelmatrix for x variables
        po_mm_na_omit <- PipeOpModelMatrix$new()
        po_mm_na_omit$param_set$values$formula <- rewrited_formula
        rewrited_formula <- as.formula(paste("~", as.character(rewrited_formula)[3]))
        po_mm_na_omit$param_set$values$formula <- rewrited_formula
        
        mm_task_na_omit <- po_mm_na_omit$train(list(task_mm_na_omit))[[1]]
        data_temp <- mm_task_na_omit$data()
        data_temp <- as.data.table(data_temp)
        
        setnames(data_temp, clean_model_matrix_colnames(names(data_temp)))
        data_temp <- enforce_factor_levels(data_temp, factor_levels)  
        check_all_factor_levels(data_temp, factor_levels)
        
        # Impute missing values (Median/Mode)  -> for prediction 
        # Train on complete targets only (mlr3 disallows NA in target)
        data_train_mm <- data[donor_mask[[target_col]] & !is.na(get(target_col))]
        if (is_target_numeric) {
          task_mm <- TaskRegr$new(id = "imputation_task_mm", backend = data_train_mm, target = target_col)
        } else {
          task_mm <- TaskClassif$new(id = "imputation_task_mm", backend = data_train_mm, target = target_col)
        }
        
        pipeline_impute <- po("imputehist") %>>%  # Histogram-based imputation for numeric variables (Median)
          po("imputemode") %>>%                  # Mode imputation for categorical variables
          po("modelmatrix", formula = rewrited_formula) #rewrited_formula  # Create design matrix
        
        pipeline_impute$train(task_mm)

        # Predict on full data, but ensure target has no NA for task creation
        data_pred_mm <- copy(data)
        if (anyNA(data_pred_mm[[target_col]])) {
          if (is_target_numeric) {
            data_pred_mm[[target_col]][is.na(data_pred_mm[[target_col]])] <-
              median(data_pred_mm[[target_col]], na.rm = TRUE)
          } else {
            mode_value <- names(which.max(table(data_pred_mm[[target_col]], useNA = "no")))
            data_pred_mm[[target_col]][is.na(data_pred_mm[[target_col]])] <- mode_value
          }
        }
        if (is_target_numeric) {
          task_mm_pred <- TaskRegr$new(id = "imputation_task_mm_pred", backend = data_pred_mm, target = target_col)
        } else {
          task_mm_pred <- TaskClassif$new(id = "imputation_task_mm_pred", backend = data_pred_mm, target = target_col)
        }
        po_task_mm <- pipeline_impute$predict(task_mm_pred)[[1]]
        mm_data <- po_task_mm$data() # mm_data = transformed data with missings filled in, data_temp = transformed data without missings
        mm_data <- as.data.table(mm_data)
        setnames(mm_data, clean_model_matrix_colnames(names(mm_data)))
        mm_data <- enforce_factor_levels(mm_data, factor_levels)
        check_all_factor_levels(mm_data, factor_levels)
        
        
        # Identify target transformation
        lhs_transformation <- identify_lhs_transformation(selected_formula)  # transformations on left handsite 
        
        if (!is.numeric(data_temp[[var]]) && !is.null(lhs_transformation)) {
          stop(paste("Error: The target variable must be numeric if a transformation is to be applied. Current class of the target variable:", class(data_temp[[var]])))
        }
        
        # Create PipeOpTargetTrafo if a transformation is detected
        if (!is.null(lhs_transformation)) {
          if (lhs_transformation == "exp") {
            transformation <- function(x) ifelse(is.na(x), NA, exp(x))
          } else if (lhs_transformation == "log") {
            transformation <- function(x) ifelse(is.na(x), NA, log(x))
          } else if (lhs_transformation == "sqrt") {
            transformation <- function(x) ifelse(is.na(x), NA, sqrt(x))
          } else if (lhs_transformation == "inverse") {
            transformation <- function(x) ifelse(is.na(x), NA, 1 / x)
          } else {
            stop("Unknown transformation: ", lhs_transformation)
          }
          
          data_temp[[var]] <- transformation(data_temp[[var]])
          mm_data[[var]] <- transformation(mm_data[[var]])
        }
        
        
### Formula Extraction End ###   
        
      } else {
        lhs_transformation <- NULL
        selected_formula <- FALSE
        feature_cols <- setdiff(names(data), var)
        target_col <- var
        selected_cols <- c(target_col, feature_cols)
        data_temp <- as.data.table(data[, selected_cols, with = FALSE])
        data_temp <- enforce_factor_levels(data_temp, factor_levels)
        check_all_factor_levels(data_temp, factor_levels)
        
      }

      donor_ok_rows <- if (!isFALSE(selected_formula)) rep(TRUE, nrow(data_temp)) else donor_mask[[target_col]]
      
      if (!isFALSE(selected_formula) && "Intercept" %in% colnames(data_temp)) {
        data_temp <- data_temp[, !colnames(data_temp) %in% "Intercept", with = FALSE]
        if (exists("mm_data", inherits = FALSE)) {
          mm_data <- mm_data[, !colnames(mm_data) %in% "Intercept", with = FALSE]
        }
      }
      
      if (!isFALSE(selected_formula)) {
        # Formulas are only supported by methods that model via formulas
        if (!method[[var]] %in% c("robust", "regularized", "gam", "robgam")) {
          stop("Error: A formula can only be used with the 'robust', 'regularized', 'gam', or 'robgam' methods.")
        }
      }
      
      method_var <- method[[var]]
      
      # Custom ranger median prediction
      use_median <- FALSE
      ranger_median <- FALSE
      if ("predict_median" %in% names(var_learner_params)) {
        if (isTRUE(var_learner_params$predict_median)) use_median <- TRUE
        var_learner_params$predict_median <- NULL
      }
      
      if (method_var == "ranger" && use_median) {
        ranger_median <- TRUE
      }
      
### ***** Select suitable learner Start ***** ###################################################################################################
      if(verbose){
        message(paste("***** Select Learner"))
      }
      if (is.numeric(data[[target_col]])) {
        learners_list <- list(
          regularized = list(learners[["regr.cv_glmnet"]], learners[["regr.glmnet"]]),
          robust = list(learners[["regr.lm_rob"]]),
          ranger = list(learners[["regr.ranger"]]),
          xgboost = list(learners[["regr.xgboost"]]),
          gam = list(learners[["regr.gam_imp"]]),
          robgam = list(learners[["regr.robgam_imp"]])
        )
      } else if (is.factor(data[[target_col]])) {
        learners_list <- list(
          regularized = list(learners[["classif.glmnet"]]),
          robust = list(learners[["classif.glm_rob"]]),
          ranger = list(learners[["classif.ranger"]]),
          xgboost = list(learners[["classif.xgboost"]]),
          gam = list(learners[["classif.gam_imp"]]),
          robgam = list(learners[["classif.robgam_imp"]])
        )
      } 
      learner_candidates <- learners_list[[method_var]]
      
### Select suitable learner End ***** ####
      
### *****OHE Start***** ###################################################################################################
      if(verbose){
        message(paste("***** OHE"))
      }
      data_temp <- enforce_factor_levels(data_temp, factor_levels)
      
      needs_ohe <- any(sapply(learner_candidates, function(lrn) {
        supports_factors <- "factor" %in% lrn$feature_types
        has_factors <- any(sapply(feature_cols, function(col) is.factor(data_temp[[col]])))
        #cat("Learner supports factors:", supports_factors, "\n")
        !supports_factors && has_factors
      }))
      
      # if selected formular false --> no ohe needed 
      if (!isFALSE(selected_formula)) {   #model.matrix: does ohe automatically in mle3
        #cat("Selected formula exists, no OHE needed.\n")
        needs_ohe <- FALSE
      }
      if(verbose){
        cat("needs_ohe:", needs_ohe, "\n")
      }
      #CONDITION FOR OHE
      # (1) At least one learner in learner_candidates does not support factors.
      # (2) At least one of the feature columns (feature_cols) is a factor.
      # Check whether the target is a categorical variable (factor) or numeric
      if (is.factor(data_temp[[target_col]])) {
        task_type <- "classif"
      } else {
        task_type <- "regr"
      }
      
      if (needs_ohe) {
        po_ohe <- po("encode", method = "one-hot")
        
        # OHE on data
        if (task_type == "regr") {
          train_dt <- data_temp[donor_ok_rows & !is.na(get(target_col))]
          train_task <- as_task_regr(train_dt, target = target_col)  
        } else {
          train_dt <- data_temp[donor_ok_rows & !is.na(get(target_col))]
          train_task <- as_task_classif(train_dt, target = target_col)  
        }
        
        po_ohe$train(list(train_task))  # Train Encoder
        
        # Apply the encoding to the full data; target must be non-NA for task creation
        pred_dt <- copy(data_temp)
        if (anyNA(pred_dt[[target_col]])) {
          if (task_type == "regr") {
            pred_dt[[target_col]][is.na(pred_dt[[target_col]])] <-
              median(pred_dt[[target_col]], na.rm = TRUE)
          } else {
            mode_value <- names(which.max(table(pred_dt[[target_col]], useNA = "no")))
            pred_dt[[target_col]][is.na(pred_dt[[target_col]])] <- mode_value
          }
        }
        if (task_type == "regr") {
          pred_task_ohe <- as_task_regr(pred_dt, target = target_col)
        } else {
          pred_task_ohe <- as_task_classif(pred_dt, target = target_col)
        }
        data_temp <- po_ohe$predict(list(pred_task_ohe))[[1]]$data()
      }

      effective_feature_count <- max(0L, ncol(data_temp) - 1L)
      if (method_var == "regularized" && effective_feature_count < 2L) {
        warning(sprintf(
          "Variable '%s' has fewer than two usable predictor columns after preprocessing; regularized (glmnet) requires at least two columns. Switching to 'robust'.",
          var
        ))
        learners <- ensure_robust_learners(learners)
        method_var <- "robust"
        learner_candidates <- if (is.numeric(data[[target_col]])) {
          list(learners[["regr.lm_rob"]])
        } else {
          list(learners[["classif.glm_rob"]])
        }
      }
      
### OHE End ###
      
### *****Create task Start***** ###################################################################################################
      if(verbose){
        message(paste("***** Create task"))
      }
      
      # ordered -> factor
      ordered_cols <- names(data_temp)[sapply(data_temp, inherits, "ordered")]
      if (length(ordered_cols) > 0) {
        data_temp[, (ordered_cols) := lapply(.SD, function(x) factor(as.character(x))), .SDcols = ordered_cols]
      }

      data_y_fill <- copy(data_temp)
      supports_missing <- all(sapply(learner_candidates, function(lrn) "missings" %in% lrn$properties))
      
      if (method_var == "ranger") {
        supports_missing <- FALSE
      }
      
      # TODO if xgboost will not be able to handle missings in target and $properties is not adjusted
      # if (method_var == "xgboost") {
      #   supports_missing <- FALSE
      # }
      
      # If NA in target variable --> only train with the data that has no NA in Y
      data_y_fill <- data_y_fill[donor_ok_rows & !is.na(get(target_col))]
      
      # If the learner does not support missing values -> use na.omit()
      data_y_fill_final <- if (supports_missing) data_y_fill else na.omit(data_y_fill)
      data_y_fill_final <- enforce_factor_levels(data_y_fill_final, factor_levels) 
      
      
      n_train <- nrow(data_y_fill_final)

      if (n_train == 0L) {
        warning(sprintf(
          "No observed values for target '%s' after donor filtering. Using simple fallback imputation.",
          target_col
        ))
        if (is.numeric(original_data[[target_col]])) {
          fill_val <- median(data[[target_col]], na.rm = TRUE)
        } else {
          fill_val <- names(which.max(table(data[[target_col]], useNA = "no")))
        }
        if (is.numeric(original_data[[target_col]]) && (!is.finite(fill_val) || is.na(fill_val))) {
          stop(sprintf("No finite fallback value available for target '%s'.", target_col))
        }
        if (!is.numeric(original_data[[target_col]]) && (is.null(fill_val) || length(fill_val) == 0L || is.na(fill_val))) {
          stop(sprintf("No fallback level available for target '%s'.", target_col))
        }
        data[missing_indices[[var]], (var) := fill_val]
        if (imp_var) {
          imp_col <- paste0(var, "_imp")
          if (!(imp_col %in% colnames(data_new))) {
            data_new[, (imp_col) := FALSE]
          }
          data_new[, (var) := data[[var]]]
          set(data_new, i = missing_indices[[var]], j = imp_col, value = TRUE)
        }
        next
      }

      if (method_var == "gam" && n_train < 2L) {
        warning(sprintf(
          "Too few observations (%d) to train GAM for '%s'. Falling back to 'robust'.",
          n_train, target_col
        ))
        learners <- ensure_robust_learners(learners)
        method_var <- "robust"
        learner_candidates <- if (is.numeric(data[[target_col]])) {
          list(learners[["regr.lm_rob"]])
        } else {
          list(learners[["classif.glm_rob"]])
        }
      }

      
      # Create task
      if (is.numeric(data_y_fill_final[[target_col]])) {
        task <- TaskRegr$new(id = target_col, backend = data_y_fill_final, target = target_col)
      } else if (is.factor(data_y_fill_final[[target_col]])) {
        task <- TaskClassif$new(id = target_col, backend = data_y_fill_final, target = target_col)
      } else {
        stop("Mistake: Target variable is neither numerical nor a factor!")
      }
      
### *****Create Learner Start***** ###################################################################################################
      if(verbose){
        message(paste("***** Create Learner"))
      }
      max_threads <- max(1L, future::availableCores() - 1L)
      if (nrow(data_y_fill_final) < 10000) {
        optimal_threads <- 1
      } else if (nrow(data_y_fill_final) < 100000) {
        optimal_threads <- max(1, max_threads %/% 2)
      } else {
        optimal_threads <- max_threads
      }
      # XGBoost Parameter Defaults
      xgboost_params <- list(
        nrounds = 100,
        max_depth = 3,
        eta = 0.1,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 1,
        verbose = 1,
        nthread = optimal_threads
      )
      
      ## XGBOOST
      if (method_var == "xgboost") {
        
        # All valid xgboost parameters in mlr3
        valid_xgb_params <- if (is.numeric(data[[target_col]])) {
          learners[["regr.xgboost"]]$param_set$ids()
        } else {
          learners[["classif.xgboost"]]$param_set$ids()
        }
        
        # Invalid parameters
        invalid <- setdiff(names(var_learner_params), valid_xgb_params)
        
        if (length(invalid) > 0) {
          warning(sprintf(
            "learner_params for variable '%s' contain invalid XGBoost parameters: %s. These parameters were ignored.",
            var, paste(invalid, collapse = ", ")
          ))
          var_learner_params <- var_learner_params[setdiff(names(var_learner_params), invalid)]
        }
        
        # Use valid parameters 
        xgboost_params <- modifyList(xgboost_params, var_learner_params)
        
        if (verbose) {
          cat("\n--- XGBoost params for variable", var, "---\n")
          print(xgboost_params)
        }
      }
      
      # Ranger Parameter Defaults
      ranger_params <- list(
        num.trees = 500,
        num.threads = optimal_threads
      )
      
      ## RANGER
      if (method_var == "ranger") {
        
        # All valid ranger params
        valid_ranger_params <- if (is.numeric(data[[target_col]])) {
          learners[["regr.ranger"]]$param_set$ids()
        } else {
          learners[["classif.ranger"]]$param_set$ids()
        }
        
        # Invalid parameters
        invalid <- setdiff(names(var_learner_params), valid_ranger_params)
        
        if (length(invalid) > 0) {
          warning(sprintf(
            "learner_params for variable '%s' contain invalid ranger parameters: %s. These parameters were ignored.",
            var, paste(invalid, collapse = ", ")
          ))
          var_learner_params <- var_learner_params[setdiff(names(var_learner_params), invalid)]
        }
        
        # Use parameter
        ranger_params <- modifyList(ranger_params, var_learner_params)
        
        if (verbose) {
          cat("\n--- Ranger params for variable", var, "---\n")
          print(ranger_params)
        }
      }
      
      ## REGULARIZED (glmnet)
      if (method_var == "regularized") {
        
        if (is.numeric(data[[target_col]])) {
          # ---- Regression glmnet ----
          valid_glmnet_params <- learners[["regr.glmnet"]]$param_set$ids()
          
          invalid <- setdiff(names(var_learner_params), valid_glmnet_params)
          if (length(invalid) > 0) {
            warning(sprintf(
              "learner_params for variable '%s' contain invalid glmnet regression parameters: %s. They were ignored.",
              var, paste(invalid, collapse = ", ")
            ))
            var_learner_params <- var_learner_params[setdiff(names(var_learner_params), invalid)]
          }
          
          glmnet_params <- var_learner_params
          
          if (verbose) {
            cat("\n--- glmnet regression params for variable", var, "---\n")
            print(glmnet_params)
          }
          
        } else {
          # ---- Classification glmnet ----
          valid_glmnet_params <- learners[["classif.glmnet"]]$param_set$ids()
          
          invalid <- setdiff(names(var_learner_params), valid_glmnet_params)
          if (length(invalid) > 0) {
            warning(sprintf(
              "learner_params for variable '%s' contain invalid glmnet classification parameters: %s. They were ignored.",
              var, paste(invalid, collapse = ", ")
            ))
            var_learner_params <- var_learner_params[setdiff(names(var_learner_params), invalid)]
          }
          
          glmnet_params <- var_learner_params
          
          if (verbose) {
            cat("\n--- glmnet classification params for variable", var, "---\n")
            print(glmnet_params)
          }
        }
      }
      
      ## ROBUST (lm_rob / glm_rob)
      if (method_var == "robust") {
        
        # Regression vs Classification
        if (is.numeric(data[[target_col]])) {
          valid_robust_params <- learners[["regr.lm_rob"]]$param_set$ids()
        } else {
          valid_robust_params <- learners[["classif.glm_rob"]]$param_set$ids()
        }
        
        # Invalid parameters
        invalid <- setdiff(names(var_learner_params), valid_robust_params)
        
        if (length(invalid) > 0) {
          warning(sprintf(
            "learner_params for variable '%s' contain invalid robust-model parameters: %s. These parameters were ignored.",
            var, paste(invalid, collapse = ", ")
          ))
          var_learner_params <- var_learner_params[setdiff(names(var_learner_params), invalid)]
        }
        
        robust_params <- var_learner_params
        
        if (verbose) {
          cat("\n--- robust params for variable", var, "---\n")
          print(robust_params)
        }
      }

      ## GAM (mgcv-based)
      gam_params <- list()
      if (method_var == "gam") {
        valid_gam_params <- learners[["regr.gam_imp"]]$param_set$ids()
        invalid <- setdiff(names(var_learner_params), valid_gam_params)
        if (length(invalid) > 0) {
          warning(sprintf(
            "learner_params for variable '%s' contain invalid GAM parameters: %s. These parameters were ignored.",
            var, paste(invalid, collapse = ", ")
          ))
          var_learner_params <- var_learner_params[setdiff(names(var_learner_params), invalid)]
        }
        gam_params <- var_learner_params
        if (verbose) {
          cat("\n--- GAM params for variable", var, "---\n")
          print(gam_params)
        }
      }

      ## ROBGAM (robust GAM)
      robgam_params <- list()
      if (method_var == "robgam") {
        valid_robgam_params <- learners[["regr.robgam_imp"]]$param_set$ids()
        invalid <- setdiff(names(var_learner_params), valid_robgam_params)
        if (length(invalid) > 0) {
          warning(sprintf(
            "learner_params for variable '%s' contain invalid robGAM parameters: %s. These parameters were ignored.",
            var, paste(invalid, collapse = ", ")
          ))
          var_learner_params <- var_learner_params[setdiff(names(var_learner_params), invalid)]
        }
        robgam_params <- var_learner_params
        if (verbose) {
          cat("\n--- robGAM params for variable", var, "---\n")
          print(robgam_params)
        }
      }
      
      is_regr_task <- is.numeric(data_y_fill_final[[target_col]])
      measure <- if (is_regr_task) msr("regr.rmse") else msr("classif.acc")

      cv_folds <- safe_cv_folds(task, 5L)
      if (length(learner_candidates) > 1 && !is.na(cv_folds)) {
        resample_results <- lapply(learner_candidates, function(lrn) {
          
          if (grepl("xgboost", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, xgboost_params)
          } else if (grepl("ranger", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, ranger_params)
          } else if (grepl("glmnet", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, glmnet_params)
          } else if (grepl("lm_rob|glm_rob", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, robust_params)
          } else if (grepl("robgam_imp", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, robgam_params)
          } else if (grepl("gam_imp", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, gam_params)
          }
          
          # Classification: probabilistic if available
          if (!is_regr_task) {
            if ("prob" %in% lrn$predict_types) {
              lrn$predict_type <- "prob"
            } else {
              lrn$predict_type <- "response"
            }
          }
          resample(task, lrn, rsmp("cv", folds = cv_folds))
        })
        
        scores <- sapply(resample_results, function(res) res$aggregate(measure))
        if (is_regr_task) {
          best_learner <- learner_candidates[[which.min(scores)]]   # RMSE: min
        } else {
          best_learner <- learner_candidates[[which.max(scores)]]   # ACC: max
        }

      } else {
        best_learner <- learner_candidates[[1]]
      }
      
      # Initialize learner and set parameters
      learner_obj <- lrn(best_learner$id)
      default_learner <- learner_obj$clone(deep = TRUE)
      current_learner <- learner_obj$clone(deep = TRUE)
      best_learner    <- learner_obj$clone(deep = TRUE)
      tuned_learner   <- learner_obj$clone(deep = TRUE)
      
      # Set parameters for the best learner
      if (grepl("xgboost", best_learner$id)) {
        best_learner$param_set$values <- modifyList(best_learner$param_set$values, xgboost_params)
        default_learner$param_set$values <- modifyList(default_learner$param_set$values, xgboost_params)
        current_learner$param_set$values <- modifyList(current_learner$param_set$values, xgboost_params)
        tuned_learner$param_set$values <- modifyList(tuned_learner$param_set$values, xgboost_params)
      } else if (grepl("ranger", best_learner$id)) {
        best_learner$param_set$values <- modifyList(best_learner$param_set$values, ranger_params)
        default_learner$param_set$values <- modifyList(default_learner$param_set$values, ranger_params)
        current_learner$param_set$values <- modifyList(current_learner$param_set$values, ranger_params)
        tuned_learner$param_set$values <- modifyList(tuned_learner$param_set$values, ranger_params)
      } else if (grepl("glmnet", best_learner$id)) {
        best_learner$param_set$values   <- modifyList(best_learner$param_set$values, glmnet_params)
        default_learner$param_set$values <- modifyList(default_learner$param_set$values, glmnet_params)
        current_learner$param_set$values <- modifyList(current_learner$param_set$values, glmnet_params)
        tuned_learner$param_set$values   <- modifyList(tuned_learner$param_set$values, glmnet_params)
      } else if (grepl("lm_rob|glm_rob", best_learner$id)) {
        best_learner$param_set$values   <- modifyList(best_learner$param_set$values, robust_params)
        default_learner$param_set$values <- modifyList(default_learner$param_set$values, robust_params)
        current_learner$param_set$values <- modifyList(current_learner$param_set$values, robust_params)
        tuned_learner$param_set$values   <- modifyList(tuned_learner$param_set$values, robust_params)
      } else if (grepl("robgam_imp", best_learner$id)) {
        best_learner$param_set$values   <- modifyList(best_learner$param_set$values, robgam_params)
        default_learner$param_set$values <- modifyList(default_learner$param_set$values, robgam_params)
        current_learner$param_set$values <- modifyList(current_learner$param_set$values, robgam_params)
        tuned_learner$param_set$values   <- modifyList(tuned_learner$param_set$values, robgam_params)
      } else if (grepl("gam_imp", best_learner$id)) {
        best_learner$param_set$values   <- modifyList(best_learner$param_set$values, gam_params)
        default_learner$param_set$values <- modifyList(default_learner$param_set$values, gam_params)
        current_learner$param_set$values <- modifyList(current_learner$param_set$values, gam_params)
        tuned_learner$param_set$values   <- modifyList(tuned_learner$param_set$values, gam_params)
      }
      
      if (is.factor(data_temp[[target_col]])) {
        best_learner$predict_type <- "prob"
        default_learner$predict_type <- "prob"
        current_learner$predict_type <- "prob"
        tuned_learner$predict_type <- "prob"
      }
      
### Create Learner End ### 
      
### ***** Hyperparameter Start ***** ###################################################################################################
      if (verbose) {
        message("***** Parametertuning")
      }
      
      # Debug: conditions 
      # if (verbose) {
      #   cat(sprintf("~ tuning_condition: !tuned=%s | tune[%s]=%s | i=%d | mid=%d\n",
      #               (!tuning_status[[var]]), var, isTRUE(tune[[var]]), i, round(nseq/2)))
      # }
      
      # Tune each variable once, at a guaranteed-reachable iteration. The old
      # trigger i == round(nseq / 2) never fired when sequential = FALSE (nseq is
      # forced to 1 and round(1/2) == 0, but i starts at 1), so tune = TRUE was a
      # silent no-op. min(2, nseq) tunes on once-updated predictors when possible
      # and always fires for nseq <= 2; tuning_status keeps it to a single run.
      if (!tuning_status[[var]] && isTRUE(tune[[var]]) && i >= min(2L, nseq)) {
        
        best_learner_id <- best_learner$id
        ss <- build_vimpute_search_space(best_learner_id, task)
        search_space <- ss$space
        n_evals      <- ss$n_evals
        
        if (verbose) {
          cat("is.null(search_space) = ", is.null(search_space), "\n", sep = "")
        }
        
        # No Search Space → Skip Tuning + Default
        if (is.null(search_space)) {
          warning(sprintf("No search space defined for learner '%s'. Skipping tuning.", best_learner_id))
          tuning_status[[var]] <- TRUE
          if (is.null(hyperparameter_cache[[var]])) {
            hyperparameter_cache[[var]] <- list(params = default_learner$param_set$values, is_tuned = FALSE)
          }
          
        } else {
          # Classification: too less classes → Skip Tuning + Default
          if (task$task_type == "classif") {
            class_counts <- table(task$truth())
            if (any(class_counts < 2)) {
              warning(sprintf("Too few samples per class to tune '%s'. Skipping tuning.", var))
              tuning_status[[var]] <- TRUE
              if (is.null(hyperparameter_cache[[var]])) {
                hyperparameter_cache[[var]] <- list(params = default_learner$param_set$values, is_tuned = FALSE)
              }
            } else {
              # Tuning
              tryCatch({
                # XGBoost: nrounds-Default 
                if (best_learner_id %in% c("classif.xgboost", "regr.xgboost")) {
                  if (is.null(default_learner$param_set$values$nrounds)) {
                    default_learner$param_set$values$nrounds <- 100L
                  }
                }
                
                # Resample
                folds <- safe_cv_folds(task, if (task$nrow <= 3000) 5L else 3L)
                if (is.na(folds)) {
                  stop("Too few usable observations per fold for cross-validation.")
                }
                resampling <- rsmp("cv", folds = folds)
                resampling$instantiate(task)
                
                # Measure
                msr_obj <- if (task$task_type == "regr") msr("regr.rmse") else msr("classif.acc")
                
                # Tuning-Instance
                instance <- TuningInstanceBatchSingleCrit$new(
                  task         = task,
                  learner      = best_learner,
                  resampling   = resampling,
                  measure      = msr_obj,
                  search_space = search_space,
                  terminator   = trm("evals", n_evals = n_evals)
                )
                
                # Tuner
                # batch_size = 1 keeps random-search RNG consumption identical
                # across machines, so seed = ... reproduces tuning everywhere
                # (detectCores() - 1 made tuned parameters machine-dependent).
                tuner <- tnr("random_search", batch_size = 1L)
                tuner$optimize(instance)
                
                # Best Parameters
                best_params <- as.list(instance$result[, get("learner_param_vals")][[1]])
                tuned_learner$param_set$values <- best_params
                
                # Compare tuned vs default
                resampling1 <- rsmp("cv", folds = folds); resampling1$instantiate(task)
                default_result <- resample(task, default_learner, resampling1)
                
                resampling2 <- rsmp("cv", folds = folds); resampling2$instantiate(task)
                tuned_result <- resample(task, tuned_learner, resampling2)
                
                default_metric <- default_result$aggregate(msr_obj)
                tuned_metric   <- tuned_result$aggregate(msr_obj)
                
                if (is.na(default_metric) || is.na(tuned_metric)) {
                  warning(sprintf("Tuning metric is NA for '%s'. Using default parameters.", var))
                  use_tuned <- FALSE
                } else {
                  use_tuned <- if (task$task_type == "regr") (tuned_metric < default_metric) else (tuned_metric > default_metric)
                }
                
                if (use_tuned) {
                  current_learner$param_set$values <- best_params
                  count_tuned_better <- count_tuned_better + 1
                  hyperparameter_cache[[var]] <- list(params = best_params, is_tuned = TRUE)
                  if (verbose) {
                    cat(sprintf("Tuned parameters for variable '%s': %s\n",
                                var, paste(names(best_params), best_params, sep = "=", collapse = ", ")))
                    flush.console()
                  }
                } else {
                  current_learner$param_set$values <- default_learner$param_set$values
                  count_default_better <- count_default_better + 1
                  hyperparameter_cache[[var]] <- list(params = default_learner$param_set$values, is_tuned = FALSE)
                  if (verbose) {
                    cat(sprintf("Default parameters for variable '%s': %s\n",
                                var, paste(names(default_learner$param_set$values),
                                           default_learner$param_set$values, sep = "=", collapse = ", ")))
                    flush.console()
                  }
                }
                
                if (verbose) {
                  cat("\n----- Optimal parameters for variable '", var, "' -----\n", sep = "")
                  final_params <- current_learner$param_set$values
                  if (length(final_params) == 0) cat("[No tunable parameters]\n")
                  for (nm in names(final_params)) {
                    cat(sprintf("%s = %s\n", nm, as.character(final_params[[nm]])))
                  }
                  cat("---------------------------------------------\n\n")
                }
                
                # Tuning was done
                tuning_status[[var]] <- TRUE
                
              }, error = function(e) {
                warning(sprintf("Tuning failed for variable '%s': %s. Using default parameters.", var, e$message))
                current_learner$param_set$values <- default_learner$param_set$values
                tuning_status[[var]] <- "failed"
                hyperparameter_cache[[var]] <- list(params = default_learner$param_set$values, is_tuned = FALSE)
              })
      
            }
          } else {
            # Regression: tune directly 
            tryCatch({
              if (best_learner_id %in% c("classif.xgboost", "regr.xgboost")) {
                if (is.null(default_learner$param_set$values$nrounds)) {
                  default_learner$param_set$values$nrounds <- 100L
                }
              }
              folds <- safe_cv_folds(task, if (task$nrow <= 3000) 5L else 3L)
              if (is.na(folds)) {
                stop("Too few usable observations for cross-validation.")
              }
              resampling <- rsmp("cv", folds = folds); resampling$instantiate(task)
              msr_obj <- msr("regr.rmse")
              
              instance <- TuningInstanceBatchSingleCrit$new(
                task         = task,
                learner      = best_learner,
                resampling   = resampling,
                measure      = msr_obj,
                search_space = search_space,
                terminator   = trm("evals", n_evals = n_evals)
              )
              
              # batch_size = 1: machine-independent tuning (see comment above)
              tuner <- tnr("random_search", batch_size = 1L)
              tuner$optimize(instance)
              
              best_params <- as.list(instance$result[, get("learner_param_vals")][[1]])
              tuned_learner$param_set$values <- best_params
              
              res1 <- rsmp("cv", folds = folds); res1$instantiate(task)
              default_result <- resample(task, default_learner, res1)
              
              res2 <- rsmp("cv", folds = folds); res2$instantiate(task)
              tuned_result <- resample(task, tuned_learner, res2)
              
              default_metric <- default_result$aggregate(msr_obj)
              tuned_metric   <- tuned_result$aggregate(msr_obj)
              use_tuned <- (!is.na(default_metric) && !is.na(tuned_metric) && tuned_metric < default_metric)
              
              if (use_tuned) {
                current_learner$param_set$values <- best_params
                count_tuned_better <- count_tuned_better + 1
                hyperparameter_cache[[var]] <- list(params = best_params, is_tuned = TRUE)
                if (verbose) {
                  cat(sprintf("Tuned parameters for variable '%s': %s\n",
                              var, paste(names(best_params), best_params, sep = "=", collapse = ", ")))
                  flush.console()
                }
              } else {
                current_learner$param_set$values <- default_learner$param_set$values
                count_default_better <- count_default_better + 1
                hyperparameter_cache[[var]] <- list(params = default_learner$param_set$values, is_tuned = FALSE)
                if (verbose) {
                  cat(sprintf("Default parameters for variable '%s': %s\n",
                              var, paste(names(default_learner$param_set$values),
                                         default_learner$param_set$values, sep = "=", collapse = ", ")))
                  flush.console()
                }
              }
              
              if (verbose) {
                cat("\n----- Optimal parameters for variable '", var, "' -----\n", sep = "")
                final_params <- current_learner$param_set$values
                if (length(final_params) == 0) cat("[No tunable parameters]\n")
                for (nm in names(final_params)) cat(sprintf("%s = %s\n", nm, as.character(final_params[[nm]])))
                cat("---------------------------------------------\n\n")
              }
              
              tuning_status[[var]] <- TRUE
              
            }, error = function(e) {
              warning(sprintf("Tuning failed for variable '%s': %s. Using default parameters.", var, e$message))
              current_learner$param_set$values <- default_learner$param_set$values
              tuning_status[[var]] <- "failed"
              hyperparameter_cache[[var]] <- list(params = default_learner$param_set$values, is_tuned = FALSE)
            })
          }
        }
        
        # Restore the plan that was active at entry instead of forcing
        # "sequential" -- forcing it clobbered the user's future::plan for the
        # rest of the call (entry/exit save-restore only covers after return).
        future::plan(old_plan)

      } else if (tuning_status[[var]]) {
        # Already tuned
        if (!is.null(hyperparameter_cache[[var]]$params)) {
          current_learner$param_set$values <- hyperparameter_cache[[var]]$params
        }
      }
      
      # Tuning-Log
      tuned_flag <- FALSE
      if (!is.null(hyperparameter_cache[[var]]) && !is.null(hyperparameter_cache[[var]]$is_tuned)) {
        tuned_flag <- isTRUE(hyperparameter_cache[[var]]$is_tuned)
      }
      tuning_log[[length(tuning_log) + 1]] <- list(
        variable     = var,
        tuned        = !isFALSE(tuning_status[[var]]),   # tuning actually executed
        tuned_better = tuned_flag,                       # tuned params beat defaults
        params       = hyperparameter_cache[[var]]$params # chosen parameters (reusable
                                                          # via the tuned_params argument)
      )

      # if (!tuning_status[[var]] && nseq >= 2 && isTRUE(tune[[var]])) {
      #   
      #   if ((nseq > 2 && i == round(nseq / 2)) || (nseq == 2 && i == 2)) {
      #     tuner = tnr("random_search")
      #     p = length(task$feature_names)
      #     
      #     search_spaces <- list(
      #       "regr.cv_glmnet" = ps(alpha = p_dbl(0, 1), nfolds = p_int(7, 20)),  
      #       "regr.glmnet" = ps(alpha = p_dbl(0, 1), lambda = p_dbl(10^-4, 10^2, logscale = TRUE)),
      #       "classif.glmnet" = ps(alpha = p_dbl(0, 1), lambda = p_dbl(10^-4, 10^2, logscale = TRUE)),
      #       # alpha = mixture between lasso (1) and ridge (0)
      #       # lambda = the larger the stronger the regulation
      #       
      #       # Ranger
      #       "regr.ranger" = ps(num.trees = p_int(500, 700 ,default = 500), min.node.size = p_int(3, 10, default=5), sample.fraction = p_dbl(0.8,1)),
      #       "classif.ranger" = ps(num.trees = p_int(500, 700), min.node.size = p_int(3, 10), sample.fraction = p_dbl(0.8,1)),
      #       # min.node.size = minimum node size 
      #       # sample.fraction = proportion of data sampled per tree
      #       
      #       # XGBoost
      #       "regr.xgboost" = ps(nrounds = p_int(100,500), eta = p_dbl(0.01, 0.3), max_depth = p_int(3, 9),  colsample_bytree = p_dbl(0.7, 0.9)),
      #       "classif.xgboost" = ps(nrounds = p_int(100,500), eta = p_dbl(0.01, 0.3), max_depth = p_int(3, 9), subsample = p_dbl(0.7, 0.9), colsample_bytree = p_dbl(0.7, 0.9)),
      #       # eta = learning rate, low values --> more stable but slower models
      #       # max_depth = Maximum tree depth, large values --> more complex patterns but possibly overfitting
      #       # subsample = proportion of data used per boosting iteration | colsample_bytree = proportion of features used per tree          
      #       
      #       # Robust Models
      #       "regr.lm_rob" = ps(tuning.chi = p_dbl(1.2,1.8), tuning.psi = p_dbl(1.2,1.8),  max.it = p_int(60,300)), #psi = p_fct(c("bisquare", "optimal")),
      #       "classif.glm_rob" = ps(method = p_fct(c("Mqle", "WBY")), acc = p_dbl(0, 0.1), test.acc = p_fct(c("coef", "resid")), tcc = p_dbl(1,2), maxit = p_int(30,300))
      #       # psi = function for weighting the residuals 
      #       # acc = tolerance for the convergence of the algorithm | test.acc = criterion for the convergence test | tcc = tuning constant
      #     )
      #     
      #     best_learner_id <- best_learner$id  
      #     #best_learner = learners[[best_learner_id]]
      #     
      #     search_space <- search_spaces[[best_learner_id]]
      #     if (is.null(search_space)) {
      #       warning(sprintf("No search space defined for learner '%s'. Skipping tuning.", best_learner_id))
      #       tuning_status[[var]] <- TRUE   # mark as 'tuning attempted'
      #       next
      #     }
      #     
      #     #future::plan("multisession") 
      #     
      #     tryCatch({
      #       # train default model
      #       if (best_learner_id == "classif.xgboost" || best_learner_id == "regr.xgboost") {
      #         default_learner$param_set$values$nrounds = 100  # Set a default value for nrounds
      #       }
      #       
      #       resampling = rsmp("cv", folds = 5)
      #       resampling$instantiate(task)
      #       
      #       # Tuning-Instance
      #       instance = TuningInstanceBatchSingleCrit$new(
      #         task = task,  
      #         learner = best_learner,
      #         resampling = resampling,
      #         # resampling = rsmp("cv", folds = 5,repeats = 3),
      #         measure = if (task$task_type == "regr") msr("regr.rmse") else msr("classif.acc"),
      #         search_space = search_space,
      #         terminator = trm("evals", n_evals = 20)
      #       )
      #       
      #       # tuning
      #       batch <- max(1, parallel::detectCores() - 1)
      #       tuner <- tnr("random_search", batch_size = batch)
      #       tuner$optimize(instance)
      #       
      #       # save best parameters
      #       best_params <- as.list(instance$result[, get("learner_param_vals")][[1]])
      #       tuning_status[[var]] <- TRUE
      #       
      #       # compare with default
      #       tuned_learner$param_set$values <- best_params
      #       
      #       resampling1 = rsmp("cv", folds = 5)
      #       resampling1$instantiate(task)
      #       default_result <- resample(task, default_learner, resampling1)
      #       
      #       resampling2 = rsmp("cv", folds = 5)
      #       resampling2$instantiate(task)
      #       tuned_result <- resample(task, tuned_learner, resampling2)
      #       
      #       # which model is better
      #       if (task$task_type == "regr") {
      #         if (tuned_result$aggregate(msr("regr.rmse")) < default_result$aggregate(msr("regr.rmse"))) {
      #           current_learner$param_set$values <- best_params
      #           count_tuned_better <- count_tuned_better + 1
      #           
      #           hyperparameter_cache[[var]] <- list(
      #             params = best_params,
      #             is_tuned = TRUE
      #           )
      #           
      #           if (verbose) {
      #             cat(sprintf("Tuned parameters for variable '%s': %s\n", var, paste(names(best_params), best_params, sep = "=", collapse = ", ")))
      #             flush.console()
      #           }
      #           
      #         } else {
      #           current_learner$param_set$values <- default_learner$param_set$values
      #           count_default_better <- count_default_better + 1
      #           hyperparameter_cache[[var]] <- list(
      #             params = default_learner$param_set$values,
      #             is_tuned = FALSE
      #           )
      #           if (verbose) {
      #             cat(sprintf("Default parameters for variable '%s': %s", var, paste(names(default_learner$param_set$values), default_learner$param_set$values, sep = "=", collapse = ", ")))
      #             flush.console()
      #           }
      #           
      #         }
      #         
      #       } else {
      #         if (tuned_result$aggregate(msr("classif.acc")) > default_result$aggregate(msr("classif.acc"))) {
      #           current_learner$param_set$values <- best_params
      #           count_tuned_better <- count_tuned_better + 1
      #           
      #           hyperparameter_cache[[var]] <- list(
      #             params = best_params,
      #             is_tuned = TRUE
      #           )
      #           if (verbose) {
      #             cat(sprintf("Tuned parameters for variable '%s': %s\n", var, paste(names(best_params), best_params, sep = "=", collapse = ", ")))
      #             flush.console()
      #           }
      #           
      #         } else {
      #           current_learner$param_set$values <- default_learner$param_set$values
      #           count_default_better <- count_default_better + 1
      #           hyperparameter_cache[[var]] <- list(
      #             params = default_learner$param_set$values,
      #             is_tuned = FALSE
      #           )
      #           if (verbose) {
      #             cat(sprintf("Default parameters for variable '%s': %s", var, paste(names(default_learner$param_set$values), default_learner$param_set$values, sep = "=", collapse = ", ")))
      #             flush.console()
      #           }
      #         }
      #       }
      #       
      #     }, error = function(e) {
      #       warning(sprintf("Tuning failed for variable '%s': %s. Using default parameters.", var, e$message))
      #       current_learner$param_set$values <- default_learner$param_set$values
      #       tuning_status[[var]] <- FALSE
      #       hyperparameter_cache[[var]] <- list(
      #         params = default_learner$param_set$values,
      #         is_tuned = FALSE
      #       )
      #     })
      #     
      #     future::plan("sequential")
      #   } else if (tuning_status[[var]]) {
      #     # Use cached parameters
      #     current_learner$param_set$values <- hyperparameter_cache[[var]]$params
      #   }
      # }
      # 
      # # tuning_log
      # tuning_log[[length(tuning_log) + 1]] <- list(
      #   variable = var,
      #   tuned_better = isTRUE(hyperparameter_cache[[var]]$is_tuned)
      # )

### Hyperparameter End ###
      
### ***** NAs Start***** ###################################################################################################
      if(verbose){
        message(paste("***** NAs in feature variables bearbeiten"))
      }
      if (method_var == "xgboost") {
        po_x_miss <- NULL  
        
      } else if (supports_missing) {
        if (sum(is.na(data_temp)) > 0)  {  #task$data_temp()
          po_x_miss <- po("missind", param_vals = list(
            affect_columns = mlr3pipelines::selector_all(),
            which = "all",
            type = "factor"
          ))
        } else {
          po_x_miss <- NULL  
        }
        
      } 
### NAs End ###     
      
### *****Train Model Start***** ###################################################################################################
      if(verbose){
        message("***** Train Model")
      }
      
      ordered_cols <- names(data_temp)[sapply(data_temp, inherits, "ordered")]
      if (length(ordered_cols) > 0) {
        data_temp[, (ordered_cols) := lapply(.SD, function(x) factor(as.character(x))), .SDcols = ordered_cols]
      }
      
      ordered_cols_final <- names(data_y_fill_final)[sapply(data_y_fill_final, inherits, "ordered")]
      if (length(ordered_cols_final) > 0) {
        data_y_fill_final[, (ordered_cols_final) := lapply(.SD, function(x) factor(as.character(x))), .SDcols = ordered_cols_final]
      }
      
      # Check semicontinous
      is_sc <- is_semicontinuous(data_temp[[var]])
      
      if (is_sc && method_var == "regularized") {
        warning(sprintf("Variable '%s' is semikontinuous; regularized (glmnet) is unstable here. Switching to 'robust'.", var))
        learners <- ensure_robust_learners(learners)
        method_var <- "robust"
        robust_learner_id <- if (is.numeric(data[[target_col]])) "regr.lm_rob" else "classif.glm_rob"
        best_learner <- lrn(robust_learner_id)
        current_learner <- best_learner$clone(deep = TRUE)
        default_learner <- best_learner$clone(deep = TRUE)
        tuned_learner <- best_learner$clone(deep = TRUE)
        if (is.numeric(data[[target_col]])) {
          robust_params <- var_learner_params[intersect(names(var_learner_params), best_learner$param_set$ids())]
          best_learner$param_set$values <- modifyList(best_learner$param_set$values, robust_params)
          current_learner$param_set$values <- modifyList(current_learner$param_set$values, robust_params)
          default_learner$param_set$values <- modifyList(default_learner$param_set$values, robust_params)
          tuned_learner$param_set$values <- modifyList(tuned_learner$param_set$values, robust_params)
        }
      }
      
      if (is_sc) {
        
        reserve_level <- ".__IMPUTEOOR_NEW__"  # important if there are new levels in prediction compared to training
        
        # Prepare factorlevels  -> important that train and pred are working with same factorlevels
        for (col in setdiff(names(data_temp), c(var))) {
          if (is.factor(data_temp[[col]])) {
            factor_levels[[col]] <- unique(c(levels(data_temp[[col]]), reserve_level))
            data_temp[[col]] <- factor(data_temp[[col]], levels = factor_levels[[col]])
          }
        }
        
        # Zero-Flag adding -> to check if regression is necessary
        zero_flag_col <- paste0(var, "_zero_flag")
        factor_levels[[zero_flag_col]] <- c("zero", "positive")
        data_temp[[zero_flag_col]] <- factor(
          ifelse(data_temp[[var]] == 0, "zero", "positive"),
          levels = factor_levels[[zero_flag_col]]
        )
        
        new_cols <- setdiff(names(data_temp), names(tuning_status))
        for (col in new_cols) {
          tuning_status[[col]] <- FALSE
        }

        # Features for classification
        relevant_features <- setdiff(names(data_temp), c(var, zero_flag_col))
        if (length(relevant_features) == 0) stop("No relevant features for classification for ", var)
        
        # Prepare classification data 
        class_data <- data_temp[donor_ok_rows & !is.na(data_temp[[var]]), c(relevant_features, var, zero_flag_col), with = FALSE]
        if (nrow(class_data) == 0) stop("No rows left for classification for ", var)
        
        # Harmonize factor-levels -> no new levels and no lost levels
        class_data <- enforce_factor_levels(class_data, factor_levels)
        check_all_factor_levels(class_data, factor_levels)
        
        # Before training: make sure all features have right levels
        for (col in relevant_features) {
          if (is.factor(class_data[[col]])) {
            class_data[[col]] <- factor(class_data[[col]], levels = factor_levels[[col]])
          }
        }
        
        # Dummy rows for missing levels -> every level has to be present (min one time)
        class_data <- ensure_dummy_rows_for_factors(
          dt = class_data,
          target_col = zero_flag_col
        )

        # Heuristic: Ranger vs. LogReg
        use_ranger <- needs_ranger_classif(
          y = class_data[[zero_flag_col]],
          X = class_data[, relevant_features, with = FALSE]
        )
        
        classif_learner <- if (use_ranger) {
          lrn("classif.ranger")   # kann missings (property "missings")
        } else {
          lrn("classif.log_reg")  # kann KEINE missings
        }
        supports_missing_cls <- "missings" %in% classif_learner$properties
        
        if (!supports_missing_cls) {
          # e.g. log_reg: no nas allowed
          na_rows <- !complete.cases(subset(class_data, select = relevant_features))
          if (any(na_rows)) {
            class_data <- class_data[!na_rows]
          }
          if (nrow(class_data) == 0) {
            stop("No rows left for classification after NA handling for ", var)
          }
          
          # Levels
          class_data <- enforce_factor_levels(class_data, factor_levels)
          check_all_factor_levels(class_data, factor_levels)
        }
        
        # Classification Task & Pipeline
        class_task <- TaskClassif$new(
          id = paste0(zero_flag_col, "_task"),
          backend = class_data,
          target = zero_flag_col
        )
        class_task$select(relevant_features)
        
        po_fix <- po("fixfactors", droplevels = FALSE) # no level drops
        po_oor <- po("imputeoor",
                     affect_columns = mlr3pipelines::selector_type("factor"),
                     create_empty_level = TRUE) # new levels -> reserve levels
        
        class_pipeline <- po_fix %>>% po_oor %>>% classif_learner
        class_learner <- GraphLearner$new(class_pipeline)
        class_learner$predict_type <- "prob"
        
        # Train
        class_learner$train(class_task)
        
        # Regression-Learner 
        regr_learner_id <- best_learner$id
        regr_learner <- lrn(regr_learner_id)
        
        if (grepl("xgboost", best_learner$id)) {
          regr_learner$param_set$values <- modifyList(regr_learner$param_set$values, xgboost_params)
        } else if (grepl("ranger", best_learner$id)) {
          regr_learner$param_set$values <- modifyList(regr_learner$param_set$values, ranger_params)
        }
        
        # Hyperparameter-Cache for classification
        if (isTRUE(tuning_status[[var]]) && !is.null(tuning_status[[zero_flag_col]]) && isTRUE(tuning_status[[zero_flag_col]])) {
          if (!is.null(hyperparameter_cache[[zero_flag_col]]) && isTRUE(hyperparameter_cache[[zero_flag_col]]$is_tuned)) {
            params <- hyperparameter_cache[[zero_flag_col]]$params
            
            # without prefix
            pipeline_valid <- intersect(names(params), class_pipeline$param_set$ids())
            class_pipeline$param_set$values <- modifyList(class_pipeline$param_set$values, params[pipeline_valid])
            
            # with prefix
            prefixed_names <- paste0(best_learner$id, ".", names(params))
            learner_valid <- prefixed_names %in% class_learner$param_set$ids()
            if (any(learner_valid)) {
              prefixed_params <- setNames(params[learner_valid], prefixed_names[learner_valid])
              class_learner$param_set$values <- modifyList(class_learner$param_set$values, prefixed_params)
            }
            
            # warning if missing variables 
            missing_in_pipeline <- setdiff(names(params), class_pipeline$param_set$ids())
            missing_in_learner <- setdiff(names(params), 
                                          sub(paste0("^", best_learner$id, "\\."), "", 
                                              class_learner$param_set$ids()[startsWith(class_learner$param_set$ids(), best_learner$id)]))
            if (length(missing_in_pipeline) > 0) warning("Missing in Pipeline (classification): ", paste(missing_in_pipeline, collapse = ", "))
            if (length(missing_in_learner) > 0) warning("Missing in Learner (classification): ", paste(missing_in_learner, collapse = ", "))
          }
        }
        
        # Predict-Type classification
        if ("prob" %in% class_learner$predict_types) {
          class_learner$predict_type <- "prob"
        } else {
          class_learner$predict_type <- "response"
          warning(sprintf("predict_type 'prob' not supported by learner '%s'; fallback to 'response'", class_learner$id))
        }
        
        # 2) Regression
        reg_data <- data_temp[donor_ok_rows & data_temp[[var]] > 0,]  # only positive values
        # Same features as classification
        reg_features <- relevant_features
        # Regression without NA in target
        reg_data <- reg_data[!is.na(reg_data[[var]]), ]
        
        # Harmonize factor-levels
        reg_data <- enforce_factor_levels(reg_data, factor_levels)
        check_all_factor_levels(reg_data, factor_levels)
        
        has_na_in_features <- any(sapply(reg_features, function(cn) anyNA(reg_data[[cn]])))
        
        # Does Regressions-Learner support missings?
        supports_missing <- "missings" %in% regr_learner$properties
        if (method_var == "ranger") supports_missing <- FALSE
        
        po_x_miss_reg <- NULL
        if (has_na_in_features && supports_missing && method_var != "xgboost") {
          po_x_miss_reg <- po("missind", param_vals = list(
            affect_columns = mlr3pipelines::selector_name(reg_features),
            which = "all",
            type = "factor"
          ))
        }
        
        if (has_na_in_features && !supports_missing) {
          cols <- c(reg_features, var)
          reg_data <- na.omit(subset(reg_data, select = cols))
          reg_data <- enforce_factor_levels(reg_data, factor_levels)
          check_all_factor_levels(reg_data, factor_levels)
        }
        
        if (nrow(reg_data) == 0) {
          warning("reg_data empty after NA handling for ", var, " - skipping regressor.")
          reg_learner <- NULL
        } else {
          # Task
          reg_task <- TaskRegr$new(id = var, backend = reg_data, target = var)
          reg_task$select(reg_features)
          # Pipeline
          reg_pipeline <- if (!is.null(po_x_miss_reg)) {
            po_x_miss_reg %>>% regr_learner
          } else {
            regr_learner
          }
          
          reg_learner <- GraphLearner$new(reg_pipeline)
          
          # Train
          reg_learner$train(reg_task)
          
          # save models
          learner <- list(classifier = class_learner, regressor = if (!exists("reg_learner") || is.null(reg_learner)) NULL else reg_learner)
        }
        
        learner <- list(
          classifier = class_learner,
          regressor  = reg_learner  # can be NULL
        )
        
        # if not semicontinous
      } else {
        
        # Basis-Pipeline with best learner
        full_pipeline <- current_learner
        if (grepl("xgboost", best_learner$id)) {
          current_learner$param_set$values <- modifyList(current_learner$param_set$values, xgboost_params)
        } else if (grepl("ranger", best_learner$id)) {
          current_learner$param_set$values <- modifyList(current_learner$param_set$values, ranger_params)
        } else if (grepl("glmnet", best_learner$id)) {
          current_learner$param_set$values <- modifyList(current_learner$param_set$values, glmnet_params)
        } else if (grepl("lm_rob|glm_rob", best_learner$id)) {
          current_learner$param_set$values <- modifyList(current_learner$param_set$values, robust_params)
        } else if (grepl("robgam_imp", best_learner$id)) {
          current_learner$param_set$values <- modifyList(current_learner$param_set$values, robgam_params)
        } else if (grepl("gam_imp", best_learner$id)) {
          current_learner$param_set$values <- modifyList(current_learner$param_set$values, gam_params)
        }
        
        # Handling of missing values
        if (method_var != "xgboost" && supports_missing && !is.null(po_x_miss)) { #xgboost can handle NAs directly, support_missings are learners that can handle missings if they are marked as such
          full_pipeline <- po_x_miss %>>% full_pipeline
        }
        # full_pipeline <- po_fixfactors %>>% full_pipeline
        
        # create and train graphLearner 
        learner <- GraphLearner$new(full_pipeline)
        
        if (isTRUE(tuning_status[[var]])) {
          # if tuning was done
          if (!is.null(hyperparameter_cache[[var]]) && isTRUE(hyperparameter_cache[[var]]$is_tuned)) {
            # If optimized parameters exist in the cache
            params <- hyperparameter_cache[[var]]$params
            if (verbose) {
              cat(sprintf("Use optimized parameters from the cache for %s\n", var))
            }
            
            # Set parameter in full_pipeline (without prefix)
            pipeline_valid <- intersect(names(params), full_pipeline$param_set$ids())
            full_pipeline$param_set$values <- modifyList(
              full_pipeline$param_set$values,
              params[pipeline_valid]
            )
            
            # Set parameters in GraphLearner (with prefix)
            prefixed_names <- paste0(best_learner$id, ".", names(params))
            learner_valid <- prefixed_names %in% learner$param_set$ids()
            
            if (any(learner_valid)) {
              prefixed_params <- setNames(params[learner_valid], prefixed_names[learner_valid])
              learner$param_set$values <- modifyList(
                learner$param_set$values,
                prefixed_params
              )
            }
            
            # Warning for non existent parameters
            missing_in_pipeline <- setdiff(names(params), full_pipeline$param_set$ids())
            missing_in_learner <- setdiff(names(params), 
                                          sub(paste0("^", best_learner$id, "\\."), "", 
                                              learner$param_set$ids()[startsWith(learner$param_set$ids(), best_learner$id)]))
            
            if (length(missing_in_pipeline) > 0) {
              warning("Missing in Pipeline: ", paste(missing_in_pipeline, collapse = ", "))
            }
            if (length(missing_in_learner) > 0) {
              warning("Missing in Learner: ", paste(missing_in_learner, collapse = ", "))
            }
            
          } 
        } 
        
        # Set predict type for classification problems
        if (exists("target_col") && is.factor(data_temp[[target_col]])) {
          learner$predict_type <- "prob"
        }
        
        learner$train(task)

      # Bootstrap: refit on resampled training data for model uncertainty
        stored_model_info <- NULL
        if (boot && !is_sc) {
          model_info <- extract_model_info(learner, method = method_var)
          model_info <- complete_model_info(model_info, learner = learner, task = task)
          stored_model_info <- model_info

          boot_idx <- bootstrap_resample(
            n = nrow(data_y_fill_final),
            strategy = robustboot,
            weights = model_info$weights,
            residuals = model_info$residuals,
            alpha = 0.75
          )

          boot_data <- data_y_fill_final[boot_idx, ]
          boot_data <- enforce_factor_levels(boot_data, factor_levels)

          if (is.numeric(boot_data[[target_col]])) {
            boot_task <- TaskRegr$new(id = paste0(target_col, "_boot"),
                                      backend = boot_data, target = target_col)
          } else {
            boot_task <- TaskClassif$new(id = paste0(target_col, "_boot"),
                                         backend = boot_data, target = target_col)
          }

          tryCatch({
            learner$train(boot_task)
            stored_model_info <- extract_model_info(learner, method = method_var)
            stored_model_info <- complete_model_info(stored_model_info, learner = learner, task = boot_task)
          }, error = function(e) {
            if (verbose) warning(sprintf(
              "Bootstrap refit failed for '%s': %s. Using original model.", var, e$message))
          })
        }

        # Store model info for uncertainty methods (even without bootstrap)
        if (is.null(stored_model_info) && uncert %in% c("normalerror", "resid") && !is_sc) {
          stored_model_info <- extract_model_info(learner, method = method_var)
          stored_model_info <- complete_model_info(stored_model_info, learner = learner, task = task)
        }
        
      }
### Train Model End ###
      
### *****Identify NAs Start***** ###################################################################################################
      if(verbose){
        message("***** Identify missing values *****")
      }
      
      # Missing indices
      missing_idx <- missing_indices[[var]]
      if (length(missing_idx) == 0) next
      
      variables <- colnames(data_temp)
      zero_flag_col <- paste0(var, "_zero_flag")
      
      if (!is_sc) {
        # Not semicontinuous
        feature_cols <- setdiff(variables, var)
        
        if (!isFALSE(selected_formula)) {
          backend_data <- mm_data[missing_idx, ]
          backend_data <- enforce_factor_levels(backend_data, factor_levels)
          
          # Ranger-specific handling for new levels
          if (method_var == "ranger") {
            backend_data <- set_out_of_range_factor_levels_to_na(backend_data, data_temp)
          }
          
          # Impute Missing Values
          if (any(is.na(backend_data))) {
            backend_data <- impute_missing_values(backend_data, data_temp)
          }
          
          check_all_factor_levels(backend_data, factor_levels)
          
        } else {
          backend_cols <- union(feature_cols, var)
          backend_data <- data_temp[missing_idx, backend_cols, with = FALSE]
          backend_data <- enforce_factor_levels(backend_data, factor_levels)
          
          if (method_var == "ranger") {
            backend_data <- set_out_of_range_factor_levels_to_na(backend_data, data_temp)
          }
          
          if (!supports_missing) {
            backend_data <- impute_missing_values(backend_data, data_y_fill)
          }
          check_all_factor_levels(backend_data, factor_levels)
        }
        
      } else {
        # Semicontinuous
        feature_cols <- setdiff(variables, c(var, zero_flag_col))
        
        if (!isFALSE(selected_formula)) {
          class_pred_data <- mm_data[missing_idx, ]
          class_pred_data <- enforce_factor_levels(class_pred_data, factor_levels)
          
          if (method_var == "ranger") {
            class_pred_data <- set_out_of_range_factor_levels_to_na(class_pred_data, data_temp)
          }
          
          if (anyNA(class_pred_data)) { # Nnew
            class_pred_data <- impute_missing_values(class_pred_data, data_temp) # Nnew
          }
          
        } else {
          class_pred_data <- data_temp[missing_idx, c(feature_cols, zero_flag_col), with = FALSE]
          class_pred_data <- enforce_factor_levels(class_pred_data, factor_levels)
          
          if (!supports_missing && anyNA(class_pred_data)) {
            class_pred_data <- impute_missing_values(class_pred_data, data_temp)
          }
        }
        
        reg_pred_data <- data_temp[data_temp[[var]] > 0, feature_cols, with = FALSE]
        reg_pred_data <- enforce_factor_levels(reg_pred_data, factor_levels)
        
        if (method_var == "ranger") { #Nnew
          reg_pred_data <- set_out_of_range_factor_levels_to_na(reg_pred_data, data_temp) #Nnew
        }
        
        # Replace new levels (Log-Regression) with modus
        if (method_var == "logreg") {
          for (col in names(reg_pred_data)) {
            if (is.factor(reg_pred_data[[col]])) {
              known_levels <- levels(data_temp[[col]])
              unknown_idx <- !reg_pred_data[[col]] %in% known_levels
              if (any(unknown_idx, na.rm = TRUE)) {
                mode_value <- names(which.max(table(data_temp[[col]], useNA = "no")))
                reg_pred_data[[col]][unknown_idx] <- mode_value
              }
            }
          }
        }
        
        if (!supports_missing && anyNA(reg_pred_data)) {
          reg_pred_data <- impute_missing_values(reg_pred_data, data_temp)
        }
      }
### Identify NAs End ###
      
### *****Predict Start***** ###################################################################################################
      if(verbose){
        message("***** Predict")
      }
      prediction_result <- predict_imputations(
        is_sc = is_sc,
        var = var,
        data = data,
        data_temp = data_temp,
        missing_idx = missing_idx,
        feature_cols = feature_cols,
        learner = learner,
        factor_levels = factor_levels,
        backend_data = backend_data,
        target_col = target_col,
        method_var = method_var,
        ranger_median = ranger_median,
        sequential = sequential,
        i = i,
        nseq = nseq,
        lhs_transformation = lhs_transformation,
        missing_indices = missing_indices,
        pmm = pmm,
        pmm_k = pmm_k,
        pmm_k_method = pmm_k_method,
        uncert = uncert,
        has_any_pmm = has_any_pmm,
        stored_model_info = stored_model_info,
        verbose = verbose
      )
      preds <- prediction_result$preds
      convergence_preds <- prediction_result$convergence_preds
      pred_task <- prediction_result$pred_task
      data <- prediction_result$data
### Predict End ###################################################################################################
      
      
### *****Prediction History Start***** ###################################################################################################
      if(verbose){
        message(paste("***** Predict History"))
      }
      if (pred_history == TRUE) {
        history[[paste0(var, "_iter", i)]] <- data.table(
          iteration = i,
          variable = var,
          index = missing_idx,
          predicted_values = preds
        )
      }
### Prediction History End ###
      
### *****Replace missing values with predicted values Start***** ###################################################################################################
      if(verbose){
        message(paste("***** Replace values with new predictions"))
      }
      imputation_result <- apply_imputations(
        data = data,
        data_new = data_new,
        convergence_data = convergence_data,
        var = var,
        missing_idx = missing_idx,
        preds = preds,
        convergence_preds = convergence_preds,
        original_data = original_data,
        factor_levels = factor_levels,
        imp_var = imp_var
      )
      data <- imputation_result$data
      data_new <- imputation_result$data_new
      convergence_data <- imputation_result$convergence_data
### Replace missing values with predicted values End ###

      var_end_time <- Sys.time()
      var_time <- difftime(var_end_time, var_start_time, units = "secs")
      iteration_times[[var]] <- round(as.numeric(var_time), 2)
      if(verbose){
        message(paste("time used for", var, ":", iteration_times[[var]], "seconds"))
      }
    }
### Variable loop END ###
    
### *****Stop Criteria Start***** ###################################################################################################
    convergence_result <- check_convergence(
      sequential = sequential,
      i = i,
      variables_NA = variables_NA,
      missing_indices = missing_indices,
      convergence_prev = convergence_prev,
      convergence_data = convergence_data,
      original_data = original_data,
      eps = eps,
      no_change_counter = no_change_counter,
      verbose = verbose,
      data_temp = data_temp,
      target_col = target_col,
      learner = learner,
      pred_task = pred_task
    )
    no_change_counter <- convergence_result$no_change_counter
    if (isTRUE(convergence_result$should_break)) {
      break
    }
  }
### Stop criteria END ###
  
  return(build_vimpute_result(
    data = data,
    data_new = data_new,
    imp_var = imp_var,
    factor_levels = factor_levels,
    pred_history = pred_history,
    history = history,
    tune = tune,
    tuning_log = tuning_log,
    ordered_info = ordered_info,
    data_all_variables = data_all_variables,
    considered_variables = considered_variables,
    keep_all_columns = keep_all_columns
  ))
}
