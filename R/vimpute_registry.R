# =============================================================================
# vimpute() method registry
# =============================================================================
# One package-env registry is the single source of truth for the imputation
# methods vimpute() knows: the six built-ins are seeded through the same
# contract, and third parties add methods with one register_vimpute_method()
# call instead of patching VIM (previously ~12 coordinated edits across
# vimpute.R and helper_vimpute.R).

.vimpute_methods <- new.env(parent = emptyenv())

# Validates the contract and builds a registry entry (shared by the exported
# registration function and the built-in seeder).
new_vimpute_method_entry <- function(name,
                                     learner,
                                     packages = character(),
                                     setup = NULL,
                                     defaults = NULL,
                                     search_space = NULL,
                                     supports_formula = FALSE,
                                     fallback = "robust",
                                     validate = NULL,
                                     builtin = FALSE) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name) || is.na(name)) {
    stop("'name' must be a single non-empty character string.")
  }
  if (!is.list(learner) || is.null(names(learner)) ||
      !all(names(learner) %in% c("regr", "classif")) || length(learner) == 0L) {
    stop("'learner' must be a named list with elements 'regr' and/or 'classif', ",
         "e.g. list(regr = \"regr.rpart\", classif = \"classif.rpart\").")
  }
  for (tt in names(learner)) {
    ids <- learner[[tt]]
    if (!is.character(ids) || length(ids) == 0L || anyNA(ids) || !all(nzchar(ids))) {
      stop(sprintf("'learner$%s' must be a character vector of mlr3 learner ids.", tt))
    }
  }
  if (!is.character(packages) || anyNA(packages)) {
    stop("'packages' must be a character vector.")
  }
  for (arg in c("setup", "defaults", "search_space", "validate")) {
    val <- get(arg, inherits = FALSE)
    ok <- switch(arg,
      defaults = is.null(val) || is.function(val) || is.list(val),
      is.null(val) || is.function(val)
    )
    if (!ok) {
      stop(sprintf("'%s' must be NULL or a function%s.", arg,
                   if (arg == "defaults") " (or a named list of parameter values)" else ""))
    }
  }
  if (!is.logical(supports_formula) || length(supports_formula) != 1L ||
      is.na(supports_formula)) {
    stop("'supports_formula' must be TRUE or FALSE.")
  }
  if (!is.character(fallback) || length(fallback) != 1L || !nzchar(fallback)) {
    stop("'fallback' must be a single method name.")
  }
  list(
    name = name,
    learner = learner,
    packages = packages,
    setup = setup,
    defaults = defaults,
    search_space = search_space,
    supports_formula = supports_formula,
    fallback = fallback,
    validate = validate,
    builtin = builtin
  )
}

#' Register an imputation method for `vimpute()`
#'
#' `vimpute()` resolves its `method` argument through a package-level method
#' registry. The built-in methods (`"ranger"`, `"xgboost"`, `"regularized"`,
#' `"robust"`, `"gam"`, `"robgam"`, `"restricted"`) are pre-registered; this function adds
#' (or, with `overwrite = TRUE`, replaces) a user-defined method backed by any
#' pair of mlr3 learners -- e.g. `regr.rpart`/`classif.rpart` from mlr3
#' itself, or learners from `mlr3extralearners` such as lightgbm -- without
#' modifying VIM. After registration the new name can be used anywhere the
#' built-in method names work: as a global `method`, in a per-variable method
#' list, and in method-keyed `learner_params`.
#'
#' Uncertainty handling for registered methods: PMM (`uncert = "pmm"`, the
#' default, and `pmm = TRUE`) and `uncert = "midastouch"` work with any
#' method because they only use the method's predictions.
#' `uncert = "normalerror"`/`"resid"` and `boot = TRUE` derive residuals from
#' training predictions when the model object does not expose them.
#'
#' @param name Single character string: the method name to be used in
#'   `vimpute(method = )`. Must not collide with a registered method unless
#'   `overwrite = TRUE`. Built-in methods cannot be replaced or removed.
#' @param learner Named list with elements `regr` and/or `classif`, each a
#'   character vector of mlr3 learner ids (candidates in preference order; the
#'   first is the default, multiple candidates are compared by cross-validation
#'   like the built-in `"regularized"` method). Methods registered with only a
#'   `regr` (or only a `classif`) learner fall back to `fallback` for target
#'   variables of the other type, with a warning.
#' @param packages Character vector of packages that must be installed when the
#'   method is used (checked with `requireNamespace()` at `vimpute()` call
#'   time, not at registration).
#' @param setup `NULL` or a function with no arguments, called once per
#'   `vimpute()` call before the method's learners are constructed. Use it to
#'   register custom mlr3 learners or load learner collections (e.g.
#'   `function() library(mlr3extralearners)`).
#' @param defaults `NULL`, a named list of learner parameter values, or a
#'   function `function(task_type, nthread)` returning such a list
#'   (`task_type` is `"regr"` or `"classif"`, `nthread` the thread count
#'   vimpute chose for the data size). User-supplied `learner_params` override
#'   these defaults.
#' @param search_space `NULL` or a function `function(learner_id, task)`
#'   returning `list(space = paradox::ps(...), n_evals = <integer>)`, consulted
#'   when `tune = TRUE`. Without it, tuning is skipped for the method with a
#'   warning (as for unknown learners).
#' @param supports_formula Logical: can the method be used with the `formula`
#'   argument of `vimpute()`? Formula-based imputation requires a learner that
#'   models from a design matrix; the built-ins with formula support are
#'   `"robust"`, `"regularized"`, `"gam"`, `"robgam"`, and `"restricted"`.
#' @param fallback Single method name used when `validate` rejects a variable
#'   or a target type has no learner. Defaults to `"robust"`.
#' @param validate `NULL` or a function `function(y_obs, data, variable)`
#'   called during pre-checking for every variable the method is assigned to
#'   (`y_obs`: the observed values of the target; `data`: the full dataset;
#'   `variable`: the target's name). Return `NULL` to accept, a character
#'   string (the warning message) to reject towards `fallback`, or
#'   `list(reason = , fallback = )` to reject towards a specific method.
#'   Fallbacks are validated in turn until a method accepts.
#' @param overwrite Logical: replace an existing registration of the same
#'   name? Built-in methods can never be replaced.
#' @return Invisibly, the registered method name.
#' @seealso [vimpute_methods()], [unregister_vimpute_method()], [vimpute()]
#' @family vimpute method registry
#' @export
#' @examples
#' # a CART method backed by mlr3's rpart learners -- one call, no VIM patching
#' register_vimpute_method("cart",
#'   learner  = list(regr = "regr.rpart", classif = "classif.rpart"),
#'   packages = "rpart")
#' "cart" %in% vimpute_methods()
#' \donttest{
#' data(sleep)
#' res <- vimpute(sleep[, c("Sleep", "Dream", "Span")], method = "cart",
#'                sequential = FALSE)
#' }
#' unregister_vimpute_method("cart")
register_vimpute_method <- function(name,
                                    learner,
                                    packages = character(),
                                    setup = NULL,
                                    defaults = NULL,
                                    search_space = NULL,
                                    supports_formula = FALSE,
                                    fallback = "robust",
                                    validate = NULL,
                                    overwrite = FALSE) {
  entry <- new_vimpute_method_entry(
    name = name, learner = learner, packages = packages, setup = setup,
    defaults = defaults, search_space = search_space,
    supports_formula = supports_formula, fallback = fallback,
    validate = validate, builtin = FALSE
  )
  existing <- get_vimpute_method(name)
  if (!is.null(existing)) {
    if (existing$builtin) {
      stop(sprintf("'%s' is a built-in vimpute method and cannot be replaced.", name))
    }
    if (!isTRUE(overwrite)) {
      stop(sprintf(
        "Method '%s' is already registered. Set overwrite = TRUE to replace it.", name))
    }
  }
  assign(name, entry, envir = .vimpute_methods)
  invisible(name)
}

#' List the imputation methods registered for `vimpute()`
#'
#' @return Character vector of registered method names (built-ins plus any
#'   methods added via [register_vimpute_method()]).
#' @seealso [register_vimpute_method()], [unregister_vimpute_method()]
#' @family vimpute method registry
#' @export
#' @examples
#' vimpute_methods()
vimpute_methods <- function() {
  sort(ls(envir = .vimpute_methods))
}

#' Remove a user-registered `vimpute()` method
#'
#' @param name Single character string: the method name to remove. Built-in
#'   methods cannot be removed.
#' @return Invisibly `TRUE` on success.
#' @seealso [register_vimpute_method()], [vimpute_methods()]
#' @family vimpute method registry
#' @export
#' @examples
#' register_vimpute_method("cart",
#'   learner = list(regr = "regr.rpart", classif = "classif.rpart"))
#' unregister_vimpute_method("cart")
unregister_vimpute_method <- function(name) {
  entry <- get_vimpute_method(name)
  if (is.null(entry)) {
    stop(sprintf("Method '%s' is not registered.", name))
  }
  if (entry$builtin) {
    stop(sprintf("'%s' is a built-in vimpute method and cannot be removed.", name))
  }
  rm(list = name, envir = .vimpute_methods)
  invisible(TRUE)
}

# Fetches a registry entry (NULL if the method is unknown).
get_vimpute_method <- function(name) {
  if (!is.character(name) || length(name) != 1L) return(NULL)
  if (!exists(name, envir = .vimpute_methods, inherits = FALSE)) return(NULL)
  get(name, envir = .vimpute_methods, inherits = FALSE)
}

# Names of the registered methods that support the formula interface.
vimpute_formula_methods <- function() {
  nms <- vimpute_methods()
  nms[vapply(nms, function(n) isTRUE(get_vimpute_method(n)$supports_formula),
             logical(1))]
}

# Prepares a method for use in a vimpute() run: verifies its packages are
# installed and runs its setup hook (e.g. registration of VIM's custom R6
# learners in mlr3's dictionary). Returns the entry invisibly.
prepare_vimpute_method <- function(name) {
  entry <- get_vimpute_method(name)
  if (is.null(entry)) {
    stop(sprintf("Unsupported method '%s'. Registered methods: %s.",
                 name, paste(vimpute_methods(), collapse = ", ")))
  }
  if (length(entry$packages) > 0L) {
    available <- vapply(entry$packages, requireNamespace, logical(1),
                        quietly = TRUE)
    if (!all(available)) {
      stop(sprintf(
        "Method '%s' requires the package(s): %s. Please install them.",
        name, paste(entry$packages[!available], collapse = ", ")))
    }
  }
  if (is.function(entry$setup)) entry$setup()
  invisible(entry)
}

# The learner candidates (constructed Learner objects) of a method for the
# current target type, in the entry's preference order.
method_learner_candidates <- function(entry, target_numeric, learners) {
  ids <- if (target_numeric) entry$learner$regr else entry$learner$classif
  lapply(ids, function(id) learners[[id]])
}

# Resolves the effective learner parameters of a method for one variable:
# registry defaults (possibly thread/task-type dependent), overridden by the
# user's learner_params, validated against the parameter ids every learner
# candidate of the method understands (invalid entries are dropped with a
# warning). Replaces the six per-method copies of this block in vimpute().
resolve_method_params <- function(method, candidates, user_params, variable,
                                  nthread, verbose = FALSE) {
  entry <- get_vimpute_method(method)
  task_type <- candidates[[1]]$task_type
  base <- if (is.function(entry$defaults)) {
    entry$defaults(task_type, nthread)
  } else if (is.list(entry$defaults)) {
    entry$defaults
  } else {
    list()
  }
  user <- if (is.null(user_params)) list() else user_params
  if (length(user) > 0L) {
    valid_ids <- Reduce(intersect,
                        lapply(candidates, function(l) l$param_set$ids()))
    invalid <- setdiff(names(user), valid_ids)
    if (length(invalid) > 0L) {
      warning(sprintf(
        "learner_params for variable '%s' contain parameters not supported by method '%s': %s. These parameters were ignored.",
        variable, method, paste(invalid, collapse = ", ")))
      user <- user[setdiff(names(user), invalid)]
    }
  }
  params <- utils::modifyList(base, user)
  if (verbose) {
    cat(sprintf("\n--- %s params for variable %s ---\n", method, variable))
    print(params)
  }
  params
}

# -----------------------------------------------------------------------------
# Built-in methods, seeded through the same contract
# -----------------------------------------------------------------------------

# Shared single-level-factor-predictor / degenerate-target checks of the GAM
# family; `method` only parameterizes the warning texts.
validate_gam_family <- function(y_obs, data, variable, method) {
  for (col in setdiff(names(data), variable)) {
    col_data <- data[[col]]
    if (is.factor(col_data)) {
      col_data <- droplevels(col_data)
      if (nlevels(col_data) < 2L) {
        return(sprintf(
          "Method '%s' for variable '%s' has single-level factor predictors. Falling back to 'robust'.",
          method, variable))
      }
    }
  }
  if (is.numeric(y_obs) && length(unique(y_obs)) < 2L) {
    return(sprintf(
      "Target '%s' has too few unique values for method '%s'. Falling back to 'robust'.",
      variable, method))
  }
  NULL
}

register_builtin_vimpute_methods <- function() {
  put <- function(...) {
    entry <- new_vimpute_method_entry(..., builtin = TRUE)
    assign(entry$name, entry, envir = .vimpute_methods)
  }

  put("ranger",
      learner = list(regr = "regr.ranger", classif = "classif.ranger"),
      defaults = function(task_type, nthread) {
        list(num.trees = 500, num.threads = nthread)
      })

  put("xgboost",
      learner = list(regr = "regr.xgboost", classif = "classif.xgboost"),
      defaults = function(task_type, nthread) {
        list(
          nrounds = 100,
          max_depth = 3,
          eta = 0.1,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          verbose = 1,
          nthread = nthread
        )
      })

  put("regularized",
      learner = list(regr = c("regr.cv_glmnet", "regr.glmnet"),
                     classif = "classif.glmnet"),
      packages = "glmnet",
      supports_formula = TRUE,
      validate = function(y_obs, data, variable) {
        if (is.factor(y_obs) && any(table(y_obs) <= 1)) {
          return(sprintf(
            "Target '%s' has too few observations per class for 'regularized'. Falling back to 'robust'.",
            variable))
        }
        if (is.numeric(y_obs) && length(unique(y_obs)) < 3) {
          return(sprintf(
            "Target '%s' has too few unique values for 'regularized'. Falling back to 'robust'.",
            variable))
        }
        for (col in setdiff(names(data), variable)) {
          x_obs <- data[[col]][!is.na(data[[col]])]
          if (is.factor(x_obs) && any(table(x_obs) <= 1)) {
            return(sprintf(
              "Predictor '%s' unsuitable for glmnet. Falling back to 'robust' for '%s'.",
              col, variable))
          }
          if (is.numeric(x_obs) && length(unique(x_obs)) < 2) {
            return(sprintf(
              "Predictor '%s' unsuitable for glmnet. Falling back to 'robust' for '%s'.",
              col, variable))
          }
        }
        NULL
      })

  put("robust",
      learner = list(regr = "regr.lm_rob", classif = "classif.glm_rob"),
      setup = function() register_robust_learners(),
      supports_formula = TRUE)

  put("restricted",
      learner = list(regr = "regr.restricted"),
      packages = c("ECOSolveR", "validate"),
      setup = function() register_restricted_learners(),
      supports_formula = TRUE)

  put("gam",
      learner = list(regr = "regr.gam_imp", classif = "classif.gam_imp"),
      packages = "mgcv",
      setup = function() register_gam_learners(),
      supports_formula = TRUE,
      validate = function(y_obs, data, variable) {
        validate_gam_family(y_obs, data, variable, "gam")
      })

  put("robgam",
      learner = list(regr = "regr.robgam_imp", classif = "classif.robgam_imp"),
      packages = "mgcv",
      setup = function() register_gam_learners(),
      supports_formula = TRUE,
      validate = function(y_obs, data, variable) {
        if (!is.numeric(y_obs)) {
          return(list(
            reason = sprintf(
              "Target '%s' is non-numeric. Method 'robgam' is regression-only. Falling back to 'gam'.",
              variable),
            fallback = "gam"))
        }
        validate_gam_family(y_obs, data, variable, "robgam")
      })

  invisible(NULL)
}

register_builtin_vimpute_methods()
