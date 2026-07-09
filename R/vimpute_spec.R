# =============================================================================
# Per-variable imputation specs and the formula grammar (audit P2.62)
# =============================================================================
# Layer 1: spec objects -- vimpute(data, spec = list(y = vs_ranger(num.trees =
# 500, tune = TRUE), x = vs_gam(formula = ~ s(age)), .default = vs_ranger())).
# Layer 2: formula grammar as sugar over the same specs --
# vimpute(data, y ~ x1 + x2 | ranger(tune = TRUE), g ~ . | robust()).
# Both layers compile to the classic flat per-variable arguments (method,
# learner_params, formula, predictors, tune, pmm, pmm_k, pmm_k_method, makeNA,
# donorcond), which keep working unchanged as the compatibility layer.

#' Per-variable imputation specification for `vimpute()`
#'
#' Bundles everything `vimpute()` can configure for a single variable --
#' method, learner parameters, `formula` or `predictors`, `tune`, PMM
#' settings, `makeNA` values and a `donorcond` donor condition -- into one
#' object, instead of coordinating up to nine parallel per-variable
#' arguments. Pass a named list of specs as `vimpute(spec = )`; the reserved
#' name `".default"` supplies the spec for variables not listed (its method,
#' learner parameters and tune/PMM knobs; `formula`/`predictors`/`makeNA`/
#' `donorcond` are variable-specific and not allowed in `".default"`).
#'
#' Learner parameters in `...` are validated **eagerly** against the
#' method's learner parameter sets, so a typo fails at the constructor call,
#' not in iteration 7 of the imputation. `vs_ranger()`, `vs_xgboost()`,
#' `vs_regularized()`, `vs_robust()`, `vs_gam()` and `vs_robgam()` are
#' shorthands for the built-in methods; methods added via
#' [register_vimpute_method()] use `vimpute_spec("<name>", ...)`.
#'
#' A variable with a spec uses that spec's settings exactly (no field-level
#' merging with `".default"`). `uncert` remains a call-level argument of
#' [vimpute()] and is not a spec knob.
#'
#' @param method Single string: a registered imputation method (see
#'   [vimpute_methods()]).
#' @param ... Named learner parameters for the method's learners (e.g.
#'   `num.trees = 500` for `"ranger"`), validated eagerly.
#' @param formula One-sided (`~ x1 + s(x2)`) or two-sided formula for the
#'   variable's model; only for methods with formula support. A one-sided
#'   formula is completed with the variable as its left-hand side.
#' @param predictors Character vector of predictor columns (the
#'   `predictors` equivalent, works for every method). A spec takes either
#'   `formula` or `predictors`, not both.
#' @param tune Logical: tune this variable's learner (see
#'   [vimpute_tune_control()]).
#' @param pmm Logical: predictive mean matching for this (numeric) variable.
#' @param pmm_k `NULL` or a positive integer: PMM donor count.
#' @param pmm_k_method `NULL`, `"mean"`, `"median"`, `"random"`, or a
#'   function: PMM aggregation when `pmm_k > 1`.
#' @param makeNA `NULL` or a vector of values to be treated as missing for
#'   this variable (as in `vimpute(makeNA = )`).
#' @param donorcond `NULL` or a donor condition string such as `">= 0"` (as
#'   in `vimpute(donorcond = )`).
#' @param uncert Not a spec knob -- set `uncert` on the [vimpute()] call;
#'   supplying it here errors (it guards against silently treating it as a
#'   learner parameter).
#' @return An object of class `vimpute_spec`.
#' @seealso [vimpute()], [register_vimpute_method()]
#' @export
#' @examples
#' \dontrun{
#' data(sleep)
#' res <- vimpute(sleep,
#'   spec = list(
#'     Sleep    = vs_ranger(num.trees = 300, tune = TRUE),
#'     NonD     = vs_robust(donorcond = ">= 0"),
#'     .default = vs_ranger()
#'   ),
#'   seed = 1)
#'
#' # the same, as formula grammar:
#' res2 <- vimpute(sleep,
#'   Sleep ~ . | ranger(num.trees = 300, tune = TRUE),
#'   NonD  ~ . | robust(donorcond = ">= 0"),
#'   .default = vs_ranger(),
#'   seed = 1)
#' }
vimpute_spec <- function(method, ..., formula = NULL, predictors = NULL,
                         tune = FALSE, pmm = FALSE, pmm_k = NULL,
                         pmm_k_method = NULL, makeNA = NULL, donorcond = NULL,
                         uncert = NULL) {
  if (!is.character(method) || length(method) != 1L || !nzchar(method)) {
    stop("'method' must be a single method name; see vimpute_methods().")
  }
  entry <- prepare_vimpute_method(method)

  if (!is.null(uncert)) {
    stop("'uncert' is a call-level argument of vimpute(), not a spec knob; ",
         "set it in the vimpute() call.")
  }

  params <- list(...)
  if (length(params) > 0L) {
    if (is.null(names(params)) || any(!nzchar(names(params)))) {
      stop("Learner parameters passed to a spec must be named, e.g. vs_ranger(num.trees = 500).")
    }
    # eager validation: union of the parameter ids over all learner
    # candidates of the method (the task-type intersection happens at fit
    # time, as for the flat learner_params)
    valid_ids <- unique(unlist(lapply(
      unlist(entry$learner, use.names = FALSE),
      function(id) lrn(id)$param_set$ids()
    )))
    bad <- setdiff(names(params), valid_ids)
    if (length(bad) > 0L) {
      stop(sprintf("Unknown parameter(s) for method '%s': %s.",
                   method, paste(bad, collapse = ", ")))
    }
  }

  if (!is.null(formula) && !is.null(predictors)) {
    stop("A spec takes either 'formula' or 'predictors', not both.")
  }
  if (!is.null(formula)) {
    if (!inherits(formula, "formula")) {
      stop("'formula' must be a formula (one-sided '~ x1 + x2' or two-sided).")
    }
    if (!isTRUE(entry$supports_formula)) {
      stop(sprintf(
        "Method '%s' does not support formulas (methods with formula support: %s). Use 'predictors' to restrict the columns instead.",
        method, paste(vimpute_formula_methods(), collapse = ", ")))
    }
  }
  if (!is.null(predictors) &&
      (!is.character(predictors) || length(predictors) == 0L || anyNA(predictors))) {
    stop("'predictors' must be a character vector of column names.")
  }
  if (!is.logical(tune) || length(tune) != 1L || is.na(tune)) {
    stop("'tune' must be TRUE or FALSE.")
  }
  if (!is.logical(pmm) || length(pmm) != 1L || is.na(pmm)) {
    stop("'pmm' must be TRUE or FALSE.")
  }
  if (!is.null(pmm_k) && (!is.numeric(pmm_k) || length(pmm_k) != 1L ||
                          is.na(pmm_k) || pmm_k < 1 || pmm_k != as.integer(pmm_k))) {
    stop("'pmm_k' must be NULL or a single positive integer.")
  }
  if (!is.null(pmm_k_method) && !is.function(pmm_k_method) &&
      !(is.character(pmm_k_method) && length(pmm_k_method) == 1L &&
        pmm_k_method %in% c("mean", "median", "random"))) {
    stop("'pmm_k_method' must be NULL, \"mean\", \"median\", \"random\", or a function.")
  }
  if (!is.null(makeNA) && !is.atomic(makeNA)) {
    stop("'makeNA' must be NULL or a vector of values to treat as missing.")
  }
  if (!is.null(donorcond) && (!is.character(donorcond) || length(donorcond) == 0L)) {
    stop("'donorcond' must be NULL or a condition string such as \">= 0\".")
  }

  structure(
    list(
      method       = method,
      params       = params,
      formula      = formula,
      predictors   = predictors,
      tune         = isTRUE(tune),
      pmm          = isTRUE(pmm),
      pmm_k        = if (is.null(pmm_k)) NULL else as.integer(pmm_k),
      pmm_k_method = pmm_k_method,
      makeNA       = makeNA,
      donorcond    = donorcond
    ),
    class = "vimpute_spec"
  )
}

#' @rdname vimpute_spec
#' @export
vs_ranger <- function(...) vimpute_spec("ranger", ...)

#' @rdname vimpute_spec
#' @export
vs_xgboost <- function(...) vimpute_spec("xgboost", ...)

#' @rdname vimpute_spec
#' @export
vs_regularized <- function(...) vimpute_spec("regularized", ...)

#' @rdname vimpute_spec
#' @export
vs_robust <- function(...) vimpute_spec("robust", ...)

#' @rdname vimpute_spec
#' @export
vs_gam <- function(...) vimpute_spec("gam", ...)

#' @rdname vimpute_spec
#' @export
vs_robgam <- function(...) vimpute_spec("robgam", ...)

#' @param x A `vimpute_spec` object
#' @rdname vimpute_spec
#' @method print vimpute_spec
#' @export
print.vimpute_spec <- function(x, ...) {
  knobs <- c(
    if (length(x$params) > 0L)
      paste0(names(x$params), " = ", vapply(x$params, function(p) paste(deparse(p), collapse = ""), character(1))),
    if (!is.null(x$formula)) paste("formula:", paste(deparse(x$formula), collapse = " ")),
    if (!is.null(x$predictors)) paste("predictors:", paste(x$predictors, collapse = ", ")),
    if (isTRUE(x$tune)) "tune",
    if (isTRUE(x$pmm)) paste0("pmm", if (!is.null(x$pmm_k)) paste0("(k = ", x$pmm_k, ")")),
    if (!is.null(x$makeNA)) paste("makeNA:", paste(x$makeNA, collapse = ", ")),
    if (!is.null(x$donorcond)) paste("donorcond:", paste(x$donorcond, collapse = " & "))
  )
  cat(sprintf("vimpute spec: %s%s\n", x$method,
              if (length(knobs) > 0L) paste0(" (", paste(knobs, collapse = "; "), ")") else ""))
  invisible(x)
}

# -----------------------------------------------------------------------------
# Compilation: spec list -> the flat per-variable arguments
# -----------------------------------------------------------------------------

# Compiles a named list of vimpute_spec objects (reserved name ".default"
# for unlisted variables) into vimpute()'s classic flat per-variable
# arguments. `data` is the raw input (needed to predict which variables will
# be imputed, mirroring vimpute's impute_mask: observed NAs, or the makeNA
# matches for variables whose spec sets makeNA).
compile_vimpute_spec <- function(spec, data, considered_variables) {
  if (!is.list(spec) || length(spec) == 0L) {
    stop("'spec' must be a non-empty named list of vimpute_spec objects.")
  }
  nm <- names(spec)
  if (is.null(nm) || any(!nzchar(nm))) {
    stop("'spec' must be fully named: variable names, plus optionally '.default'.")
  }
  if (anyDuplicated(nm)) {
    stop(sprintf("Duplicate names in 'spec': %s.",
                 paste(unique(nm[duplicated(nm)]), collapse = ", ")))
  }

  default_spec <- spec[[".default"]]
  var_specs <- spec[setdiff(nm, ".default")]

  for (v in names(var_specs)) {
    if (!inherits(var_specs[[v]], "vimpute_spec")) {
      stop(sprintf(
        "spec entry '%s' is not a vimpute_spec object; build it with vs_*() or vimpute_spec().", v))
    }
  }
  if (!is.null(default_spec)) {
    if (!inherits(default_spec, "vimpute_spec")) {
      stop("'.default' must be a vimpute_spec object; build it with vs_*() or vimpute_spec().")
    }
    if (!is.null(default_spec$formula) || !is.null(default_spec$predictors) ||
        !is.null(default_spec$makeNA) || !is.null(default_spec$donorcond)) {
      stop("'.default' can set the method, learner parameters and tune/PMM knobs only; ",
           "'formula', 'predictors', 'makeNA' and 'donorcond' are variable-specific.")
    }
  }

  unknown <- setdiff(names(var_specs), names(data))
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown variable name(s) in 'spec': %s.",
                 paste(unknown, collapse = ", ")))
  }
  outside <- setdiff(names(var_specs), considered_variables)
  if (length(outside) > 0L) {
    stop(sprintf("Variable(s) in 'spec' are not in 'considered_variables': %s.",
                 paste(outside, collapse = ", ")))
  }

  # makeNA (from the specs) feeds the prediction of which variables will be
  # imputed, exactly mirroring vimpute's impute_mask semantics
  makeNA <- list()
  for (v in names(var_specs)) {
    if (!is.null(var_specs[[v]]$makeNA)) makeNA[[v]] <- var_specs[[v]]$makeNA
  }

  will_impute <- vapply(considered_variables, function(v) {
    if (v %in% names(makeNA)) {
      idx <- data[[v]] %in% makeNA[[v]]
      idx[is.na(idx)] <- FALSE
      any(idx)
    } else {
      anyNA(data[[v]])
    }
  }, logical(1))
  na_vars <- considered_variables[will_impute]
  if (length(na_vars) == 0L) {
    stop("Error: No variables with missing data found.")
  }

  spec_for <- function(v) {
    if (!is.null(var_specs[[v]])) var_specs[[v]] else default_spec
  }
  default_method <- if (!is.null(default_spec)) default_spec$method else "ranger"

  method <- setNames(vector("list", length(na_vars)), na_vars)
  tune   <- setNames(vector("list", length(na_vars)), na_vars)
  pmm    <- setNames(vector("list", length(na_vars)), na_vars)
  learner_params <- list()
  pmm_k <- list()
  pmm_k_method <- list()
  formula <- list()
  predictors <- list()
  donorcond <- list()

  for (v in na_vars) {
    sp <- spec_for(v)
    method[[v]] <- if (is.null(sp)) default_method else sp$method
    tune[[v]]   <- if (is.null(sp)) FALSE else isTRUE(sp$tune)
    pmm[[v]]    <- if (is.null(sp)) FALSE else isTRUE(sp$pmm)
    if (!is.null(sp)) {
      if (length(sp$params) > 0L) learner_params[[v]] <- sp$params
      if (!is.null(sp$pmm_k)) pmm_k[[v]] <- sp$pmm_k
      if (!is.null(sp$pmm_k_method)) pmm_k_method[[v]] <- sp$pmm_k_method
    }
  }

  # formula / predictors / donorcond only exist on explicit variable specs
  for (v in names(var_specs)) {
    sp <- var_specs[[v]]
    if (!is.null(sp$formula)) {
      f <- sp$formula
      if (length(f) == 2L) {
        # one-sided: complete with the variable as target, keep the env
        f <- eval(call("~", as.name(v), f[[2]]), envir = environment(sp$formula))
      } else {
        lhs_vars <- all.vars(f[[2]])
        if (!identical(lhs_vars, v)) {
          stop(sprintf(
            "The formula of spec '%s' must model '%s' on its left-hand side (got: %s).",
            v, v, paste(deparse(f), collapse = " ")))
        }
      }
      formula[[v]] <- f
    }
    if (!is.null(sp$predictors)) predictors[[v]] <- sp$predictors
    if (!is.null(sp$donorcond)) donorcond[[v]] <- sp$donorcond
  }

  list(
    method         = method,
    learner_params = if (length(learner_params) > 0L) learner_params else NULL,
    formula        = if (length(formula) > 0L) formula else FALSE,
    predictors     = if (length(predictors) > 0L) predictors else NULL,
    tune           = tune,
    pmm            = pmm,
    pmm_k          = if (length(pmm_k) > 0L) pmm_k else NULL,
    pmm_k_method   = if (length(pmm_k_method) > 0L) pmm_k_method else "mean",
    makeNA         = if (length(makeNA) > 0L) makeNA else NULL,
    donorcond      = if (length(donorcond) > 0L) donorcond else NULL
  )
}

# -----------------------------------------------------------------------------
# Formula grammar: bare formulas in vimpute(...) -> a spec list
# -----------------------------------------------------------------------------

# TRUE when the expression is only column symbols combined with `+`
is_plain_columns <- function(e) {
  if (is.name(e)) return(TRUE)
  if (is.call(e) && identical(e[[1]], as.name("+"))) {
    return(all(vapply(as.list(e)[-1], is_plain_columns, logical(1))))
  }
  FALSE
}

# Builds a vimpute_spec from an (unevaluated) method call like
# `ranger(num.trees = 300, tune = TRUE)`, evaluating the argument values in
# `env`. The call's function name is the registered method name.
spec_from_method_call <- function(method_call, env) {
  if (!is.call(method_call) || !is.name(method_call[[1]])) {
    stop("The part right of '|' must be a method call like ranger(tune = TRUE); ",
         "see vimpute_methods() for the registered methods.")
  }
  method_name <- as.character(method_call[[1]])
  args <- lapply(as.list(method_call)[-1], eval, envir = env)
  do.call(vimpute_spec, c(list(method = method_name), args))
}

# Parses one grammar formula `target ~ predictors | method(...)`:
# a plain-column RHS lowers to `predictors` (works for every method), an RHS
# with transformations (s(x), log(x), I(x^2), interactions, ...) lowers to a
# full model `formula` (formula-capable methods only). Without `| method()`
# the default method is used.
parse_grammar_formula <- function(f, default_method) {
  env <- environment(f)
  lhs <- f[[2]]
  rhs <- f[[3]]

  method_call <- NULL
  if (is.call(rhs) && identical(rhs[[1]], as.name("|"))) {
    method_call <- rhs[[3]]
    rhs <- rhs[[2]]
  }

  lhs_vars <- all.vars(lhs)
  if (length(lhs_vars) != 1L) {
    stop(sprintf(
      "The left-hand side of a grammar formula must reference exactly one target variable (got: %s).",
      paste(deparse(f), collapse = " ")))
  }
  target <- lhs_vars
  lhs_plain <- is.name(lhs)

  rhs_is_dot <- identical(rhs, as.name("."))
  rhs_vars <- all.vars(rhs)
  if (!rhs_is_dot && "." %in% rhs_vars) {
    stop("'.' can only be used alone on the right-hand side (target ~ .).")
  }

  spec_args <- list()
  if (!lhs_plain || (!rhs_is_dot && !is_plain_columns(rhs))) {
    # transformations involved: lower to a full model formula
    spec_args$formula <- eval(call("~", lhs, rhs), envir = env)
  } else if (!rhs_is_dot) {
    spec_args$predictors <- rhs_vars
  }

  spec <- if (!is.null(method_call)) {
    extra <- spec_args
    base_args <- lapply(as.list(method_call)[-1], eval, envir = env)
    if (any(names(extra) %in% names(base_args))) {
      stop(sprintf(
        "'%s' is set both by the formula and inside the method call for '%s'.",
        paste(intersect(names(extra), names(base_args)), collapse = ", "), target))
    }
    do.call(vimpute_spec, c(list(method = as.character(method_call[[1]])),
                            base_args, extra))
  } else {
    do.call(vimpute_spec, c(list(method = default_method), spec_args))
  }

  list(target = target, spec = spec)
}

# Turns the unevaluated `...` of a vimpute() call (grammar formulas plus an
# optional `.default =`) into a spec list for compile_vimpute_spec().
compile_vimpute_grammar <- function(dots, env) {
  dot_names <- names(dots)
  if (is.null(dot_names)) dot_names <- rep("", length(dots))

  # resolve .default first: formulas without a | method() part need it
  default_spec <- NULL
  default_idx <- which(dot_names == ".default")
  if (length(default_idx) > 1L) stop("'.default' may be given once.")
  if (length(default_idx) == 1L) {
    ex <- dots[[default_idx]]
    if (is.call(ex) && is.name(ex[[1]]) &&
        !is.null(get_vimpute_method(as.character(ex[[1]])))) {
      # bare method call: .default = ranger(num.trees = 300)
      default_spec <- spec_from_method_call(ex, env)
    } else {
      val <- eval(ex, env)
      if (inherits(val, "vimpute_spec")) {
        default_spec <- val
      } else if (is.character(val) && length(val) == 1L) {
        default_spec <- vimpute_spec(val)
      } else {
        stop("'.default' must be a vimpute_spec (vs_*()), a method name, ",
             "or a bare method call like ranger().")
      }
    }
    dots <- dots[-default_idx]
    dot_names <- dot_names[-default_idx]
  }

  bad_named <- which(nzchar(dot_names))
  if (length(bad_named) > 0L) {
    stop(sprintf(
      "Unknown argument(s) in vimpute(): %s. Per-variable settings are given as bare grammar formulas (y ~ x | ranger()), '.default = ', or via 'spec = '.",
      paste(dot_names[bad_named], collapse = ", ")))
  }

  default_method <- if (!is.null(default_spec)) default_spec$method else "ranger"

  spec <- list()
  for (ex in dots) {
    f <- eval(ex, env)
    if (!inherits(f, "formula") || length(f) != 3L) {
      stop("Bare arguments to vimpute() must be two-sided grammar formulas of the ",
           "form 'target ~ predictors | method(...)'; use 'spec = ' or the flat ",
           "arguments otherwise.")
    }
    parsed <- parse_grammar_formula(f, default_method = default_method)
    if (!is.null(spec[[parsed$target]])) {
      stop(sprintf("Variable '%s' is specified by more than one grammar formula.",
                   parsed$target))
    }
    spec[[parsed$target]] <- parsed$spec
  }
  if (!is.null(default_spec)) spec[[".default"]] <- default_spec
  spec
}
