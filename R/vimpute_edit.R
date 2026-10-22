#' Controls for model-agnostic edit restrictions
#'
#' Used by [vimpute()] when `edit_rules` is supplied. Each record's eligible
#' numeric imputations are jointly projected onto the supported linear edits.
#' This control never changes restricted-regression coefficient constraints.
#'
#' @param weights `"equal"`, `"scaled"`, or a named positive finite numeric
#'   vector covering all eligible numeric variables referenced by the rules.
#'   Scaled weights are inverse squared observed scales: MAD, then IQR/1.349,
#'   then SD, then one. Nearly constant scales use one; scales are bounded to
#'   avoid floating-point overflow. These scales are units, not predictive
#'   uncertainty estimates. Observations selected by `makeNA` are excluded.
#' @param tolerance Positive finite absolute tolerance for constraint residuals.
#'   Strict inequalities use an interior margin of twice this tolerance.
#' @param on_infeasible Currently only `"error"` is supported.
#' @param save_optimization_problem Retain record/sweep solver inputs, outputs,
#'   and pre-projection predictions, including already-feasible predictions.
#' @return A `vimpute_edit_control` object. A plain named list is also accepted
#'   by `vimpute()` and merged with these defaults.
#' @export
vimpute_edit_control <- function(weights = "scaled", tolerance = 1e-6,
                                 on_infeasible = "error",
                                 save_optimization_problem = FALSE) {
  if (is.character(weights)) {
    if (length(weights) != 1L || is.na(weights) || !weights %in% c("equal", "scaled"))
      stop("weights must be 'equal', 'scaled', or a named positive numeric vector")
  } else if (!is.numeric(weights) || !length(weights) || any(!is.finite(weights) | weights <= 0) ||
             is.null(names(weights)) || anyNA(names(weights)) || any(!nzchar(names(weights))) ||
             anyDuplicated(names(weights))) {
    stop("Numeric weights must be finite, positive and uniquely named")
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1L || !is.finite(tolerance) || tolerance <= 0)
    stop("tolerance must be a positive finite number")
  if (!identical(on_infeasible, "error")) stop("on_infeasible currently supports only 'error'")
  if (!is.logical(save_optimization_problem) || length(save_optimization_problem) != 1L ||
      is.na(save_optimization_problem)) stop("save_optimization_problem must be TRUE or FALSE")
  structure(list(weights = weights, tolerance = tolerance, on_infeasible = on_infeasible,
                 save_optimization_problem = save_optimization_problem), class = "vimpute_edit_control")
}

.edit_control <- function(control) {
  if (is.null(control)) return(vimpute_edit_control())
  if (!is.list(control) || (length(control) && (is.null(names(control)) ||
      anyNA(names(control)) || any(!nzchar(names(control))) || anyDuplicated(names(control)))))
    stop("edit_control must be a named list or vimpute_edit_control object")
  unknown <- setdiff(names(control), names(formals(vimpute_edit_control)))
  if (length(unknown)) stop("Unknown edit_control fields: ", paste(unknown, collapse = ", "))
  do.call(vimpute_edit_control, unclass(control))
}

.edit_prepare <- function(rules, control = NULL) {
  if (is.null(rules)) return(NULL)
  for (pkg in c("validate", "ECOSolveR")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("edit_rules requires optional package '", pkg, "'")
  }
  if (!inherits(rules, "validator")) stop("edit_rules must be a validate::validator object")
  control <- .edit_control(control)
  # The expression normalization and restricted_mip_rule representation are
  # shared with coefficient restrictions; neither validator is modified.
  exprs <- .restricted_validator_exprs(rules)
  expanded <- .restricted_expand_rule_expressions(lapply(exprs, .restricted_rewrite_in_range))
  list(exprs = expanded$exprs, variables = unique(unlist(lapply(expanded$exprs, all.vars))),
       control = control)
}

.edit_abort <- function(message, row, rules, problem = NULL, infeasible = FALSE) {
  stop(structure(list(message = paste0("Edit projection at record ", row, " [",
    paste(unique(rules), collapse = ", "), "]: ", message), call = NULL,
    record = row, rule_names = unique(rules),
    edit_diagnostics = list(infeasible_problems = as.integer(infeasible), failure_rule_names = unique(rules)),
    edit_optimization_problem = problem), class = c("vimpute_edit_error", "error", "condition")))
}

.edit_substitute <- function(expr, fixed) {
  if (is.symbol(expr) && as.character(expr) %in% names(fixed)) return(fixed[[as.character(expr)]])
  if (is.call(expr)) {
    for (j in seq.int(2L, length(expr))) expr[[j]] <- .edit_substitute(expr[[j]], fixed)
  }
  expr
}

.edit_condition <- function(expr, fixed, free, row, name) {
  if (any(all.vars(expr) %in% free))
    .edit_abort("Conditions/disjunction alternatives must depend only on fixed variables", row, name)
  allowed <- c("(", "==", "!=", "<", "<=", ">", ">=", "&", "&&", "|", "||", "!",
               "+", "-", "*", "/", "%in%", "c")
  safe <- function(x) !is.call(x) || (as.character(x[[1L]]) %in% allowed &&
    all(vapply(as.list(x)[-1L], safe, logical(1))))
  if (!safe(expr)) .edit_abort("Unsupported condition expression", row, name)
  value <- tryCatch(eval(.edit_substitute(expr, fixed), envir = baseenv()), error = identity)
  if (!is.logical(value) || length(value) != 1L || is.na(value))
    .edit_abort("Condition cannot be evaluated from available fixed values", row, name)
  value
}

.edit_record_rules <- function(expr, fixed, free, row, name) {
  expr <- .restricted_consume_parentheses(expr)
  if (is.call(expr)) {
    op <- as.character(expr[[1L]])
    if (op %in% c("&", "&&")) return(c(
      .edit_record_rules(expr[[2L]], fixed, free, row, name),
      .edit_record_rules(expr[[3L]], fixed, free, row, name)))
    if (op == "if" && length(expr) %in% c(3L, 4L)) {
      active <- .edit_condition(expr[[2L]], fixed, free, row, name)
      if (active) return(.edit_record_rules(expr[[3L]], fixed, free, row, name))
      if (length(expr) == 4L) return(.edit_record_rules(expr[[4L]], fixed, free, row, name))
      return(list())
    }
    if (op %in% c("|", "||")) {
      adjustable <- vapply(as.list(expr)[-1L], function(x) any(all.vars(x) %in% free), logical(1))
      if (all(adjustable)) .edit_abort("Unsupported disjunction over adjustable variables (nonconvex/integer optimization)", row, name)
      if (!any(adjustable)) {
        if (.edit_condition(expr, fixed, free, row, name)) return(list())
        .edit_abort("Infeasible fixed disjunction", row, name, infeasible = TRUE)
      }
      alternative <- if (adjustable[1L]) expr[[3L]] else expr[[2L]]
      if (.edit_condition(alternative, fixed, free, row, name)) return(list())
      consequence <- if (adjustable[1L]) expr[[2L]] else expr[[3L]]
      return(.edit_record_rules(consequence, fixed, free, row, name))
    }
  }
  expr <- .restricted_rewrite_in_range(expr)
  if (is.call(expr) && as.character(expr[[1L]]) %in% c("&", "&&"))
    return(.edit_record_rules(expr, fixed, free, row, name))
  if (!is.call(expr) || !as.character(expr[[1L]]) %in% c("==", "<=", ">=", "<", ">"))
    .edit_abort("Unsupported edit: expected a linear equality or inequality", row, name)
  rule <- tryCatch(.restricted_rewrite_mip_rule(
    .restricted_linear_mip_rule(.edit_substitute(expr, fixed), name = name)), error = identity)
  if (inherits(rule, "error") || any(!is.finite(c(rule$a, rule$b))))
    .edit_abort("Unsupported nonlinear or nonfinite edit after fixed-value substitution", row, name)
  list(rule)
}

.edit_constraints <- function(prepared, full, free, row) {
  fixed_names <- setdiff(prepared$variables, free)
  fixed <- lapply(full[fixed_names], function(x) {
    z <- x[row]
    if (is.factor(z)) as.character(z) else z
  })
  unavailable <- names(fixed)[vapply(fixed, function(z) length(z) != 1L || is.na(z) ||
    (is.numeric(z) && !is.finite(z)), logical(1))]
  if (length(unavailable)) .edit_abort(paste("Unavailable fixed rule variables:", paste(unavailable, collapse = ", ")),
    row, names(prepared$exprs)[vapply(prepared$exprs, function(e) any(all.vars(e) %in% unavailable), logical(1))])
  pieces <- unlist(lapply(seq_along(prepared$exprs), function(j)
    .edit_record_rules(prepared$exprs[[j]], fixed, free, row, names(prepared$exprs)[j])), recursive = FALSE)
  mat <- matrix(0, length(pieces), length(free), dimnames = list(NULL, free))
  rhs <- numeric(length(pieces)); equality <- logical(length(pieces)); rule_names <- character(length(pieces))
  for (j in seq_along(pieces)) {
    p <- pieces[[j]]
    if (length(p$a)) mat[j, names(p$a)] <- p$a
    rhs[j] <- p$b - if (identical(p$op, "<")) 2 * prepared$control$tolerance else 0
    equality[j] <- identical(p$op, "=="); rule_names[j] <- p$rule
  }
  list(C = mat[!equality, , drop = FALSE], d = rhs[!equality],
       Aeq = mat[equality, , drop = FALSE], beq = rhs[equality],
       inequality_names = rule_names[!equality], equality_names = rule_names[equality])
}

.edit_residuals <- function(x, constraints) {
  c(inequality = pmax(0, as.numeric(constraints$C %*% x) - constraints$d),
    equality = abs(as.numeric(constraints$Aeq %*% x) - constraints$beq))
}

.edit_scale <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(1)
  threshold <- sqrt(.Machine$double.eps) * max(1, stats::median(abs(x)))
  candidates <- c(stats::mad(x), stats::IQR(x) / 1.349, stats::sd(x), 1)
  s <- candidates[is.finite(candidates) & candidates > threshold]
  min(1e150, max(1e-150, if (length(s)) s[1L] else 1))
}

.edit_solve <- function(prediction, weights, constraints, tolerance) {
  # Optimize centered, weighted changes z = sqrt(W) (x - prediction).
  # min ||z|| is equivalent to min 1/2 (x-prediction)' W (x-prediction).
  # One cone of size k+1 per record, never a dataset-sized dense system.
  k <- length(prediction); root_w <- sqrt(weights)
  C <- sweep(constraints$C, 2L, root_w, "/")
  d <- constraints$d - as.numeric(constraints$C %*% prediction)
  Aeq <- sweep(constraints$Aeq, 2L, root_w, "/")
  beq <- constraints$beq - as.numeric(constraints$Aeq %*% prediction)
  if (nrow(Aeq)) {
    q <- qr(Aeq)
    if (any(abs(beq - qr.fitted(q, beq)) > tolerance)) stop("Inconsistent equalities")
    independent <- qr(t(Aeq))
    keep <- independent$pivot[seq_len(independent$rank)]
    Aeq <- Aeq[keep, , drop = FALSE]; beq <- beq[keep]
  }
  # Constant constraints have already been checked before entering the solver.
  active <- if (nrow(C)) rowSums(abs(C)) > 0 else logical()
  C <- C[active, , drop = FALSE]; d <- d[active]
  G <- rbind(cbind(C, numeric(nrow(C))), cbind(rbind(numeric(k), diag(k)), c(-1, numeric(k))))
  arguments <- list(c = c(numeric(k), 1), G = G, h = c(d, 0, numeric(k)),
    dims = list(l = as.integer(nrow(C)), q = as.integer(k + 1L), e = 0L),
    A = if (nrow(Aeq)) cbind(Aeq, 0) else NULL, b = if (nrow(Aeq)) beq else NULL,
    control = ECOSolveR::ecos.control(verbose = 0L, feastol = min(1e-8, tolerance / 100),
      abstol = min(1e-8, tolerance / 100), reltol = min(1e-8, tolerance / 100)))
  solution <- do.call(ECOSolveR::ECOS_csolve, arguments)
  flag <- solution$retcodes[["exitFlag"]]
  if (is.null(flag)) flag <- solution$retcodes[["exitflag"]]
  x <- prediction + solution$x[seq_len(k)] / root_w
  list(x = x, inputs = arguments, output = solution, status = flag,
       residuals = .edit_residuals(x, constraints))
}

.project_imputed_values <- function(data, original_data, imputation_mask, rules, control = NULL,
                                    prepared = NULL, check_only = FALSE) {
  if (is.null(prepared)) prepared <- .edit_prepare(rules, control)
  if (is.null(prepared)) return(list(data = data, diagnostics = NULL, problems = NULL))
  control <- prepared$control
  original <- as.data.frame(original_data)
  full <- original
  current <- as.data.frame(data)
  if (nrow(current) != nrow(original)) stop("Projection data and original_data must have the same rows")
  for (v in intersect(names(current), names(full))) full[[v]] <- current[[v]]
  unknown <- setdiff(prepared$variables, names(full))
  if (length(unknown)) stop("Edit rules reference missing columns: ", paste(unknown, collapse = ", "))
  mask <- matrix(FALSE, nrow(full), ncol(full), dimnames = list(NULL, names(full)))
  if (is.matrix(imputation_mask) || is.data.frame(imputation_mask)) {
    if (nrow(imputation_mask) != nrow(full) || is.null(colnames(imputation_mask))) stop("Invalid imputation_mask")
    imputation_mask <- as.list(as.data.frame(imputation_mask))
  }
  if (!is.list(imputation_mask) || (length(imputation_mask) && (is.null(names(imputation_mask)) ||
      anyNA(names(imputation_mask)) || any(!nzchar(names(imputation_mask))) || anyDuplicated(names(imputation_mask)))) ||
      any(!names(imputation_mask) %in% names(full))) stop("Invalid imputation_mask")
  for (v in names(imputation_mask)) {
    z <- imputation_mask[[v]]
    if (!is.logical(z) || length(z) != nrow(full) || anyNA(z)) stop("Invalid imputation_mask for ", v)
    mask[, v] <- z
  }
  # precheck may encode logical covariates as 0/1. Conditions refer to their
  # original logical meaning; normalize the equivalent encoding for edits.
  logical_vars <- names(original)[vapply(original, is.logical, logical(1))]
  for (v in logical_vars) {
    z <- as.character(full[[v]])
    decoded <- rep(NA, length(z))
    decoded[z %in% c("TRUE", "1")] <- TRUE
    decoded[z %in% c("FALSE", "0")] <- FALSE
    if (any(!is.na(z) & is.na(decoded))) stop("Invalid logical encoding for rule variable: ", v)
    full[[v]] <- decoded
  }
  for (v in prepared$variables) {
    observed <- !mask[, v]
    a <- full[[v]][observed]; b <- original[[v]][observed]
    same <- if (is.numeric(a) && is.numeric(b)) identical(as.numeric(a), as.numeric(b))
            else identical(as.character(a), as.character(b))
    if (!same) stop("Fixed observed rule values were changed before projection: ", v)
  }
  numeric_vars <- names(original)[vapply(original, function(z) is.numeric(z) && !is.object(z), logical(1))]
  eligible <- intersect(prepared$variables, numeric_vars[colSums(mask[, numeric_vars, drop = FALSE]) > 0])
  scales <- vapply(eligible, function(v) .edit_scale(original[[v]][!mask[, v]]), numeric(1))
  weights <- if (is.numeric(control$weights)) {
    missing <- setdiff(eligible, names(control$weights))
    if (length(missing)) stop("Missing edit weights for eligible variables: ", paste(missing, collapse = ", "))
    control$weights[eligible]
  } else if (control$weights == "equal") stats::setNames(rep(1, length(eligible)), eligible) else 1 / scales^2
  summaries <- list(records_evaluated = nrow(full), records_already_satisfying = 0L,
    records_adjusted = 0L, variables_adjusted = 0L, optimization_problems_solved = 0L,
    maximum_absolute_adjustment = 0, maximum_scaled_adjustment = 0,
    maximum_edit_rule_residual = 0, infeasible_problems = 0L, failure_rule_names = character(),
    adjusted_rows = integer(), adjusted_variables = character())
  problems <- if (control$save_optimization_problem && !check_only) list() else NULL
  for (i in seq_len(nrow(full))) {
    free <- eligible[mask[i, eligible]]
    raw_vars <- numeric_vars[mask[i, numeric_vars]]
    raw_predictions <- if (control$save_optimization_problem && !check_only)
      stats::setNames(vapply(full[raw_vars], function(z) as.numeric(z[i]), numeric(1)), raw_vars) else NULL
    constraints <- .edit_constraints(prepared, full, free, i)
    names_rules <- c(constraints$inequality_names, constraints$equality_names)
    used <- if (length(free)) colSums(abs(rbind(constraints$C, constraints$Aeq))) > 0 else logical()
    # Remove variables absent from the active constraints (including canceled
    # coefficients), so the optimizer cannot touch unrelated integer cells.
    constraints$C <- constraints$C[, used, drop = FALSE]
    constraints$Aeq <- constraints$Aeq[, used, drop = FALSE]
    free <- free[used]
    integer_vars <- free[vapply(original[free], is.integer, logical(1))]
    if (length(integer_vars)) .edit_abort(paste("Integer-valued edit projection is unsupported; convert explicitly to continuous numeric:",
      paste(integer_vars, collapse = ", ")), i, names_rules)
    prediction <- stats::setNames(vapply(full[free], function(z) as.numeric(z[i]), numeric(1)), free)
    if (any(!is.finite(prediction))) .edit_abort("Nonfinite or missing eligible imputations", i, names_rules)
    residuals <- .edit_residuals(prediction, constraints)
    valid <- all(residuals <= control$tolerance)
    solver <- NULL; final <- prediction
    failure_problem <- if (control$save_optimization_problem) list(record = i, variables = free,
      original_predictions = prediction, stochastic_predictions = raw_predictions,
      weights = weights[free], constraints = constraints) else NULL
    if (!valid) {
      if (check_only) .edit_abort("Final completed values violate edit rules", i, names_rules[residuals > control$tolerance])
      constants <- rowSums(abs(rbind(constraints$C, constraints$Aeq))) == 0
      if (!length(free) || any(constants & residuals > control$tolerance))
        .edit_abort("Infeasible constraints on fixed values", i, names_rules[residuals > control$tolerance], failure_problem, infeasible = TRUE)
      solver <- tryCatch(.edit_solve(prediction, weights[free], constraints, control$tolerance), error = identity)
      if (inherits(solver, "error")) .edit_abort(paste("Optimization failed:", conditionMessage(solver)), i, names_rules,
        failure_problem, infeasible = grepl("Inconsistent equalities", conditionMessage(solver), fixed = TRUE))
      if (length(solver$x) != length(free) || any(!is.finite(solver$x)) ||
          !solver$status %in% c(0L, 10L) || any(!is.finite(solver$residuals)) ||
          any(solver$residuals > control$tolerance))
        .edit_abort("Infeasible or numerically invalid ECOS solution", i, names_rules,
                    if (control$save_optimization_problem) c(failure_problem, list(ecos_input = solver$inputs, ecos_output = solver$output)) else NULL,
                    infeasible = solver$status %in% c(1L, 11L))
      final <- solver$x
      for (v in free) full[[v]][i] <- final[v]
      delta <- abs(final - prediction)
      changed <- free[delta > 0]
      summaries$adjusted_rows <- c(summaries$adjusted_rows, i)
      summaries$adjusted_variables <- union(summaries$adjusted_variables, changed)
      summaries$optimization_problems_solved <- summaries$optimization_problems_solved + 1L
      summaries$maximum_absolute_adjustment <- max(summaries$maximum_absolute_adjustment, delta)
      summaries$maximum_scaled_adjustment <- max(summaries$maximum_scaled_adjustment, delta * sqrt(weights[free]))
      residuals <- solver$residuals
    } else summaries$records_already_satisfying <- summaries$records_already_satisfying + 1L
    summaries$maximum_edit_rule_residual <- max(c(summaries$maximum_edit_rule_residual, residuals))
    if (!is.null(problems) && length(raw_vars)) problems[[length(problems) + 1L]] <- list(
      record = i, variables = free, original_predictions = prediction, stochastic_predictions = raw_predictions, adjusted_imputations = final,
      weights = weights[free], constraints = constraints, ecos_input = if (!is.null(solver)) solver$inputs else NULL,
      ecos_output = if (!is.null(solver)) solver$output else NULL,
      solver_status = if (!is.null(solver)) solver$status else NULL, constraint_residuals = residuals)
  }
  summaries$records_adjusted <- length(summaries$adjusted_rows)
  summaries$variables_adjusted <- length(summaries$adjusted_variables)
  out <- current
  for (v in intersect(union(eligible, logical_vars), names(out))) out[[v]] <- full[[v]]
  if (data.table::is.data.table(data)) out <- data.table::as.data.table(out)
  list(data = out, diagnostics = summaries, problems = problems)
}

.edit_attach <- function(result, diagnostics, problems = NULL) {
  if (length(diagnostics)) {
    # Unique records/variables over all sweeps; solver calls and maxima over sweeps.
    diag <- diagnostics[[length(diagnostics)]]
    diag$adjusted_rows <- sort(unique(unlist(lapply(diagnostics, `[[`, "adjusted_rows"))))
    diag$adjusted_variables <- unique(unlist(lapply(diagnostics, `[[`, "adjusted_variables")))
    diag$records_adjusted <- length(diag$adjusted_rows)
    diag$records_already_satisfying <- diag$records_evaluated - diag$records_adjusted
    diag$variables_adjusted <- length(diag$adjusted_variables)
    diag$optimization_problems_solved <- sum(vapply(diagnostics, `[[`, numeric(1), "optimization_problems_solved"))
    for (v in c("maximum_absolute_adjustment", "maximum_scaled_adjustment", "maximum_edit_rule_residual"))
      diag[[v]] <- max(vapply(diagnostics, `[[`, numeric(1), v))
    diag$sweeps <- length(diagnostics); diag$per_sweep <- diagnostics
    attr(result, "edit_diagnostics") <- diag
  }
  if (length(problems)) attr(result, "edit_optimization_problems") <- problems
  result
}

# Formula wrappers perform several single-target fits. Apply this same generic
# projector once to their assembled LHS results, rather than projecting an
# unfinished target-by-target table or touching RHS missings left unchanged.
.edit_finish_wrapper <- function(result, original, variables, rules, control) {
  if (is.null(rules)) return(result)
  mask <- lapply(as.data.frame(original)[variables], is.na)
  projected <- .project_imputed_values(result, original, mask, rules, control)
  .edit_attach(projected$data, list(projected$diagnostics),
               if (!is.null(projected$problems)) list(projected$problems) else NULL)
}
