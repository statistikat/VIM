#' Restricted Regression Imputation
#'
#' Impute missing values with a linear regression model whose least-squares
#' estimator is computed as a conic optimization problem. Linear restrictions on
#' the imputed values are taken from a [validate::validator()] object.
#'
#' The arguments are the same as for [regressionImp()], with the additional
#' argument `rules`. Only numeric left-hand side variables and linear validation
#' rules are supported. Rules that contain the currently imputed variable are
#' translated into linear constraints on the regression predictions for rows to
#' be imputed.
#'
#' @param formula model formula to impute one or several variables
#' @param data A data.frame containing the data
#' @param family accepted for compatibility with [regressionImp()] and ignored
#' @param robust accepted for compatibility with [regressionImp()] and ignored
#' @param imp_var logical; if TRUE create TRUE/FALSE variables indicating
#'   imputation status
#' @param imp_suffix suffix used for TRUE/FALSE imputation variables
#' @param mod_cat accepted for compatibility with [regressionImp()] and ignored
#' @param rules a `validator` object from the validate package
#'
#' @return the imputed data set (same class as `data`)
#' @family imputation methods
#' @export
regression_restricted <- function(
  formula,
  data,
  family = "AUTO",
  robust = FALSE,
  imp_var = TRUE,
  imp_suffix = "imp",
  mod_cat = FALSE,
  rules
) {
  check_data(data)

  if (missing(rules)) {
    stop("Argument `rules` must be supplied.", call. = FALSE)
  }
  if (!requireNamespace("validate", quietly = TRUE)) {
    stop(
      "Package `validate` is required for `regression_restricted()`.",
      call. = FALSE
    )
  }
  if (!inherits(rules, "validator")) {
    stop(
      "`rules` must be an object of class `validator` from package validate.",
      call. = FALSE
    )
  }
  if (!identical(family, "AUTO") || !isFALSE(robust) || !isFALSE(mod_cat)) {
    warning(
      "`family`, `robust`, and `mod_cat` are accepted for compatibility with ",
      "regressionImp() but ignored by regression_restricted()."
    )
  }

  data_out <- as.data.frame(data)
  formchar <- as.character(formula)
  lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
  rhs <- formchar[3]
  rhs_vars <- all.vars(stats::as.formula(paste("~", rhs)))

  for (lhsV in lhs) {
    lhs_vector <- data_out[[lhsV]]

    if (!is.numeric(lhs_vector)) {
      stop(
        "`regression_restricted()` currently supports only numeric left-hand side variables.",
        call. = FALSE
      )
    }

    target_imp_col <- paste0(lhsV, "_", imp_suffix)

    if (!any(is.na(lhs_vector))) {
      message(sprintf("No missings in %s.", lhsV))
      if (imp_var) {
        if (target_imp_col %in% colnames(data_out)) {
          warning(
            "The following TRUE/FALSE imputation status variables will be updated: ",
            target_imp_col
          )
        }
        data_out[[target_imp_col]] <- is.na(lhs_vector)
      }
      next
    }

    rhs_complete <- stats::complete.cases(data_out[, rhs_vars, drop = FALSE])
    train_idx <- rhs_complete & !is.na(lhs_vector)
    pred_idx <- rhs_complete & is.na(lhs_vector)

    if (sum(train_idx) < 2L) {
      warning(
        "Not enough complete observations to fit a restricted regression for ",
        lhsV,
        "."
      )
      if (imp_var) {
        data_out[[target_imp_col]] <- is.na(lhs_vector)
      }
      next
    }

    model_parts <- .restricted_model_matrices(
      lhsV,
      rhs,
      data_out,
      train_idx,
      pred_idx
    )

    if (nrow(model_parts$X_pred) == 0L) {
      message(paste0(
        "No missings in ",
        lhsV,
        " with valid values in the predictor variables."
      ))
      if (imp_var) {
        data_out[[target_imp_col]] <- is.na(lhs_vector)
      }
      next
    }

    constraints <- .restricted_validate_constraints(
      rules = rules,
      lhs = lhsV,
      data_pred = data_out[pred_idx, , drop = FALSE],
      X_pred = model_parts$X_pred
    )

    beta <- .restricted_ecos_lm(
      X = model_parts$X_train,
      y = model_parts$y_train,
      C = constraints$C,
      d = constraints$d,
      Aeq = constraints$Aeq,
      beq = constraints$beq
    )

    data_out[pred_idx, lhsV] <- as.vector(model_parts$X_pred %*% beta)

    if (sum(is.na(lhs_vector)) > sum(pred_idx)) {
      message(paste(
        "There still missing values in variable",
        lhsV,
        ". Probably due to missing values in the regressors."
      ))
    }

    if (imp_var) {
      if (target_imp_col %in% colnames(data_out)) {
        warning(
          "The following TRUE/FALSE imputation status variables will be updated: ",
          target_imp_col
        )
      }
      data_out[[target_imp_col]] <- is.na(lhs_vector)
    }
  }

  data_out
}

.restricted_model_matrices <- function(lhs, rhs, data, train_idx, pred_idx) {
  form <- stats::as.formula(paste(lhs, "~", rhs))
  train_data <- data[train_idx, , drop = FALSE]
  pred_data <- data[pred_idx, , drop = FALSE]

  train_frame <- stats::model.frame(
    form,
    train_data,
    na.action = stats::na.fail
  )
  terms_obj <- stats::terms(train_frame)
  x_terms <- stats::delete.response(terms_obj)
  xlevels <- .getXlevels(terms_obj, train_frame)
  contrasts_arg <- attr(stats::model.matrix(x_terms, train_frame), "contrasts")

  pred_frame <- stats::model.frame(
    x_terms,
    pred_data,
    na.action = stats::na.pass,
    xlev = xlevels
  )

  X_train <- stats::model.matrix(
    x_terms,
    train_frame,
    contrasts.arg = contrasts_arg
  )
  X_pred <- stats::model.matrix(
    x_terms,
    pred_frame,
    contrasts.arg = contrasts_arg
  )

  list(
    X_train = X_train,
    y_train = stats::model.response(train_frame),
    X_pred = X_pred
  )
}

.restricted_validate_constraints <- function(rules, lhs, data_pred, X_pred,
                                             eps = 0.001) {
  p <- ncol(X_pred)
  empty <- list(
    C = matrix(numeric(0), nrow = 0L, ncol = p),
    d = numeric(0),
    Aeq = matrix(numeric(0), nrow = 0L, ncol = p),
    beq = numeric(0)
  )

  if (length(rules) == 0L || nrow(X_pred) == 0L) {
    return(empty)
  }

  exprs <- .restricted_validator_exprs(rules)
  uses_lhs <- vapply(exprs, function(expr) lhs %in% all.vars(expr), logical(1))
  if (!any(uses_lhs)) {
    return(empty)
  }

  constraints <- list()
  lhs_exprs <- exprs[uses_lhs]
  linear_flags <- rules$is_linear()[uses_lhs]

  if (any(linear_flags)) {
    linear <- .restricted_validator_mip_matrix(lhs_exprs[linear_flags])
    constraints[[length(constraints) + 1L]] <- .restricted_constraints_from_linear(
      linear = linear,
      lhs = lhs,
      data_pred = data_pred,
      X_pred = X_pred,
      eps = eps
    )
  }

  if (any(!linear_flags)) {
    for (expr in lhs_exprs[!linear_flags]) {
      conditional <- .restricted_conditional_linear_rule(expr, lhs)
      if (is.null(conditional)) {
        stop(
          "`regression_restricted()` currently supports only linear validation rules ",
          "or simple `if` rules involving the imputed variable `", lhs, "`.",
          call. = FALSE
        )
      }

      active <- .restricted_eval_condition(conditional$condition, data_pred)
      if (any(active)) {
        linear <- .restricted_validator_mip_matrix(list(conditional$consequence))
        constraints[[length(constraints) + 1L]] <- .restricted_constraints_from_linear(
          linear = linear,
          lhs = lhs,
          data_pred = data_pred,
          X_pred = X_pred,
          eps = eps,
          active = active
        )
      }
    }
  }

  .restricted_combine_constraints(constraints, p)
}

.restricted_constraints_from_linear <- function(linear, lhs, data_pred,
                                                X_pred, eps = 0.001,
                                                active = rep(TRUE, nrow(data_pred))) {
  p <- ncol(X_pred)
  empty <- list(
    C = matrix(numeric(0), nrow = 0L, ncol = p),
    d = numeric(0),
    Aeq = matrix(numeric(0), nrow = 0L, ncol = p),
    beq = numeric(0)
  )

  if (!any(active)) {
    return(empty)
  }

  A_rule <- linear$A
  b_rule <- linear$b
  operators <- linear$operator

  if (!(lhs %in% colnames(A_rule))) {
    return(empty)
  }

  relevant <- which(A_rule[, lhs] != 0)
  if (length(relevant) == 0L) {
    return(empty)
  }

  C_rows <- list()
  d_vals <- numeric(0)
  Aeq_rows <- list()
  beq_vals <- numeric(0)
  X_active <- X_pred[active, , drop = FALSE]
  data_active <- data_pred[active, , drop = FALSE]

  for (rule_pos in relevant) {
    coefs <- A_rule[rule_pos, , drop = FALSE]
    lhs_coef <- unname(coefs[1L, lhs])
    other_coefs <- coefs[1L, colnames(coefs) != lhs]
    names(other_coefs) <- colnames(coefs)[colnames(coefs) != lhs]
    other_coefs <- other_coefs[other_coefs != 0]

    missing_vars <- setdiff(names(other_coefs), colnames(data_pred))
    if (length(missing_vars) > 0L) {
      stop(
        "Validation rule for `",
        lhs,
        "` uses variables not found in `data`: ",
        paste(missing_vars, collapse = ", "),
        call. = FALSE
      )
    }

    if (length(other_coefs) > 0L) {
      other_data <- data_active[, names(other_coefs), drop = FALSE]
      if (anyNA(other_data)) {
        stop(
          "Validation rule for `",
          lhs,
          "` cannot be evaluated because constrained rows contain missing rule variables.",
          call. = FALSE
        )
      }
      row_offset <- as.vector(as.matrix(other_data) %*% as.numeric(other_coefs))
    } else {
      row_offset <- rep(0, nrow(data_active))
    }

    rhs <- b_rule[[rule_pos]] - row_offset
    lhs_rows <- lhs_coef * X_active

    if (identical(operators[[rule_pos]], "==")) {
      Aeq_rows[[length(Aeq_rows) + 1L]] <- lhs_rows
      beq_vals <- c(beq_vals, rhs)
    } else if (operators[[rule_pos]] %in% c("<=", "<")) {
      C_rows[[length(C_rows) + 1L]] <- lhs_rows
      d_vals <- c(d_vals, if (identical(operators[[rule_pos]], "<")) rhs - eps else rhs)
    } else {
      stop(
        "Unsupported validation operator `", operators[[rule_pos]], "`.",
        call. = FALSE
      )
    }
  }

  C <- if (length(C_rows) > 0L) do.call(rbind, C_rows) else empty$C
  Aeq <- if (length(Aeq_rows) > 0L) do.call(rbind, Aeq_rows) else empty$Aeq

  list(C = C, d = d_vals, Aeq = Aeq, beq = beq_vals)
}

.restricted_combine_constraints <- function(constraints, p) {
  constraints <- constraints[vapply(constraints, function(x) {
    nrow(x$C) > 0L || nrow(x$Aeq) > 0L
  }, logical(1))]

  if (length(constraints) == 0L) {
    return(list(
      C = matrix(numeric(0), nrow = 0L, ncol = p),
      d = numeric(0),
      Aeq = matrix(numeric(0), nrow = 0L, ncol = p),
      beq = numeric(0)
    ))
  }

  list(
    C = do.call(rbind, lapply(constraints, `[[`, "C")),
    d = unlist(lapply(constraints, `[[`, "d"), use.names = FALSE),
    Aeq = do.call(rbind, lapply(constraints, `[[`, "Aeq")),
    beq = unlist(lapply(constraints, `[[`, "beq"), use.names = FALSE)
  )
}

.restricted_conditional_linear_rule <- function(expr, lhs) {
  expr <- .restricted_consume_parentheses(expr)
  if (!is.call(expr) || !identical(as.character(expr[[1L]]), "if") || length(expr) != 3L) {
    return(NULL)
  }

  condition <- expr[[2L]]
  consequence <- expr[[3L]]
  if (lhs %in% all.vars(condition) || !(lhs %in% all.vars(consequence))) {
    return(NULL)
  }

  if (inherits(try(.restricted_linear_mip_rule(consequence), silent = TRUE), "try-error")) {
    return(NULL)
  }

  list(condition = condition, consequence = consequence)
}

.restricted_eval_condition <- function(condition, data) {
  value <- eval(condition, envir = data, enclos = parent.frame())
  if (length(value) == 1L) {
    value <- rep(value, nrow(data))
  }
  if (length(value) != nrow(data)) {
    stop("Conditional validation rule did not evaluate row-wise.", call. = FALSE)
  }
  if (anyNA(value)) {
    stop("Conditional validation rule contains missing condition values.", call. = FALSE)
  }
  as.logical(value)
}

.restricted_validator_mip_matrix <- function(rules) {
  exprs <- if (inherits(rules, "validator")) {
    .restricted_validator_exprs(rules)
  } else {
    as.list(rules)
  }

  exprs <- lapply(exprs, .restricted_rewrite_in_range)
  expanded <- .restricted_expand_rule_expressions(exprs)
  exprs <- expanded$exprs
  rule_names <- expanded$names

  mip_rules <- vector("list", length(exprs))
  for (i in seq_along(exprs)) {
    mip_rules[[i]] <- .restricted_rewrite_mip_rule(
      .restricted_linear_mip_rule(exprs[[i]], name = rule_names[[i]])
    )
  }

  variables <- sort(unique(unlist(lapply(mip_rules, function(rule) {
    names(rule$a)
  }), use.names = FALSE)))
  A <- matrix(
    0,
    nrow = length(mip_rules),
    ncol = length(variables),
    dimnames = list(rule = rule_names, variable = variables)
  )

  for (i in seq_along(mip_rules)) {
    A[i, names(mip_rules[[i]]$a)] <- mip_rules[[i]]$a
  }

  list(
    A = A,
    operator = vapply(mip_rules, `[[`, character(1), "op"),
    b = vapply(mip_rules, `[[`, numeric(1), "b")
  )
}

.restricted_validator_exprs <- function(rules) {
  rules$exprs(
    lin_eq_eps = 0,
    lin_ineq_eps = 0,
    replace_in = FALSE,
    vectorize = FALSE,
    expand_assignments = TRUE,
    expand_groups = TRUE
  )
}

.restricted_expand_rule_expressions <- function(exprs) {
  original_names <- names(exprs)
  if (is.null(original_names)) {
    original_names <- paste0("V", seq_along(exprs))
  }
  original_names[original_names == ""] <- paste0("V", which(original_names == ""))

  out_exprs <- list()
  out_names <- character(0)
  for (i in seq_along(exprs)) {
    parts <- .restricted_split_conjunction(exprs[[i]])
    part_names <- rep(original_names[[i]], length(parts))
    out_exprs <- c(out_exprs, parts)
    out_names <- c(out_names, part_names)
  }

  names(out_exprs) <- out_names
  list(exprs = out_exprs, names = out_names)
}

.restricted_split_conjunction <- function(expr) {
  expr <- .restricted_consume_parentheses(expr)
  if (is.call(expr) && as.character(expr[[1L]]) %in% c("&", "&&")) {
    return(c(
      .restricted_split_conjunction(expr[[2L]]),
      .restricted_split_conjunction(expr[[3L]])
    ))
  }
  list(expr)
}

.restricted_linear_mip_rule <- function(expr, sign = 1, name = "") {
  expr <- .restricted_consume_parentheses(expr)

  if (is.symbol(expr)) {
    return(.restricted_named_coef(sign, as.character(expr)))
  }
  if (is.numeric(expr) && length(expr) == 1L) {
    return(c(.b = sign * expr))
  }
  if (!is.call(expr)) {
    stop("Invalid linear validation rule.", call. = FALSE)
  }

  op <- as.character(expr[[1L]])

  if (op %in% c("==", ">", ">=", "<=", "<")) {
    coef <- c(
      .restricted_linear_mip_rule(expr[[2L]], sign = sign),
      .restricted_linear_mip_rule(expr[[3L]], sign = -sign),
      .b = 0
    )
    coef <- tapply(coef, names(coef), sum)
    constant <- unname(coef[[".b"]])
    a <- coef[names(coef) != ".b"]
    a <- a[a != 0]
    return(.restricted_mip_rule(a = a, op = op, b = -constant, rule = name))
  }

  if (op == "-") {
    if (length(expr) == 2L) {
      return(.restricted_linear_mip_rule(expr[[2L]], sign = -sign))
    }
    return(c(
      .restricted_linear_mip_rule(expr[[2L]], sign = sign),
      .restricted_linear_mip_rule(expr[[3L]], sign = -sign)
    ))
  }

  if (op == "+") {
    if (length(expr) == 2L) {
      return(.restricted_linear_mip_rule(expr[[2L]], sign = sign))
    }
    return(c(
      .restricted_linear_mip_rule(expr[[2L]], sign = sign),
      .restricted_linear_mip_rule(expr[[3L]], sign = sign)
    ))
  }

  if (op == "*") {
    left_constant <- .restricted_numeric_constant(expr[[2L]])
    if (!is.null(left_constant)) {
      return(.restricted_linear_mip_rule(expr[[3L]], sign = sign * left_constant))
    }
    right_constant <- .restricted_numeric_constant(expr[[3L]])
    if (!is.null(right_constant)) {
      return(.restricted_linear_mip_rule(expr[[2L]], sign = sign * right_constant))
    }
  }

  if (op == "/") {
    denominator <- .restricted_numeric_constant(expr[[3L]])
    if (!is.null(denominator) && denominator != 0) {
      return(.restricted_linear_mip_rule(expr[[2L]], sign = sign / denominator))
    }
  }

  stop("Invalid linear validation rule.", call. = FALSE)
}

.restricted_rewrite_mip_rule <- function(rule) {
  if (identical(rule$op, ">=")) {
    rule$a <- -rule$a
    rule$op <- "<="
    rule$b <- -rule$b
  } else if (identical(rule$op, ">")) {
    rule$a <- -rule$a
    rule$op <- "<"
    rule$b <- -rule$b
  }
  rule
}

.restricted_mip_rule <- function(a, op, b, rule) {
  structure(
    list(a = a, op = op, b = unname(b), rule = rule),
    class = "restricted_mip_rule"
  )
}

.restricted_named_coef <- function(value, name) {
  stats::setNames(value, name)
}

.restricted_consume_parentheses <- function(expr) {
  while (is.call(expr) && identical(as.character(expr[[1L]]), "(")) {
    expr <- expr[[2L]]
  }
  expr
}

.restricted_numeric_constant <- function(expr) {
  expr <- .restricted_consume_parentheses(expr)
  if (is.numeric(expr) && length(expr) == 1L) {
    return(expr)
  }
  NULL
}

.restricted_rewrite_in_range <- function(expr) {
  if (is.call(expr) &&
      identical(as.character(expr[[1L]]), "in_range") &&
      length(expr) == 4L) {
    return(substitute(
      (variable >= lower) & (variable <= upper),
      list(variable = expr[[2L]], lower = expr[[3L]], upper = expr[[4L]])
    ))
  }
  expr
}

.restricted_ecos_lm <- function(X, y, C, d, Aeq, beq) {
  solver <- .restricted_ecos_solver()

  X <- as.matrix(X)
  y <- as.numeric(y)
  p <- ncol(X)
  n <- nrow(X)

  C <- as.matrix(C)
  Aeq <- as.matrix(Aeq)

  if (nrow(C) != length(d)) {
    stop(
      "Internal error: inequality constraint dimensions do not match.",
      call. = FALSE
    )
  }
  if (nrow(Aeq) != length(beq)) {
    stop(
      "Internal error: equality constraint dimensions do not match.",
      call. = FALSE
    )
  }

  cvec <- c(rep(0, p), 1)
  G_soc <- cbind(rbind(rep(0, p), X), c(-1, rep(0, n)))
  h_soc <- c(0, y)

  if (nrow(C) > 0L) {
    G_linear <- cbind(C, rep(0, nrow(C)))
    G <- rbind(G_linear, G_soc)
    h <- c(d, h_soc)
  } else {
    G <- G_soc
    h <- h_soc
  }

  if (nrow(Aeq) > 0L) {
    A <- cbind(Aeq, rep(0, nrow(Aeq)))
    b <- beq
  } else {
    A <- NULL
    b <- NULL
  }

  dims <- list(l = as.integer(nrow(C)), q = as.integer(n + 1L), e = 0L)
  control <- if (is.function(solver$control)) {
    solver$control(verbose = 0L)
  } else {
    list()
  }
  solution <- solver$solve(
    c = cvec,
    G = G,
    h = h,
    dims = dims,
    A = A,
    b = b,
    control = control
  )

  exitflag <- solution$retcodes[["exitFlag"]]
  if (is.null(exitflag)) {
    exitflag <- solution$retcodes[["exitflag"]]
  }
  if (!identical(as.integer(exitflag), 0L)) {
    stop(
      "Restricted regression optimization failed for solver `",
      solver$package,
      "`: ",
      solution$infostring,
      call. = FALSE
    )
  }

  beta <- solution$x[seq_len(p)]
  names(beta) <- colnames(X)
  beta
}

.restricted_ecos_solver <- function() {
  if (requireNamespace("ECOSolveR", quietly = TRUE)) {
    ns <- asNamespace("ECOSolveR")
    control <- if (exists("ecos.control", envir = ns, inherits = FALSE)) {
      get("ecos.control", envir = ns)
    } else {
      NULL
    }
    return(list(
      package = "ECOSolveR",
      solve = get("ECOS_csolve", envir = ns),
      control = control
    ))
  }

  stop(
    "Package `ECOSolveR` is required for `regression_restricted()`.",
    call. = FALSE
  )
}
