# =============================================================================
# Chapter 1: Robust learner registration
# =============================================================================

# Registers robust mlr3 learners for regression and classification.
register_robust_learners <- function() {

  # Robust Regression Learner
  LearnerRegrRobustLM = R6::R6Class(
    classname = "LearnerRegrRobustLM",
    inherit = LearnerRegr,
    public = list(
      initialize = function() {
        param_set = ps(
          method = p_fct(c("M", "MM"), default = "MM"),
          psi = p_fct(c("bisquare", "lqq", "optimal"), default = "bisquare"),
          tuning.chi = p_dbl(lower = 0, upper = Inf, default = 1.55),
          tuning.psi = p_dbl(lower = 0, upper = Inf, default = 4.69),
          setting = p_fct(c("KS2014", "KS2011"), default = "KS2014"),
          max.it = p_int(lower = 1, upper = Inf, default = 50),
          k.max = p_int(lower = 1, upper = Inf, default = 200),
          nResample = p_int(lower = 1, upper = Inf, default = 500),
          subsampling = p_fct(c("simple", "nonsingular"), default = "nonsingular"),
          ridge_lambda = p_dbl(lower = 0, upper = 1, default = 1e-4),
          refine.tol = p_dbl(lower = 0, upper = Inf, default = 1e-7),
          solve.tol = p_dbl(lower = 0, upper = Inf, default = 1e-7),
          trace.lev = p_int(lower = 0, upper = Inf, default = 0)
        )

        super$initialize(
          id = "regr.lm_rob",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response"),
          packages = c("robustbase", "stats"),
          man = "robustbase::lmrob",
          param_set = param_set
        )

        self$param_set$values = list(
          method = "MM",
          psi = "bisquare",
          tuning.chi = 1.55,
          tuning.psi = 4.69,
          setting = "KS2014",
          max.it = 50,
          k.max = 200,
          nResample = 500,
          subsampling = "nonsingular",
          ridge_lambda = 1e-4,
          refine.tol = 1e-7,
          solve.tol = 1e-7,
          trace.lev = 0
        )
      }
    ),

    private = list(
      .train = function(task) {
        pv = self$param_set$get_values()
        data = as.data.frame(task$data())
        target = task$target_names
        features = task$feature_names

        sanitized = sanitize_model_features(data, target, features)
        data = sanitized$data
        features = sanitized$features
        self$state$feature_names = features
        self$state$factor_levels = sanitized$factor_levels

        if (length(sanitized$dropped) > 0L) {
          warning(sprintf(
            "Dropping constant or single-level predictors for '%s': %s",
            target, paste(sanitized$dropped, collapse = ", ")
          ))
        }

        formula = build_feature_formula(target, features)
        control = do.call(robustbase::lmrob.control, pv)

        model = tryCatch(
          robustbase::lmrob(formula, data = data, control = control),
          error = function(e) {
            warning(sprintf("lmrob() failed for '%s': %s\nFalling back to lm()", target, e$message))
            NULL
          }
        )

        if (is.null(model)) {
          # Fallback: lm
          model = lm(formula, data = data)
          class(model) = c("lm_fallback", class(model))
          self$state$used_fallback = TRUE
        } else {
          self$state$used_fallback = FALSE
        }

        return(model)
      },

      .predict = function(task) {
        model = self$model
        newdata = as.data.frame(task$data())
        feature_names = if (is.null(self$state$feature_names)) character(0) else self$state$feature_names

        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
              new_levels = setdiff(levels(newdata[[var]]), self$state$factor_levels[[var]])
              if (length(new_levels) > 0) {
                warning(sprintf("New levels (%s) in factor '%s' replaced with NA",
                                paste(new_levels, collapse = ", "), var))
              }
              newdata[[var]] = factor(newdata[[var]], levels = self$state$factor_levels[[var]])
            }
          }
        }
        newdata = newdata[, feature_names, drop = FALSE]

        response = tryCatch({
          predict(model, newdata = newdata)
        }, error = function(e) {
          warning("Vorhersage fehlgeschlagen: ", e$message)
          rep(NA_real_, nrow(newdata))
        })

        PredictionRegr$new(task = task, response = response)
      }
    )
  )

  mlr3::mlr_learners$add("regr.lm_rob", LearnerRegrRobustLM)

  LearnerClassifGlmRob <- R6::R6Class(
    inherit = mlr3::LearnerClassif,
    public = list(
      initialize = function() {
        super$initialize(
          id = "classif.glm_rob",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response", "prob"),
          packages = c("robustbase"),
          properties = c("twoclass", "multiclass")
        )
        self$state$models <- NULL
        self$state$classes <- NULL
      }
    ),
    private = list(
      .train = function(task) {
        data <- as.data.frame(task$data())
        y    <- task$truth()
        target_name <- task$target_names
        features <- task$feature_names
        classes <- task$class_names
        self$state$classes <- classes

        data[[target_name]] <- y
        sanitized <- sanitize_model_features(data, target_name, features)
        data <- sanitized$data
        features <- sanitized$features
        X <- data[, features, drop = FALSE]
        self$state$feature_names <- features
        self$state$factor_levels <- sanitized$factor_levels

        if (length(sanitized$dropped) > 0L) {
          warning(sprintf(
            "Dropping constant or single-level predictors for '%s': %s",
            target_name, paste(sanitized$dropped, collapse = ", ")
          ))
        }

        if (length(classes) == 2L) {
          # Binary case: one robust logistic model. glmrob(binomial) models
          # P(second factor level), so the constant fallback stores the same
          # quantity and .predict assembles the columns accordingly.
          df <- data.frame(y = as.factor(y), X)
          prob <- mean(as.character(df$y) == classes[2])
          if (prob %in% c(0, 1)) {
            mod <- new_constant_binomial_model(prob)
          } else {
            mod <- tryCatch(
              robustbase::glmrob(build_feature_formula("y", features), data = df, family = binomial()),
              error = function(e) glm(build_feature_formula("y", features), data = df, family = binomial())
            )
          }
          self$state$models <- list(mod)
        } else {
          # Multiclass: One-vs-Rest
          mods <- vector("list", length(classes))
          names(mods) <- classes
          for (k in classes) {
            # y_k = 1{y==k} vs rest
            yk <- factor(ifelse(y == k, 1L, 0L))
            df <- data.frame(y = yk, X)
            prob <- mean(df$y == 1L)
            if (prob %in% c(0, 1)) {
              mods[[k]] <- new_constant_binomial_model(prob)
            } else {
              mods[[k]] <- tryCatch(
                robustbase::glmrob(build_feature_formula("y", features), data = df, family = binomial()),
                error = function(e) glm(build_feature_formula("y", features), data = df, family = binomial())
              )
            }
          }
          self$state$models <- mods
        }
        invisible(TRUE)
      },

      .predict = function(task) {
        data <- as.data.frame(task$data(cols = task$feature_names))
        classes <- self$state$classes
        feature_names <- if (is.null(self$state$feature_names)) character(0) else self$state$feature_names

        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(data) && is.factor(data[[var]])) {
              data[[var]] <- factor(data[[var]], levels = self$state$factor_levels[[var]])
            }
          }
        }
        data <- data[, feature_names, drop = FALSE]

        if (length(classes) == 2L) {
          mod <- self$state$models[[1]]
          # P(second factor level) -- glmrob/glm binomial convention
          p2 <- predict_binomial_response(mod, data, fallback = 0.5)
          p2 <- sanitize_binary_probs(p2, fallback = 0.5)
          probs <- cbind(1 - p2, p2)
          colnames(probs) <- classes
          if (self$predict_type == "prob") {
            pred <- mlr3::PredictionClassif$new(task = task, prob = probs)
          } else {
            resp <- classes[max.col(probs, ties.method = "first")]
            pred <- mlr3::PredictionClassif$new(task = task, response = resp)
          }
          return(pred)
        } else {
          # Multiclass OvR
          mods <- self$state$models
          Pk   <- matrix(NA_real_, nrow(data), length(classes))
          colnames(Pk) <- classes
          for (k in classes) {
            Pk[, k] <- predict_binomial_response(mods[[k]], data, fallback = 0.5)
            Pk[, k] <- sanitize_binary_probs(Pk[, k], fallback = 0.5)
          }
          probs <- normalize_multiclass_probs(Pk)

          if (self$predict_type == "prob") {
            pred <- mlr3::PredictionClassif$new(task = task, prob = probs)
          } else {
            resp <- classes[max.col(probs, ties.method = "first")]
            pred <- mlr3::PredictionClassif$new(task = task, response = resp)
          }
          return(pred)
        }
      }
    )
  )

  mlr3::mlr_learners$add("classif.glm_rob", LearnerClassifGlmRob)

}

#
# task = mlr3::tsk("iris")$filter(1:1000)  # binary classification
# learner = mlr3::lrn("classif.glm_rob", predict_type = "prob")
# learner$train(task)
# pred = learner$predict(task)
# print(pred)

# =============================================================================
# Chapter 2: Data and model preparation
# =============================================================================

# Adds dummy rows so all factor levels are represented in the training data.
ensure_dummy_rows_for_factors <- function(dt, target_col) {
  dt <- data.table::copy(dt)

  factor_cols <- names(dt)[sapply(dt, is.factor)]
  factor_cols <- setdiff(factor_cols, target_col)

  for (col in factor_cols) {
    lvls <- levels(dt[[col]])
    present <- unique(dt[[col]])
    missing_lvls <- setdiff(lvls, present)

    if (length(missing_lvls) > 0) {
      for (lvl in missing_lvls) {
        dummy <- dt[1]
        for (fc in factor_cols) {
          dummy[[fc]] <- levels(dt[[fc]])[1]
        }
        dummy[[col]] <- lvl
        dummy[[target_col]] <- levels(dt[[target_col]])[1]
        dt <- rbind(dt, dummy)
      }
    }
  }
  dt
}
# Checks whether classification data should use a ranger fallback.
needs_ranger_classif <- function(y, X) {
  tab <- table(y)
  imbalance <- min(tab) / sum(tab) < 0.05
  high_dim  <- ncol(X) > nrow(X) / 5
  rare_levels <- any(sapply(X, function(col) {
    is.factor(col) && any(table(col) < 10)
  }))
  multicollinear <- ncol(X) > 1 && {
    mm <- model.matrix(~ ., data = X)
    qr(mm)$rank < ncol(mm)
  }
  imbalance || high_dim || rare_levels || multicollinear
}
# Drops constant predictors and normalizes factor levels for model fitting.
sanitize_model_features <- function(data, target, features) {
  data <- as.data.frame(data)

  if (target %in% names(data) && is.factor(data[[target]])) {
    data[[target]] <- droplevels(data[[target]])
  }

  keep_features <- character(0)
  dropped_features <- character(0)

  for (feature in features) {
    if (!feature %in% names(data)) next

    col <- data[[feature]]

    if (is.ordered(col)) {
      col <- factor(as.character(col))
      data[[feature]] <- col
    }

    if (is.factor(col)) {
      col <- droplevels(col)
      data[[feature]] <- col
      if (nlevels(col) >= 2L) {
        keep_features <- c(keep_features, feature)
      } else {
        dropped_features <- c(dropped_features, feature)
      }
    } else {
      n_unique <- length(unique(stats::na.omit(col)))
      if (n_unique >= 2L) {
        keep_features <- c(keep_features, feature)
      } else {
        dropped_features <- c(dropped_features, feature)
      }
    }
  }

  data <- data[, c(target, keep_features), drop = FALSE]
  factor_cols <- keep_features[vapply(data[, keep_features, drop = FALSE], is.factor, logical(1))]
  factor_levels <- lapply(data[, factor_cols, drop = FALSE], levels)

  list(
    data = data,
    features = keep_features,
    dropped = dropped_features,
    factor_levels = factor_levels
  )
}
# Builds a safe model formula from the target and feature list.
build_feature_formula <- function(target, features) {
  if (!length(features)) {
    return(as.formula(paste(target, "~ 1")))
  }
  reformulate(features, response = target)
}
# Creates a constant binomial fallback model for degenerate class data.
new_constant_binomial_model <- function(prob) {
  structure(list(prob = as.numeric(prob)), class = "vimpute_constant_binomial")
}
# Returns robust binomial probabilities with a fallback for prediction errors.
predict_binomial_response <- function(model, newdata, fallback = 0.5) {
  if (inherits(model, "vimpute_constant_binomial")) {
    return(rep(model$prob, nrow(newdata)))
  }

  tryCatch(
    as.numeric(stats::predict(model, newdata = newdata, type = "response")),
    error = function(e) rep(fallback, nrow(newdata))
  )
}
# Clips binary probabilities to valid numeric values.
sanitize_binary_probs <- function(p1, fallback = 0.5) {
  p1 <- as.numeric(p1)
  bad <- !is.finite(p1)
  if (any(bad)) {
    p1[bad] <- fallback
  }
  pmin(pmax(p1, 1e-6), 1 - 1e-6)
}
# Normalizes one-vs-rest probabilities into multiclass probabilities.
normalize_multiclass_probs <- function(Pk, fallback = NULL) {
  Pk <- as.matrix(Pk)
  if (!ncol(Pk)) {
    return(Pk)
  }

  bad <- !is.finite(Pk)
  if (any(bad)) {
    Pk[bad] <- 0
  }

  Pk <- pmin(pmax(Pk, 1e-6), 1 - 1e-6)
  Q <- Pk / (1 - Pk)
  row_sums <- rowSums(Q)
  invalid_rows <- !is.finite(row_sums) | row_sums <= 0

  probs <- Q
  if (any(!invalid_rows)) {
    probs[!invalid_rows, ] <- Q[!invalid_rows, , drop = FALSE] / row_sums[!invalid_rows]
  }

  if (is.null(fallback)) {
    fallback <- rep(1 / ncol(Pk), ncol(Pk))
  }
  fallback <- as.numeric(fallback)
  if (length(fallback) != ncol(Pk) || any(!is.finite(fallback)) || sum(fallback) <= 0) {
    fallback <- rep(1 / ncol(Pk), ncol(Pk))
  } else {
    fallback <- fallback / sum(fallback)
  }

  if (any(invalid_rows)) {
    probs[invalid_rows, ] <- matrix(
      rep(fallback, sum(invalid_rows)),
      nrow = sum(invalid_rows),
      byrow = TRUE
    )
  }

  probs
}
# =============================================================================
# Chapter 3: Formulas and transformations
# =============================================================================

# Detects transformations on the formula left-hand side, e.g. log or sqrt.
identify_lhs_transformation <- function(formula) {
  lhs <- as.character(formula)[2]  # Extract the left side of the formula

  # Permitted transformations
  transformations <- c("log", "sqrt", "exp", "I\\(1/", "boxcox")

  for (t in transformations) {
    if (grepl(paste0("^", t), lhs)) {
      return(ifelse(t == "I\\(1/", "inverse", gsub("\\\\", "", t)))  # Names "I(1/" as "inverse".
    }
  }

  return(NULL)  # No transformation
}
# Analyzes a formula for variables, transformations, operators, and missing values.
identify_variables <- function(formula, data, target_col) {
  data <- as.data.frame(data)

  if (is.list(formula)) {
    results <- lapply(formula, function(f) identify_variables(f, data, target_col))
    return(results)
  }

  # Extract formula as character string
  formchar <- as.character(formula)
  lhs <- gsub("^I\\(1/|log\\(|sqrt\\(|boxcox\\(|exp\\(|\\)$| ", "", formchar[2])   # Remove transformations and spaces from the left-hand side.
  rhs <- ifelse(length(formchar) > 2, gsub(" ", "", formchar[3]), "")

  # Decompose the right-hand side according to all relevant operators
  rhs_vars <- if (rhs != "") unlist(strsplit(rhs, "[-+*:/%()]")) else character(0)
  rhs_vars <- rhs_vars[rhs_vars != ""]

  # Extract the original variable names without transformations
  raw_lhs <- gsub("(log\\(|sqrt\\(|I\\(1/|boxcox\\(|exp\\(|\\))", "", lhs)
  raw_rhs_vars <- gsub("(log\\(|sqrt\\(|I\\(1/|boxcox\\(|exp\\(|\\))", "", rhs_vars)

  # Remove duplicate variable names
  raw_rhs_vars <- unique(raw_rhs_vars)

  # Identify transformations in the predictors and the response variable
  transformations <- c(
    ifelse(grepl("log\\(", formchar[2]), "log", "none"),
    sapply(rhs_vars, function(var) {
      if (grepl("log\\(", var)) return("log")
      if (grepl("sqrt\\(", var)) return("sqrt")
      if (grepl("^I\\(1/", var)) return("inverse")
      if (grepl("boxcox\\(", var)) return("boxcox")
      if (grepl("exp\\(", var)) return("exponential")
      return("none")
    }, USE.NAMES = FALSE)
  )

  # Identify model matrix operators
  operator_mapping <- list(
    ":" = "interaction",
    "*" = "crossing",
    "^" = "power",
    "%in%" = "nested",
    "/" = "sub-nested",
    "-" = "exclusion"
  )

  operators <- unique(unlist(regmatches(rhs, gregexpr("[:*^%in%/-]", rhs))))
  operator_types <- setNames(operators, sapply(operators, function(op) operator_mapping[[op]]))

  # Initialize empty lists if no predictors are available
  if (length(rhs_vars) == 0) {
    transformations <- character(0)
    raw_rhs_vars <- character(0)
  }

  # Identify the type of variable (select only existing columns)
  existing_vars <- c(raw_lhs, raw_rhs_vars)
  existing_vars <- setdiff(existing_vars, target_col)
  existing_vars <- existing_vars[existing_vars %in% colnames(data)]
  variable_types <- sapply(data[, existing_vars, drop = FALSE], class)

  # Check missing values in the predictors
  missing_values <- sapply(data[, intersect(raw_rhs_vars, colnames(data)), drop = FALSE], function(col) sum(is.na(col)))

  # Compile results
  result <- list(
    response_variable = raw_lhs,
    predictor_variables = existing_vars,
    transformations = setNames(transformations, c(lhs, rhs_vars)),
    variable_types = variable_types,
    missing_values = missing_values,
    model_matrix_operators = operator_types
  )

  return(result)
}
# Selects the matching formula for a target variable from a formula list.
select_formula <- function(formula_list, response_variable) {
  # Remove transformations and spaces from response variable
  response_variable_cleaned <- gsub("^I\\(1/|^log\\(|^sqrt\\(|^boxcox\\(|^exp\\(|\\)$| ", "", response_variable)

  selected_formula <- Filter(function(f) {
    # Remove transformations and spaces from the left-hand side of the formula
    formula_lhs <- gsub("^I\\(1/|^log\\(|^sqrt\\(|^boxcox\\(|^exp\\(|\\)$| ", "", deparse(f[[2]]))
    identical(formula_lhs, response_variable_cleaned)
  }, formula_list)

  if (length(selected_formula) == 0) return(FALSE)
  return(selected_formula[[1]])
}
# =============================================================================
# Chapter 4: Argument mapping and prechecks
# =============================================================================

# Maps PMM settings to imputable variables and disables PMM for non-numeric data.
map_pmm <- function(variables_NA, pmm, original_data) {

  user_set_pmm_per_variable <- is.list(pmm)

  # 1: Single logical
  if (is.logical(pmm) && length(pmm) == 1) {
    out <- setNames(as.list(rep(pmm, length(variables_NA))), variables_NA)

    invalid_numeric <- variables_NA[!vapply(original_data[, variables_NA, with=FALSE], is.numeric, logical(1))]
    for (v in invalid_numeric) out[[v]] <- FALSE

    return(out)
  }

  # 2: List
  if (is.list(pmm)) {

    nm <- names(pmm)
    if (is.null(nm)) stop("pmm as list must use variable names as list names.")

    unknown <- setdiff(nm, variables_NA)
    # if (length(unknown) > 0) {
    #   warning(sprintf("pmm contains names that are not NA-variables: %s (ignored).",
    #                   paste(unknown, collapse=", ")))
    # }

    out <- setNames(vector("list", length(variables_NA)), variables_NA)

    for (v in variables_NA) {
      if (!is.null(pmm[[v]])) {
        out[[v]] <- as.logical(pmm[[v]])
      } else {
        out[[v]] <- FALSE
      }

      # PMM only for numeric
      if (isTRUE(out[[v]]) && !is.numeric(original_data[[v]])) {

        # Warn ONLY if the user set pmm for this variable explicitly:
        if (user_set_pmm_per_variable && !is.null(pmm[[v]]) && isTRUE(pmm[[v]])) {
          warning(sprintf("PMM not possible for non-numeric variable '%s'. PMM disabled.", v))
        }

        out[[v]] <- FALSE
      }
      }
    return(out)
  }

  stop("pmm must be either a single logical or a named list of logicals.")
}
# Maps the PMM donor count k per variable while respecting active PMM flags.
map_pmm_k <- function(variables_NA, pmm_k, pmm) {

  user_set_k_per_variable <- is.list(pmm_k)   # TRUE = user explicitly set per-variable

  # 1) Single global integer → ALWAYS silent fallback for non-numeric vars
  if (is.numeric(pmm_k) && length(pmm_k) == 1) {
    if (pmm_k < 1 || pmm_k %% 1 != 0)
      stop("'pmm_k' must be a positive integer (>= 1).")

    out <- setNames(as.list(rep(as.integer(pmm_k), length(variables_NA))), variables_NA)

    for (v in variables_NA) {
      if (isFALSE(pmm[[v]])) {
        # PMM disabled for this variable:
        # but user set pmm_k globally → DO NOT WARN
        out[[v]] <- NULL
      }
    }
    return(out)
  }

  # 2) Named list case → validate per variable
  if (is.list(pmm_k)) {
    nm <- names(pmm_k)
    if (is.null(nm))
      stop("pmm_k as list must provide variable names.")

    unknown <- setdiff(nm, variables_NA)
    if (length(unknown) > 0) {
      warning(sprintf(
        "pmm_k list contains names not matching NA variables: %s (ignored).",
        paste(unknown, collapse = ", ")
      ))
    }

    out <- setNames(vector("list", length(variables_NA)), variables_NA)

    for (v in variables_NA) {

      if (isFALSE(pmm[[v]])) {
        # Only warn if user explicitly set pmm_k[[v]]
        if (!is.null(pmm_k[[v]])) {
          warning(sprintf("pmm_k specified for '%s' but PMM disabled. pmm_k ignored.", v))
        }
        out[[v]] <- NULL

      } else {
        # PMM enabled → validate
        if (!is.null(pmm_k[[v]])) {
          val <- as.integer(pmm_k[[v]])
          if (is.na(val) || val < 1L)
            stop(sprintf("pmm_k for '%s' must be a positive integer (>= 1).", v))
          out[[v]] <- val
        } else {
          out[[v]] <- 1L
        }
      }
    }
    return(out)
  }

  # 3) pmm_k missing → default behavior
  if (is.null(pmm_k)) {
    out <- setNames(vector("list", length(variables_NA)), variables_NA)
    for (v in variables_NA)
      out[[v]] <- if (isTRUE(pmm[[v]])) 1L else NULL
    return(out)
  }

  stop("pmm_k must be a single integer, a named list, or NULL.")
}
# Maps the PMM aggregation method per variable.
map_pmm_k_method <- function(variables_NA, pmm_k_method, pmm) {
  allowed <- c("mean", "median", "random")
  normalize_method <- function(x, var_name = NULL) {
    if (is.function(x)) {
      return(x)
    }

    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      if (is.null(var_name)) {
        stop("pmm_k_method must be one of: 'mean', 'median', 'random', or a function.")
      } else {
        stop(sprintf(
          "pmm_k_method for '%s' must be one of: 'mean', 'median', 'random', or a function.",
          var_name
        ))
      }
    }

    method <- tolower(x)
    if (!(method %in% allowed)) {
      if (is.null(var_name)) {
        stop("pmm_k_method must be one of: 'mean', 'median', 'random', or a function.")
      } else {
        stop(sprintf(
          "pmm_k_method for '%s' must be one of: 'mean', 'median', 'random', or a function.",
          var_name
        ))
      }
    }

    method
  }

  # 1) Single global method
  if (is.function(pmm_k_method)) {
    out <- setNames(as.list(rep(list(pmm_k_method), length(variables_NA))), variables_NA)

    for (v in variables_NA) {
      if (isFALSE(pmm[[v]])) {
        out[[v]] <- NULL
      }
    }
    return(out)
  }

  # 2) Single global method-name
  if (is.character(pmm_k_method) && length(pmm_k_method) == 1L) {
    method <- normalize_method(pmm_k_method)
    out <- setNames(as.list(rep(method, length(variables_NA))), variables_NA)

    for (v in variables_NA) {
      if (isFALSE(pmm[[v]])) {
        out[[v]] <- NULL
      }
    }
    return(out)
  }

  # 3) Named list case
  if (is.list(pmm_k_method)) {
    nm <- names(pmm_k_method)
    if (is.null(nm))
      stop("pmm_k_method as list must provide variable names.")

    unknown <- setdiff(nm, variables_NA)
    if (length(unknown) > 0) {
      warning(sprintf(
        "pmm_k_method list contains names not matching NA variables: %s (ignored).",
        paste(unknown, collapse = ", ")
      ))
    }

    out <- setNames(vector("list", length(variables_NA)), variables_NA)

    for (v in variables_NA) {
      if (isFALSE(pmm[[v]])) {
        if (!is.null(pmm_k_method[[v]])) {
          warning(sprintf("pmm_k_method specified for '%s' but PMM disabled. pmm_k_method ignored.", v))
        }
        out[[v]] <- NULL
      } else {
        if (!is.null(pmm_k_method[[v]])) {
          out[[v]] <- normalize_method(pmm_k_method[[v]], v)
        } else {
          out[[v]] <- "mean"
        }
      }
    }
    return(out)
  }

  # 4) pmm_k_method missing -> default behavior
  if (is.null(pmm_k_method)) {
    out <- setNames(vector("list", length(variables_NA)), variables_NA)
    for (v in variables_NA)
      out[[v]] <- if (isTRUE(pmm[[v]])) "mean" else NULL
    return(out)
  }

  stop("pmm_k_method must be a single method, a function, a named list, or NULL.")
}
# Maps tuning flags to variables with missing values.
map_tune <- function(variables_NA, tune) {

  # Single TRUE/FALSE → applies to all
  if (is.logical(tune) && length(tune) == 1) {
    return(setNames(as.list(rep(tune, length(variables_NA))), variables_NA))
  }

  if (is.list(tune)) {
    nm <- names(tune)
    if (is.null(nm))
      stop("tune as list must have variable names.")

    unknown <- setdiff(nm, variables_NA)
    if (length(unknown) > 0) {
      warning(sprintf("tune contains names that are not NA-variables: %s (ignored).",
                      paste(unknown, collapse=", ")))
    }

    out <- setNames(vector("list", length(variables_NA)), variables_NA)
    for (v in variables_NA) {
      if (!is.null(tune[[v]])) out[[v]] <- as.logical(tune[[v]]) else out[[v]] <- FALSE
    }
    return(out)
  }

  stop("tune must be a single logical or a named list.")
}
# Maps user-defined learner parameters by variable or method.
map_learner_params <- function(variables, method, learner_params) {

  # 0) Empty
  if (is.null(learner_params) || length(learner_params) == 0) {
    return(setNames(vector("list", length(variables)), variables))
  }

  # 1) Context
  method_names     <- unique(unlist(method))
  only_one_method  <- length(method_names) == 1

  nm       <- names(learner_params)
  nm_clean <- if (is.null(nm)) character(0) else nm[nzchar(nm) & !is.na(nm)]

  # Top-Level-Keys
  var_keys   <- intersect(nm_clean, variables)
  meth_keys  <- intersect(nm_clean, method_names)
  unknown    <- setdiff(nm_clean, c(variables, method_names))

  has_any_names          <- length(nm_clean) > 0
  has_var_keys           <- length(var_keys)  > 0
  has_meth_keys          <- length(meth_keys) > 0
  has_any_var_or_method  <- has_var_keys || has_meth_keys

  # 2) Global mode: only valid with one method and no variable or method key.
  if (!has_any_var_or_method) {
    if (only_one_method) {
      out <- setNames(vector("list", length(variables)), variables)
      for (v in variables) out[[v]] <- learner_params
      return(out)
    } else {
      # More Methods
      warning("Global learner_params cannot be applied because multiple methods are used. Parameters ignored.")
      return(setNames(vector("list", length(variables)), variables))
    }
  }

  # 3) Mixed (Variable- and Method-Keys at same time)
  if (has_var_keys && has_meth_keys) {
    warning("Mixed learner_params keys (variables AND methods) are not allowed. All learner_params were ignored. Hint: ensure no variable shares a name with a reserved method (ranger, xgboost, regularized, robust).")
    return(setNames(vector("list", length(variables)), variables))
  }

  # 4) Unknown Top-Level-Keys
  if (length(unknown) > 0) {
    warning(sprintf(
      "learner_params contain entries that do not match any NA-variable or method and will be ignored: %s",
      paste(unknown, collapse = ", ")
    ))
  }

  # 5) Variable mode
  if (has_var_keys && !has_meth_keys) {
    out <- setNames(vector("list", length(variables)), variables)
    for (v in variables) {
      out[[v]] <- if (v %in% var_keys) learner_params[[v]] else list()
    }
    return(out)
  }

  # 6) Method mode
  if (!has_var_keys && has_meth_keys) {
    out <- setNames(vector("list", length(variables)), variables)
    for (v in variables) {
      m <- method[[v]]
      out[[v]] <- if (!is.null(m) && (m %in% meth_keys)) learner_params[[m]] else list()
    }
    return(out)
  }

  # 7) Fallback
  setNames(vector("list", length(variables)), variables)
}
#
#
# Validates inputs, variable types, methods, formulas, PMM, and tuning before imputation.
precheck <- function(
    data,
    pmm,
    formula,
    method,
    sequential,
    pmm_k,
    pmm_k_method,
    learner_params,
    tune,
    boot = FALSE,
    robustboot = "stratified",
    uncert = "none",
    m = 1L,
    default_method = NULL,
    variables_NA = NULL,
    verbose = FALSE
) {
  # -------------------------------------------------------------------------
  # 1) Identify variables and variables containing missing values
  # -------------------------------------------------------------------------
  variables      <- colnames(data)
  if (is.null(variables_NA)) {
    variables_NA <- variables[apply(data, 2, function(x) any(is.na(x)))]
  } else {
    unknown_variables_NA <- setdiff(variables_NA, variables)
    if (length(unknown_variables_NA) > 0L) {
      message(sprintf(
        "Unknown variable name(s) in 'variables_NA': %s",
        paste(unknown_variables_NA, collapse = ", ")
      ))
    }
    variables_NA <- intersect(variables, variables_NA)
  }

  if (length(variables_NA) == 0) {
    stop("Error: No variables with missing data found.")
  }

  # -------------------------------------------------------------------------
  # 2) Warn if any variable name equals a reserved method name
  #    (this would cause ambiguity in learner_params)
  # -------------------------------------------------------------------------
  # Reserved method names used in learner_params mapping
  reserved_methods <- vimpute_methods()
  conflicting_vars <- intersect(variables, reserved_methods)
  if (length(conflicting_vars) > 0) {
    warning(sprintf(
      "Variable name(s) conflict with reserved method names: %s. ",
      paste(conflicting_vars, collapse = ", ")
    ))
  }

  # -------------------------------------------------------------------------
  # 3) Ensure data is a data.table
  # -------------------------------------------------------------------------
  # Normalize input type to data.table
  if (!is.data.table(data) && (is.matrix(data) || is.data.frame(data))) {
    data <- as.data.table(data)
  } else if (!is.data.table(data)) {
    stop("Error: Input must be a data.frame, matrix, or data.table.")
  }

  # -------------------------------------------------------------------------
  # 3b) Validate MI / bootstrap / uncertainty parameters
  # -------------------------------------------------------------------------
  if (!is.logical(boot) || length(boot) != 1L || is.na(boot)) {
    stop("'boot' must be TRUE or FALSE.")
  }
  # 'psi'/'quantile' robustboot strategies are currently disabled; fall back
  # to 'standard' with a warning rather than erroring (see bootstrap_resample).
  if (length(robustboot) == 1L && robustboot %in% c("psi", "quantile")) {
    warning(sprintf(
      "robustboot strategy '%s' is not available; falling back to 'standard'.",
      robustboot))
    robustboot <- "standard"
  }
  robustboot <- match.arg(robustboot, c("standard", "stratified", "residual"))
  uncert <- match.arg(uncert, c("none", "normalerror", "resid", "pmm", "midastouch"))
  if (length(m) != 1L || is.na(m) || !is.numeric(m) || m < 1 || m != as.integer(m)) {
    stop("'m' must be a positive integer.")
  }
  m <- as.integer(m)

  # -------------------------------------------------------------------------
  # 4) Validate formula list (if provided)
  # -------------------------------------------------------------------------
  if (!identical(formula, FALSE)) {
    if (!is.list(formula)) stop("'formula' must be FALSE or a named list of formulas.")

    for (idx in seq_along(formula)) {
      form <- formula[[idx]]
      form_label <- names(formula)[idx]
      if (is.null(form_label) || !nzchar(form_label)) {
        form_label <- deparse(form[[2]])
      }

      if (!inherits(form, "formula")) {
        stop(sprintf("Element for variable '%s' is not a valid formula.", form_label))
      }

      # Check that all variables referenced exist in the dataset
      form_vars <- all.vars(form)
      missing_vars <- setdiff(form_vars, names(data))
      if (length(missing_vars) > 0) {
        stop(sprintf("Formula for '%s' references unknown variables: %s",
                     form_label, paste(missing_vars, collapse=", ")))
      }
    }
  }

  # -------------------------------------------------------------------------
  # 5) Normalize 'method' argument so every NA-variable gets a valid method
  # -------------------------------------------------------------------------
  # Methods supported by vimpute(): everything in the method registry
  # (built-ins plus register_vimpute_method() additions)
  supported_methods <- vimpute_methods()

  if (length(method) == 0) {
    stop("No method specified. Please provide at least one method.")
  }

  # no default method
  if (is.null(default_method)) {
    m_vec <- unlist(method, use.names = FALSE)
    if (length(m_vec) > 0 && length(unique(m_vec)) == 1L) {
      default_method <- unique(m_vec)
    }
  }

  # Fallback → ranger
  if (is.null(default_method)) {
    default_method <- "ranger"
  }

  #  A: global method
  if (is.character(method) && length(method) == 1L) {

    if (!(method %in% supported_methods)) {
      stop(sprintf("Unsupported method '%s'. Registered methods: %s.",
                   method, paste(supported_methods, collapse = ", ")))
    }

    method <- setNames(as.list(rep(method, length(variables_NA))), variables_NA)

  } else if (is.list(method) && length(method) == 1L && is.character(method[[1]]) &&
             is.null(names(method))) {

    # only an UNNAMED length-1 list is a global method; a named length-1 list
    # (e.g. list(Dream = "gam")) must fall through to the per-variable branch
    # below so its name is honoured and validated (was silently applied to all
    # variables, discarding -- and never validating -- the name).
    single_method <- method[[1]]
    if (!(single_method %in% supported_methods)) {
      stop(sprintf("Unsupported method '%s'. Registered methods: %s.",
                   single_method, paste(supported_methods, collapse = ", ")))
    }

    method <- setNames(as.list(rep(single_method, length(variables_NA))), variables_NA)

    # B: Named list
  } else if (is.list(method) && !is.null(names(method))) {

    unknown <- setdiff(names(method), variables)
    if (length(unknown) > 0) {
      stop(sprintf(
        "Unknown variable name(s) in 'method': %s",
        paste(unknown, collapse = ", ")
      ))
    }

    all_methods <- unlist(method, use.names = FALSE)
    if (!all(all_methods %in% supported_methods)) {
      bad_methods <- unique(setdiff(all_methods, supported_methods))
      stop(sprintf("Unsupported method(s) in 'method': %s. Registered methods: %s.",
                   paste(bad_methods, collapse = ", "),
                   paste(supported_methods, collapse = ", ")))
    }

    # Final mapping
    full_method <- setNames(vector("list", length(variables_NA)), variables_NA)

    for (v in variables_NA) {
      if (!is.null(method[[v]])) {
        full_method[[v]] <- method[[v]]
      } else {
        full_method[[v]] <- default_method
        warning(sprintf(
          "No method specified for NA-variable '%s'. Using default method '%s'.",
          v, default_method
        ))
      }
    }

    method <- full_method

    # C: unnamed list
  } else if (is.list(method) &&
             (length(method) == length(variables) || length(method) == length(variables_NA)) &&
             is.null(names(method))) {

    all_methods <- unlist(method, use.names = FALSE)
    if (!all(all_methods %in% supported_methods)) {
      bad_methods <- unique(setdiff(all_methods, supported_methods))
      stop(sprintf("Unsupported method(s) in 'method': %s. Registered methods: %s.",
                   paste(bad_methods, collapse = ", "),
                   paste(supported_methods, collapse = ", ")))
    }

    # mapping: a list with one method per COLUMN aligns to the column positions
    # of the NA-variables; a list with one entry per NA-variable aligns in order.
    idx <- if (length(method) == length(variables)) {
      match(variables_NA, variables)
    } else {
      seq_along(variables_NA)
    }
    method <- setNames(as.list(all_methods[idx]), variables_NA)

  } else {
    stop("Invalid 'method' specification: must be a single method or a (named) list.")
  }

  if (!identical(formula, FALSE)) {
    formula_targets <- unique(vapply(formula, function(f) {
      gsub("^I\\(1/|^log\\(|^sqrt\\(|^boxcox\\(|^exp\\(|\\)$| ", "", deparse(f[[2]]))
    }, character(1)))

    formula_methods <- vimpute_formula_methods()
    for (target in formula_targets) {
      target_method <- if (target %in% names(method)) method[[target]] else default_method
      if (!is.null(target_method) && !target_method %in% formula_methods) {
        stop(sprintf(
          "A formula can only be used with methods that support formulas (%s).",
          paste(formula_methods, collapse = ", ")))
      }
    }
  }

  # -------------------------------------------------------------------------
  # 6) Per-method validate hooks from the registry
  # -------------------------------------------------------------------------
  # Each registered method may carry a validate(y_obs, data, variable) hook
  # (the glmnet and GAM-family suitability checks of the built-ins live in
  # their registry entries). A rejection warns with the hook's reason and
  # moves the variable to the fallback method, which is validated in turn
  # until a method accepts. A method without a learner for the variable's
  # target type is rejected the same way.
  for (var in variables_NA) {
    y_obs <- data[[var]][!is.na(data[[var]])]
    target_regr <- is.numeric(y_obs) || is.logical(y_obs)
    tried <- character(0)

    repeat {
      current <- method[[var]]
      entry <- get_vimpute_method(current)
      if (is.null(entry) || current %in% tried) break
      tried <- c(tried, current)

      verdict <- NULL
      wanted <- if (target_regr) "regr" else "classif"
      if (is.null(entry$learner[[wanted]])) {
        verdict <- list(reason = sprintf(
          "Method '%s' has no %s learner for target '%s'. Falling back to '%s'.",
          current, if (target_regr) "regression" else "classification",
          var, entry$fallback))
      } else if (is.function(entry$validate)) {
        verdict <- entry$validate(y_obs, data, var)
      }

      if (is.null(verdict)) break
      if (is.character(verdict)) verdict <- list(reason = verdict)
      warning(verdict$reason)
      method[[var]] <- if (!is.null(verdict$fallback)) verdict$fallback else entry$fallback
    }
  }

  # -------------------------------------------------------------------------
  # 7) Warning for variables with > 50% missing values
  # -------------------------------------------------------------------------
  missing_counts <- colSums(is.na(data))
  na_cols <- names(missing_counts[missing_counts > 0.5 * nrow(data)])
  if (length(na_cols) > 0) {
    warning(sprintf("Variables with more than 50%% missing values: %s", paste(na_cols, collapse=", ")))
  }

  # -------------------------------------------------------------------------
  # 8) Standardize datatypes (numeric, factor, etc.)
  # -------------------------------------------------------------------------
  ordered_cols <- names(data)[sapply(data, inherits, "ordered")]
  if (length(ordered_cols) > 0) {
    data[, (ordered_cols) := lapply(.SD, function(x) factor(as.character(x))), .SDcols=ordered_cols]
  }

  data[, (variables) := lapply(.SD, function(x) {
    if (is.numeric(x)) {
      as.numeric(x)
    } else if (is.character(x)) {
      as.factor(x)
    } else if (is.logical(x)) {
      as.numeric(x)
    } else if (is.factor(x)) {
      x
    } else {
      stop("Unknown datatype encountered.")
    }
  }), .SDcols = variables]

  # -------------------------------------------------------------------------
  # 9) Map learner_params (complex logic, separate helper)
  # -------------------------------------------------------------------------
  checked_learner_params <- map_learner_params(
    variables      = variables_NA,
    method         = method,
    learner_params = learner_params
  )

  # -------------------------------------------------------------------------
  # 10) Map pmm / pmm_k / tune using flexible rules
  # -------------------------------------------------------------------------
  checked_pmm   <- map_pmm(variables_NA, pmm,  original_data = data)
  checked_pmm_k <- map_pmm_k(variables_NA, pmm_k, checked_pmm)
  checked_pmm_k_method <- map_pmm_k_method(variables_NA, pmm_k_method, checked_pmm)
  checked_tune  <- map_tune(variables_NA, tune)

  return(list(
    data           = data,
    variables      = variables,
    variables_NA   = variables_NA,
    method         = method,
    learner_params = checked_learner_params,
    pmm            = checked_pmm,
    pmm_k          = checked_pmm_k,
    pmm_k_method   = checked_pmm_k_method,
    tune           = checked_tune,
    boot           = boot,
    robustboot     = robustboot,
    uncert         = uncert,
    m              = m
  ))
}
# =============================================================================
# Chapter 5: Factor levels, value ranges, and result formatting
# =============================================================================

# Detects semicontinuous numeric variables with a concentration at zero.
is_semicontinuous <- function(x) {
  is.numeric(x) &&
    any(x == 0, na.rm = TRUE) &&
    any(x > 0, na.rm = TRUE) &&
    sum(x == 0, na.rm = TRUE) / sum(!is.na(x)) > 0.1 &&
    !all(na.omit(x) %in% c(0, 1))  # check that it is not binary
}

# Enforces original factor levels in a result dataset.
enforce_factor_levels <- function(df, original_levels) {
  for (colname in names(original_levels)) {
    levels <- original_levels[[colname]]
    if (is.null(levels)) next  # skip numeric variables

    if (colname %in% names(df)) {
      if (is.factor(df[[colname]]) || is.character(df[[colname]])) {
        vals <- df[[colname]]
        unknown_levels <- setdiff(unique(vals[!is.na(vals)]), levels)

        if (length(unknown_levels) > 0) {
          warning(sprintf("Column '%s' has unknown level: %s",
                          colname, paste(unknown_levels, collapse = ", ")))
        }

        df[[colname]] <- factor(vals, levels = levels)
      }
    }
  }
  return(df)
}

# Restore the `ordered` class (and original level order) on columns that were
# ordered factors in the user's input but got coerced to plain factors during
# internal processing (precheck() flattens ordered -> factor(as.character(.))).
# `ordered_info` maps column name -> the input's ordered levels. Values are
# matched by label, so this only corrects the class/level-order metadata; the
# imputed values themselves are unchanged. Works on data.frame and data.table.
restore_ordered_factors <- function(df, ordered_info) {
  if (length(ordered_info) == 0L) return(df)
  is_dt <- data.table::is.data.table(df)
  for (colname in names(ordered_info)) {
    if (colname %in% names(df)) {
      newcol <- factor(df[[colname]], levels = ordered_info[[colname]], ordered = TRUE)
      if (is_dt) data.table::set(df, j = colname, value = newcol) else df[[colname]] <- newcol
    }
  }
  df
}

# When considered_variables is a strict subset of the input, re-attach the
# untouched (non-considered) columns so vimpute() returns the full dataset,
# matching kNN/hotdeck/irmi and mice. `result` holds the imputed considered
# columns (plus any *_imp indicators); `data_all_variables` is the pristine
# full input. Considered columns take their imputed values from `result`;
# non-considered columns pass through unchanged (NAs included); *_imp columns
# stay at the end. Original input column order is preserved for data columns.
merge_passthrough_columns <- function(result, data_all_variables, considered_variables) {
  if (is.null(data_all_variables)) return(result)
  passthrough <- setdiff(names(data_all_variables), considered_variables)
  if (length(passthrough) == 0L) return(result)  # nothing was excluded
  result <- as.data.table(result)
  full <- copy(as.data.table(data_all_variables))
  for (v in considered_variables) {
    if (v %in% names(result)) data.table::set(full, j = v, value = result[[v]])
  }
  extra <- setdiff(names(result), names(full))  # e.g. *_imp indicator columns
  if (length(extra)) full <- cbind(full, result[, extra, with = FALSE])
  full
}

# Stops on factor-level mismatches between data and reference levels.
check_all_factor_levels <- function(df, factor_levels) {
  for (var in names(factor_levels)) {
    if (var %in% names(df) && is.factor(df[[var]])) {
      missing_levels <- setdiff(levels(df[[var]]), factor_levels[[var]])
      new_levels <- setdiff(factor_levels[[var]], levels(df[[var]]))
      if (length(missing_levels) > 0 || length(new_levels) > 0) {
        stop(sprintf(
          "Level mismatch in variable '%s':\n  Levels in data: %s\n  Expected levels: %s",
          var,
          paste(levels(df[[var]]), collapse = ", "),
          paste(factor_levels[[var]], collapse = ", ")
        ))
      }
    }
  }
}

# UNUSED: Replaced new factor levels with NA; currently not called anywhere.
# set_new_levels_to_na <- function(df, factor_levels, data_y_fill_final, skip_methods = c("xgboost"), method_var = NULL) {
#   if (!is.null(method_var) && !(method_var %in% skip_methods)) {
#     for (col in names(factor_levels)) {
#       if (col %in% names(df) && is.factor(df[[col]]) && col %in% names(data_y_fill_final)) {
#         valid_levels <- levels(data_y_fill_final[[col]])
#         df[[col]][!df[[col]] %in% valid_levels & !is.na(df[[col]])] <- NA
#       }
#     }
#   }
#   return(df)
# }

# UNUSED: Warned about factor-level mismatches; currently not called anywhere.
# check_factor_levels <- function(data, original_levels) {
#   for (colname in names(original_levels)) {
#     if (colname %in% names(data)) {
#       if (is.factor(data[[colname]])) {
#         data_levels <- levels(data[[colname]])
#         ref_levels <- original_levels[[colname]]
#
#         missing_levels <- setdiff(ref_levels, data_levels)
#         if (length(missing_levels) > 0) {
#           warning(sprintf("Column '%s': Missing levels in factor: %s", colname, paste(missing_levels, collapse = ", ")))
#         }
#
#         new_levels <- setdiff(data_levels, ref_levels)
#         if (length(new_levels) > 0) {
#           warning(sprintf("Column '%s': New levels in factor: %s", colname, paste(new_levels, collapse = ", ")))
#         }
#
#         invalid_values <- setdiff(unique(as.character(data[[colname]])), ref_levels)
#         if (length(invalid_values) > 0) {
#           warning(sprintf("Column '%s': Invalid value: %s", colname, paste(invalid_values, collapse = ", ")))
#         }
#       }
#     }
#   }
# }

# Back-transforms model predictions to the original scale.
inverse_transform <- function(x, method) {
  switch(method,
         exp = log(x),
         log = exp(x),
         sqrt = x^2,
         inverse = 1 / x,
         stop("Unknown transformation: ", method)
  )
}
# Determines the number of visible decimal places for a numeric value.
get_decimal_places <- function(x) {
  if (is.na(x)) return(0)
  if (x == floor(x)) return(0)
  nchar(sub(".*\\.", "", as.character(x)))
}
# Computes ranger regression predictions as the median across tree predictions.
predict_ranger_median <- function(graph_learner, newdata, target_name = NULL) {
  model_names <- names(graph_learner$model)
  ranger_idx <- grep("regr\\.ranger$", model_names)
  if (length(ranger_idx) == 0) {
    return(NULL)
  }

  ranger_model <- graph_learner$model[[ranger_idx[1]]]$model
  if (is.list(ranger_model) && !inherits(ranger_model, "ranger") && "model" %in% names(ranger_model)) {
    ranger_model <- ranger_model$model
  }
  if (!inherits(ranger_model, "ranger")) {
    return(NULL)
  }

  pred_dt <- as.data.table(newdata)
  if (!is.null(target_name) && target_name %in% colnames(pred_dt)) {
    pred_dt <- pred_dt[, setdiff(colnames(pred_dt), target_name), with = FALSE]
  }

  tree_preds <- predict(ranger_model, data = as.data.frame(pred_dt), predict.all = TRUE)$predictions
  if (is.null(dim(tree_preds))) {
    return(as.numeric(tree_preds))
  }
  if (length(dim(tree_preds)) != 2) {
    return(NULL)
  }

  apply(tree_preds, 1, median)
}

# =============================================================================
# Chapter 6: Bootstrap, uncertainty, and donor selection
# =============================================================================

#' Bootstrap resampling with robust strategies
#'
#' Returns bootstrap row indices based on the chosen strategy.
#' Strategies adapted from imputeRobust (Templ 2024).
#'
#' @param n Number of observations
#' @param strategy One of "standard", "stratified", "residual"
#' @param weights Robustness weights from model (currently unused in active strategies).
#' @param residuals Model residuals. Used by "stratified" and "residual".
#' @param alpha Fraction of "good" observations (default 0.75). Used by "stratified".
#' @param best_subset Integer indices of best observations (currently unused).
#' @return Integer vector of length n with bootstrap row indices
#' @keywords internal
# Creates bootstrap indices using standard, stratified, or residual-weighted sampling.
bootstrap_resample <- function(
    n,
    strategy = "stratified",
    weights = NULL,
    residuals = NULL,
    alpha = 0.75,
    best_subset = NULL
) {
  # 'psi' and 'quantile' strategies are currently disabled (not reliable
  # enough); fall back to 'standard' with a warning instead of erroring.
  if (length(strategy) == 1L && strategy %in% c("psi", "quantile")) {
    warning(sprintf(
      "robustboot strategy '%s' is not available; falling back to 'standard'.",
      strategy))
    strategy <- "standard"
  }
  strategy <- match.arg(strategy, c("standard", "stratified", "residual"))

  if (strategy == "standard") {
    # Classical bootstrap: sample all rows uniformly with replacement.
    return(sample.int(n, size = n, replace = TRUE))
  }

  # Disabled for now: 'quantile' is not fully wired in vimpute().
  # if (strategy == "quantile") {
  #   # Prefer observations with high robustness weight or from the best subset.
  #   if (!is.null(best_subset)) {
  #     return(sample(best_subset, size = n, replace = TRUE))
  #   }
  #   if (!is.null(weights)) {
  #     return(sample.int(n, size = n, replace = TRUE, prob = weights))
  #   }
  #   warning("'quantile' strategy requires weights or best_subset. Falling back to 'standard'.")
  #   return(sample.int(n, size = n, replace = TRUE))
  # }

  if (is.null(residuals)) {
    warning("Bootstrap strategy '", strategy, "' requires residuals. Falling back to 'standard'.")
    return(sample.int(n, size = n, replace = TRUE))
  }

  if (strategy == "stratified") {
    # Split into small- and large-error groups using absolute residual size.
    abs_residuals <- abs(residuals)
    threshold <- quantile(abs_residuals, alpha)
    good_idx <- which(abs_residuals <= threshold)
    bad_idx <- which(abs_residuals > threshold)
    n_good <- round(n * alpha)
    n_bad <- n - n_good
    if (length(good_idx) == 0) good_idx <- seq_len(n)
    if (length(bad_idx) == 0) bad_idx <- seq_len(n)
    idx <- c(
      sample(good_idx, size = n_good, replace = TRUE),
      sample(bad_idx, size = n_bad, replace = TRUE)
    )
    return(idx)
  }

  if (strategy == "residual") {
    # Downweight observations with large absolute residuals.
    prob <- max(abs(residuals)) - abs(residuals)
    prob[prob <= 0] <- .Machine$double.eps
    return(sample.int(n, size = n, replace = TRUE, prob = prob))
  }

  # Disabled for now: 'psi' weighting is currently not reliable enough for use.
  # if (strategy == "psi") {
  #   # Convert residuals to Tukey-style robust weights, then sample by them.
  #   u <- residuals / mad(residuals)
  #   c_tukey <- 4.685
  #   w <- ifelse(abs(u) > c_tukey, 0, (1 - (u^2) / c_tukey^2)^2)
  #   prob <- max(abs(w)) - abs(w)
  #   prob[prob <= 0] <- .Machine$double.eps
  #   return(sample.int(n, size = n, replace = TRUE, prob = prob))
  # }
}

# Per-variable imputation-quality metric from the final trained learner
# (audit P1.27, missForest OOBerror analogue): NRMSE for numeric targets,
# PFC for factors. ranger delivers a free out-of-bag version (type "oob",
# from its stored OOB predictions); every other learner is scored in-sample
# and honestly labelled (type "insample").
compute_model_error <- function(learner, task) {
  # Only ranger is special-cased below, so stopping the unwrap early on any
  # other object is harmless -- it falls through to the in-sample path.
  raw_model <- unwrap_raw_model(learner)

  truth <- task$truth()
  is_regr <- identical(task$task_type, "regr")
  nrmse_of <- function(rmse) {
    s <- stats::sd(as.numeric(truth))
    rmse / (if (is.finite(s) && s > 0) s else 1)
  }

  # ranger: out-of-bag predictions come free with the fitted forest
  if (inherits(raw_model, "ranger") && !is.null(raw_model$predictions)) {
    oob <- raw_model$predictions
    if (is_regr && is.numeric(oob)) {
      ok <- is.finite(oob)
      if (any(ok)) {
        rmse <- sqrt(mean((as.numeric(truth)[ok] - oob[ok])^2))
        return(list(measure = "NRMSE", value = nrmse_of(rmse), type = "oob"))
      }
    } else if (!is_regr) {
      # probability forest: OOB class = column with the largest probability
      if (is.matrix(oob)) {
        ok <- stats::complete.cases(oob)
        if (any(ok)) {
          pred_class <- colnames(oob)[max.col(oob[ok, , drop = FALSE])]
          return(list(
            measure = "PFC",
            value = mean(pred_class != as.character(truth)[ok]),
            type = "oob"
          ))
        }
      } else if (is.factor(oob) || is.character(oob)) {
        ok <- !is.na(oob)
        if (any(ok)) {
          return(list(
            measure = "PFC",
            value = mean(as.character(oob)[ok] != as.character(truth)[ok]),
            type = "oob"
          ))
        }
      }
    }
  }

  # everything else: in-sample error, labelled as such
  pred <- tryCatch(learner$predict(task), error = function(e) NULL)
  if (is.null(pred)) return(NULL)
  if (is_regr) {
    resp <- as.numeric(pred$response)
    ok <- is.finite(resp)
    if (!any(ok)) return(NULL)
    rmse <- sqrt(mean((as.numeric(pred$truth)[ok] - resp[ok])^2))
    list(measure = "NRMSE", value = nrmse_of(rmse), type = "insample")
  } else {
    resp <- as.character(pred$response)
    ok <- !is.na(resp)
    if (!any(ok)) return(NULL)
    list(measure = "PFC",
         value = mean(resp[ok] != as.character(pred$truth)[ok]),
         type = "insample")
  }
}

#' Extract model diagnostics for bootstrap strategies
#'
#' Drills into an mlr3 learner or raw model to retrieve residuals,
#' scale estimate, and robustness weights needed for bootstrap resampling.
#'
#' @param model A fitted model object (lm, lmrob, gam, ranger, or mlr3 GraphLearner)
#' @param method The vimpute method string: "robust", "ranger", "xgboost", "regularized"
#' @return List with components: residuals (numeric vector or NULL),
#'   scale (numeric scalar or NULL), weights (numeric vector or NULL)
#' @keywords internal
# Extracts residuals, scale estimates, and weights from trained models or learners.
extract_model_info <- function(model, method = "robust") {
  info <- list(residuals = NULL, scale = NULL, weights = NULL)

  # Unwrap mlr3 GraphLearner if needed
  raw_model <- model
  if (inherits(model, "GraphLearner") || inherits(model, "Learner")) {
    if (!is.null(model$model)) {
      model_list <- model$model
      for (nm in names(model_list)) {
        inner <- model_list[[nm]]
        if (inherits(inner, "list") && "model" %in% names(inner)) {
          raw_model <- inner$model
          break
        }
        if (inherits(inner, c("lm", "lmrob", "glmrob", "gam", "ranger"))) {
          raw_model <- inner
          break
        }
      }
    }
  }

  if (inherits(raw_model, "lmrob")) {
    info$residuals <- tryCatch(as.numeric(residuals(raw_model)), error = function(e) NULL)
    info$scale <- tryCatch(summary(raw_model)$scale, error = function(e) {
      # summary.lmrob can fail with small n; fallback to residual SD
      tryCatch(sd(residuals(raw_model)), error = function(e2) NULL)
    })
    info$weights <- tryCatch(raw_model$rweights, error = function(e) NULL)
    return(info)
  }

  if (inherits(raw_model, "glmrob")) {
    info$residuals <- tryCatch(as.numeric(residuals(raw_model)), error = function(e) NULL)
    info$scale <- tryCatch(summary(raw_model)$dispersion, error = function(e) NULL)
    return(info)
  }

  if (inherits(raw_model, "lm")) {
    info$residuals <- as.numeric(residuals(raw_model))
    info$scale <- sd(residuals(raw_model))
    return(info)
  }

  if (inherits(raw_model, "gam")) {
    info$residuals <- as.numeric(residuals(raw_model))
    info$scale <- tryCatch(sqrt(summary(raw_model)$scale), error = function(e) sd(residuals(raw_model)))
    return(info)
  }

  # Handle robGAM model info (stored as list with $mod)
  if (is.list(raw_model) && !is.null(raw_model$mod)) {
    mod <- raw_model$mod
    info$residuals <- tryCatch(as.numeric(residuals(mod)), error = function(e) NULL)
    info$scale <- if (!is.null(raw_model$scale)) raw_model$scale else
      tryCatch(sqrt(summary(mod)$scale), error = function(e) sd(residuals(mod)))
    if (!is.null(raw_model$weights)) info$weights <- raw_model$weights
    if (!is.null(raw_model$subset_good)) info$best_subset <- raw_model$subset_good
    return(info)
  }

  # For ranger/xgboost: no residuals from model object.
  # Bootstrap uses "standard" strategy only.
  info
}

#' Complete model diagnostics from learner predictions when the raw model
#' does not expose them directly
#'
#' @param info Result from extract_model_info()
#' @param learner A trained mlr3 learner or GraphLearner
#' @param task A regression task used for training the learner
#' @return Model info list with residuals/scale filled where possible
#' @keywords internal
# Completes missing model diagnostics using training predictions.
complete_model_info <- function(info, learner = NULL, task = NULL) {
  if (is.null(info) || !is.list(info)) {
    info <- list(residuals = NULL, scale = NULL, weights = NULL)
  }

  needs_residuals <- is.null(info$residuals) || !is.numeric(info$residuals) || length(info$residuals) == 0L
  needs_scale <- is.null(info$scale) || !is.numeric(info$scale) || length(info$scale) != 1L ||
    !is.finite(info$scale) || info$scale <= 0

  if ((!needs_residuals && !needs_scale) || is.null(learner) || is.null(task) || !inherits(task, "TaskRegr")) {
    return(info)
  }

  truth <- tryCatch(as.numeric(task$truth()), error = function(e) NULL)

  # Prefer out-of-bag predictions where the fitted model exposes them. A
  # forest's in-sample predictions are near-interpolating, so their spread
  # understates the predictive spread about twofold -- uncert = "normalerror"
  # would then draw N(0, sigma_hat) at half the right scale, and "resid" would
  # sample from a residual pool that is far too tight. Learners exposing a
  # proper scale of their own (lm, lmrob, gam, ...) never reach this path.
  preds <- oob_predictions(learner, n = length(truth))
  if (is.null(preds)) {
    preds <- tryCatch(as.numeric(learner$predict(task)$response),
                      error = function(e) NULL)
  }

  if (is.null(truth) || is.null(preds) || length(truth) != length(preds)) {
    return(info)
  }

  residuals <- truth - preds
  residuals <- residuals[is.finite(residuals)]
  if (!length(residuals)) {
    return(info)
  }

  if (needs_residuals) {
    info$residuals <- residuals
  }

  if (needs_scale) {
    scale_est <- stats::sd(residuals)
    if (!is.finite(scale_est) || scale_est <= 0) {
      scale_est <- sqrt(mean(residuals^2))
    }
    if (is.finite(scale_est) && scale_est > 0) {
      info$scale <- scale_est
    }
  }

  info
}

#' Out-of-bag predictions of a fitted learner, when it exposes them
#'
#' ranger stores the out-of-bag predictions of the training rows on the fitted
#' object, in training-row order and at no extra cost. They estimate the true
#' predictive spread to within ~1% where the in-sample predictions understate
#' it about twofold. Returns NULL whenever they are unavailable, misaligned, or
#' too sparse to estimate a scale from, so callers fall back to their own path.
#'
#' @param learner A trained mlr3 learner or GraphLearner
#' @param n Number of training rows the predictions must align with
#' @return Numeric vector of length `n` (possibly with non-finite entries for
#'   rows that were in-bag in every tree), or NULL
#' @keywords internal
oob_predictions <- function(learner, n) {
  tryCatch({
    raw_model <- unwrap_raw_model(learner)
    if (!inherits(raw_model, "ranger")) return(NULL)
    oob <- raw_model$predictions
    # probability forests store a class-probability matrix here, not a vector
    if (!is.numeric(oob) || is.matrix(oob) || length(oob) != n) return(NULL)
    # rows in-bag in every tree carry no OOB prediction; with few trees that can
    # leave too little to estimate a scale from
    if (sum(is.finite(oob)) < max(10L, 0.5 * n)) return(NULL)
    oob
  }, error = function(e) NULL)
}

#' Unwrap a fitted mlr3 learner to its underlying model object
#'
#' Depending on the GraphLearner shape (bare learner vs preprocessing
#' pipeops) the learner state nests one level deeper, so descend through
#' "model" elements until a non-state object is reached (bounded; stops on
#' ranger / data.frame / non-list).
#'
#' @keywords internal
unwrap_raw_model <- function(learner) {
  raw_model <- learner$model
  guard <- 0L
  while (is.list(raw_model) && !inherits(raw_model, "ranger") &&
         !is.data.frame(raw_model) && guard < 5L) {
    nxt <- if ("model" %in% names(raw_model)) {
      raw_model[["model"]]
    } else {
      found <- NULL
      for (nm in names(raw_model)) {
        inner <- raw_model[[nm]]
        if (is.list(inner) && "model" %in% names(inner)) {
          found <- inner[["model"]]
          break
        }
      }
      found
    }
    if (is.null(nxt)) break
    raw_model <- nxt
    guard <- guard + 1L
  }
  raw_model
}

#' Predicted donor scores for true PMM
#'
#' Scores the observed rows of a target with the trained learner so that
#' PMM donors are matched on their predicted values (Little 1988), not on
#' their observed values. For a ranger fit without bootstrap resampling the
#' honest out-of-bag predictions are used; otherwise the trained learner
#' predicts the observed rows (aregImpute-style when the fit is a bootstrap
#' refit). Returns NULL when the rows cannot be scored.
#'
#' @keywords internal
pmm_observed_scores <- function(learner, data_temp, obs_idx, feature_cols,
                                target_col, factor_levels, method_var,
                                boot, lhs_transformation = NULL) {
  finish <- function(scores) {
    scores <- as.numeric(scores)
    if (!is.null(lhs_transformation)) {
      scores <- inverse_transform(scores, lhs_transformation)
    }
    scores
  }

  # ranger without bootstrap: the fitted forest's out-of-bag predictions
  # align 1:1 with the training rows (= the observed rows, in data order)
  if (identical(method_var, "ranger") && !isTRUE(boot)) {
    oob <- tryCatch({
      raw_model <- unwrap_raw_model(learner)
      if (inherits(raw_model, "ranger") &&
          is.numeric(raw_model$predictions) &&
          length(raw_model$predictions) == length(obs_idx)) {
        raw_model$predictions
      } else {
        NULL
      }
    }, error = function(e) NULL)
    if (!is.null(oob) && all(is.finite(oob))) {
      return(finish(oob))
    }
  }

  tryCatch({
    cols <- unique(c(feature_cols, target_col))
    obs_dt <- as.data.table(data_temp)[obs_idx, cols, with = FALSE]
    if (anyNA(obs_dt)) {
      obs_dt <- impute_missing_values(obs_dt, data_temp)
    }
    obs_dt <- enforce_factor_levels(obs_dt, factor_levels)
    obs_task <- TaskRegr$new(id = paste0(target_col, "_donors"),
                             backend = obs_dt, target = target_col)
    finish(learner$predict(obs_task)$response)
  }, error = function(e) NULL)
}

#' Score-based PMM donor selection
#'
#' For each missing value, finds k nearest observed values based on model
#' score (predicted value) distance, then aggregates using the chosen method.
#'
#' @param y_obs Observed values of target variable
#' @param score_obs Model scores for observed rows
#' @param score_miss Model scores for missing rows
#' @param k Number of nearest donors
#' @param agg_method One of "mean", "median", "random", or a function
#' @return Numeric vector of length(score_miss) with imputed values
#' @keywords internal
# Selects PMM donors by distance between observed and missing scores.
pmm_donor_selection <- function(
    y_obs, score_obs, score_miss,
    k = 1L, agg_method = "random"
) {
  k <- min(k, length(y_obs))

  sapply(score_miss, function(s) {
    idx <- order(abs(score_obs - s))[seq_len(k)]
    neighbors <- y_obs[idx]

    if (is.function(agg_method)) {
      agg <- agg_method(neighbors)
      if (length(agg) != 1L || !is.numeric(agg) || is.na(agg)) {
        stop("pmm_k_method function must return exactly one non-missing numeric value.")
      }
      return(as.numeric(agg))
    }

    switch(agg_method,
      "mean" = mean(neighbors),
      "median" = median(neighbors),
      "random" = sample(neighbors, 1),
      sample(neighbors, 1)
    )
  })
}

#' Midastouch: PMM with covariate-distance-weighted donor selection
#'
#' For each missing value, finds k nearest donors using a combined score:
#' closeness in predicted value (score) AND closeness in covariate space
#' (Mahalanobis distance). Donors closer in covariate space are upweighted.
#'
#' Based on Siddique & Belin (2008), "Multiple imputation using an iterative
#' hot-deck with distance-based donor selection", Statistics in Medicine.
#'
#' @param y_obs Observed values of the target variable
#' @param X_obs Predictor matrix for observed rows (n_obs x p)
#' @param X_miss Predictor matrix for missing rows (n_miss x p)
#' @param score_obs Model predictions for observed rows
#' @param score_miss Model predictions for missing rows
#' @param k Number of candidate donors (default 5)
#' @return Numeric vector of length n_miss with imputed values drawn from donors
#' @keywords internal
# Selects Midastouch donors using score and covariate distances.
midastouch_donors <- function(
    y_obs, X_obs, X_miss,
    score_obs = NULL, score_miss = NULL, k = 5L
) {
  n_miss <- nrow(X_miss)
  n_obs <- length(y_obs)
  k <- min(k, n_obs)

  X_obs <- as.matrix(X_obs)
  X_miss <- as.matrix(X_miss)

  # Remove columns with any NA to avoid NaN in distance computation
  complete_cols <- apply(X_obs, 2, function(col) !any(is.na(col))) &
                   apply(X_miss, 2, function(col) !any(is.na(col)))
  if (sum(complete_cols) == 0) {
    # Fallback: just use score-based PMM if no complete numeric features
    if (!is.null(score_obs) && !is.null(score_miss)) {
      return(pmm_donor_selection(y_obs, score_obs, score_miss, k = k, agg_method = "random"))
    }
    return(sample(y_obs, size = n_miss, replace = TRUE))
  }
  X_obs <- X_obs[, complete_cols, drop = FALSE]
  X_miss <- X_miss[, complete_cols, drop = FALSE]

  # Covariance for Mahalanobis distance (regularized)
  cov_mat <- tryCatch({
    S <- cov(X_obs)
    S + diag(1e-6, ncol(S))
  }, error = function(e) {
    diag(apply(X_obs, 2, var, na.rm = TRUE) + 1e-6)
  })

  S_inv <- tryCatch(solve(cov_mat), error = function(e) diag(1 / diag(cov_mat)))

  result <- numeric(n_miss)
  for (i in seq_len(n_miss)) {
    diff_mat <- sweep(X_obs, 2, X_miss[i, ], "-")
    maha_dist <- sqrt(pmax(rowSums((diff_mat %*% S_inv) * diff_mat), 0))
    # Replace any remaining NaN with large distance
    maha_dist[is.na(maha_dist)] <- max(maha_dist, na.rm = TRUE) + 1

    if (!is.null(score_obs) && !is.null(score_miss)) {
      score_dist <- abs(score_obs - score_miss[i])
      maha_range <- max(maha_dist, na.rm = TRUE) - min(maha_dist, na.rm = TRUE)
      score_range <- max(score_dist, na.rm = TRUE) - min(score_dist, na.rm = TRUE)
      maha_norm <- if (!is.na(maha_range) && maha_range > 0) maha_dist / maha_range else rep(0, n_obs)
      score_norm <- if (!is.na(score_range) && score_range > 0) score_dist / score_range else rep(0, n_obs)
      combined_dist <- 0.5 * maha_norm + 0.5 * score_norm
    } else {
      combined_dist <- maha_dist
    }

    donor_idx <- order(combined_dist)[seq_len(k)]
    donor_weights <- 1 / (combined_dist[donor_idx] + .Machine$double.eps)
    donor_weights <- donor_weights / sum(donor_weights)
    chosen <- sample(donor_idx, size = 1, prob = donor_weights)
    result[i] <- y_obs[chosen]
  }

  result
}

#' Inject imputation uncertainty into predictions
#'
#' Adds stochastic noise to point predictions to properly reflect
#' imputation uncertainty. Called after model prediction, before
#' value assignment.
#'
#' @param preds Numeric vector of predicted values for missing observations
#' @param method One of "none", "normalerror", "resid", "pmm", "midastouch"
#' @param scale Scale estimate (sigma hat) from model. Required for "normalerror".
#' @param residuals Training residuals. Required for "resid".
#' @param y_obs Observed values of target variable. Required for "pmm", "midastouch".
#' @param score_obs Model scores for observed rows. Required for "pmm".
#' @param score_miss Model scores for missing rows. Required for "pmm".
#' @param pmm_k Number of donors for pmm/midastouch (default 5).
#' @param pmm_k_method Aggregation for pmm when k > 1 (default "random").
#' @param X_obs Predictor matrix for observed rows. Required for "midastouch".
#' @param X_miss Predictor matrix for missing rows. Required for "midastouch".
#' @return Numeric vector of adjusted predictions (same length as preds)
#' @keywords internal
# Adds stochastic imputation uncertainty to predictions.
inject_uncertainty <- function(
    preds,
    method = "none",
    scale = NULL,
    residuals = NULL,
    y_obs = NULL,
    score_obs = NULL,
    score_miss = NULL,
    pmm_k = 5L,
    pmm_k_method = "random",
    X_obs = NULL,
    X_miss = NULL
) {
  method <- match.arg(method, c("none", "normalerror", "resid", "pmm", "midastouch"))
  n_miss <- length(preds)

  if (method == "none") return(preds)

  if (method == "normalerror") {
    if (is.null(scale) || length(scale) != 1L || !is.numeric(scale) || is.na(scale) || scale <= 0) {
      warning("inject_uncertainty: scale not available for 'normalerror'. Returning point predictions.")
      return(preds)
    }
    return(preds + rnorm(n_miss, mean = 0, sd = scale))
  }

  if (method == "resid") {
    if (is.null(residuals) || length(residuals) == 0) {
      warning("inject_uncertainty: residuals not available for 'resid'. Returning point predictions.")
      return(preds)
    }
    return(preds + sample(residuals, size = n_miss, replace = TRUE))
  }

  if (method == "pmm") {
    if (is.null(y_obs) || is.null(score_obs) || is.null(score_miss)) {
      warning("inject_uncertainty: observed values/scores not available for 'pmm'. Returning point predictions.")
      return(preds)
    }
    return(pmm_donor_selection(
      y_obs = y_obs, score_obs = score_obs, score_miss = score_miss,
      k = pmm_k, agg_method = pmm_k_method
    ))
  }

  if (method == "midastouch") {
    if (is.null(y_obs) || is.null(X_obs) || is.null(X_miss)) {
      warning("inject_uncertainty: covariate data not available for 'midastouch'. Returning point predictions.")
      return(preds)
    }
    return(midastouch_donors(
      y_obs = y_obs, X_obs = X_obs, X_miss = X_miss,
      score_obs = score_obs, score_miss = score_miss, k = pmm_k
    ))
  }

  preds
}

# =============================================================================
# Chapter 7: Result construction, tuning, and matrix helpers
# =============================================================================

# Builds the final return value, including optional prediction history and tuning logs.
# Resolve the predictor set for one target variable: the user's `predictors`
# entry if given (order preserved, restricted to the available columns),
# otherwise all other variables. `exclude` removes helper columns (e.g. the
# semicontinuous zero-flag) from the default set.
select_feature_cols <- function(var, all_vars, predictors, exclude = character(0)) {
  base <- setdiff(all_vars, c(var, exclude))
  if (!is.null(predictors) && !is.null(predictors[[var]])) {
    intersect(predictors[[var]], base)
  } else {
    base
  }
}

# Train a learner, falling back to a featureless learner (mean / class-
# frequency prediction) when training fails, so one pathological variable
# cannot abort the whole imputation run (mice never dies wholesale). Returns
# the trained learner -- the original on success, the trained fallback
# otherwise -- and warns naming the variable, learner and error. The fallback
# is marked with attr(., "vimpute_fallback") so callers can skip model-info
# extraction and bootstrap refits that assume the original learner's internals.
train_with_fallback <- function(learner, task, var) {
  err <- tryCatch({ learner$train(task); NULL }, error = function(e) e)
  if (is.null(err)) return(learner)
  warning(sprintf(
    paste0("Learner '%s' failed for variable '%s' (%s). Falling back to a ",
           "featureless learner (mean/mode prediction) for this variable."),
    learner$id, var, conditionMessage(err)), call. = FALSE)
  fb <- if (task$task_type == "regr") lrn("regr.featureless") else lrn("classif.featureless")
  # keep the predict type the caller configured (e.g. "prob" for factor targets)
  try(fb$predict_type <- learner$predict_type, silent = TRUE)
  fb$train(task)
  attr(fb, "vimpute_fallback") <- TRUE
  fb
}

build_vimpute_result <- function(data, data_new, imp_var, factor_levels,
                                 pred_history = FALSE, history = NULL,
                                 tune = FALSE, tuning_log = list(),
                                 ordered_info = list(),
                                 data_all_variables = NULL,
                                 considered_variables = NULL,
                                 keep_all_columns = TRUE,
                                 input_is_dt = TRUE,
                                 convergence = NULL,
                                 chain = NULL,
                                 model_error = NULL) {
  result <- as.data.table(if (imp_var) data_new else data)
  result <- enforce_factor_levels(result, factor_levels)
  # enforce_factor_levels rebuilds columns as plain factors; put back the
  # `ordered` class on columns the user supplied as ordered factors.
  result <- restore_ordered_factors(result, ordered_info)
  # By default return the full dataset: re-attach columns excluded via
  # considered_variables (opt out with keep_all_columns = FALSE).
  if (keep_all_columns) {
    result <- merge_passthrough_columns(result, data_all_variables, considered_variables)
  }

  # Type-stable return: the result is ALWAYS the imputed data, classed like the
  # input; diagnostics ride along as attributes instead of a bare-list wrapper
  # (which used to switch the return type under tune/pred_history).
  if (!input_is_dt) {
    result <- as.data.frame(result)
  }
  if (pred_history) {
    ph <- rbindlist(history, fill = TRUE)
    if (!input_is_dt) ph <- as.data.frame(ph)
    attr(result, "pred_history") <- ph
  }
  if (any(unlist(tune))) {
    attr(result, "tuning_log") <- tuning_log
  }
  if (!is.null(convergence)) {
    attr(result, "convergence") <- convergence
  }
  if (!is.null(chain)) {
    attr(result, "chain") <- chain
  }
  if (!is.null(model_error) && length(model_error) > 0L) {
    attr(result, "model_error") <- model_error
  }

  result
}

# Runs model prediction, back-transformation, uncertainty handling, and PMM for one variable.
predict_imputations <- function(is_sc, var, data, data_temp, missing_idx, feature_cols,
                                learner, factor_levels, backend_data, target_col,
                                method_var, ranger_median, sequential, i, nseq,
                                lhs_transformation, missing_indices, pmm, pmm_k,
                                pmm_k_method, uncert, has_any_pmm,
                                stored_model_info, boot = FALSE,
                                verbose = FALSE) {
  pred_task <- NULL
  factor_noise_free <- NULL
  score_current <- NULL
  miss_idx <- missing_indices[[var]]
  obs_idx <- setdiff(seq_len(nrow(data)), miss_idx)

  if (is_sc) {
    zero_prob_col <- paste0(var, "_zero_prob")

    class_learner <- learner$classifier
    if ("prob" %in% class_learner$predict_types) {
      class_learner$predict_type <- "prob"
    } else {
      class_learner$predict_type <- "response"
      warning(sprintf("predict_type 'prob' not supported by learner '%s'; fallback to 'response'", class_learner$id))
    }

    class_pred_data <- data_temp[missing_idx, feature_cols, with = FALSE]
    class_pred_data <- enforce_factor_levels(class_pred_data, factor_levels)
    check_all_factor_levels(class_pred_data, factor_levels)

    if (anyNA(class_pred_data)) {
      class_pred_data <- impute_missing_values(class_pred_data, data_temp)
    }

    factor_cols <- names(class_pred_data)[sapply(class_pred_data, is.factor)]
    reserve_level <- ".__IMPUTEOOR_NEW__"
    for (col in factor_cols) {
      train_levels <- factor_levels[[col]]
      if (!(reserve_level %in% train_levels)) {
        train_levels <- c(train_levels, reserve_level)
      }
      class_pred_data[[col]][!class_pred_data[[col]] %in% train_levels] <- reserve_level
      class_pred_data[[col]] <- factor(class_pred_data[[col]], levels = train_levels)
    }

    pred_probs <- class_learner$predict_newdata(class_pred_data)$prob
    pi_hat <- if (!is.null(pred_probs) && "positive" %in% colnames(pred_probs)) {
      pred_probs[, "positive"]
    } else {
      as.numeric(class_learner$predict_newdata(class_pred_data)$response == "positive")
    }

    if (!zero_prob_col %in% names(data)) {
      data[[zero_prob_col]] <- NA_real_
    }
    data[[zero_prob_col]][missing_idx] <- pi_hat

    reg_learner <- learner$regressor
    reg_pred_data <- data_temp[missing_idx, feature_cols, with = FALSE]
    reg_pred_data <- enforce_factor_levels(reg_pred_data, factor_levels)
    check_all_factor_levels(reg_pred_data, factor_levels)

    if (anyNA(reg_pred_data)) {
      reg_pred_data <- impute_missing_values(reg_pred_data, data_temp[missing_idx])
    }

    preds_reg <- NULL
    if (method_var == "ranger" && isTRUE(ranger_median) && !is.null(reg_learner)) {
      preds_reg <- predict_ranger_median(reg_learner, reg_pred_data, target_name = NULL)
    }
    if (is.null(preds_reg)) {
      if (is.null(reg_learner) || !is.function(reg_learner$predict_newdata)) {
        positive_values <- data_temp[[var]][!is.na(data_temp[[var]]) & data_temp[[var]] > 0]
        fallback_positive <- if (length(positive_values) > 0) {
          stats::median(positive_values)
        } else {
          0
        }
        preds_reg <- rep(fallback_positive, nrow(reg_pred_data))
      } else {
        preds_reg <- reg_learner$predict_newdata(reg_pred_data)$response
      }
    }

    preds <- pi_hat * preds_reg
  } else {
    bdt <- as.data.table(backend_data)
    bdt <- enforce_factor_levels(bdt, factor_levels)
    check_all_factor_levels(bdt, factor_levels)

    if (method_var == "ranger") {
      bdt <- set_out_of_range_factor_levels_to_na(bdt, data_temp)
    }

    if (anyNA(bdt)) {
      bdt <- impute_missing_values(bdt, data_temp)
    }

    backend_data <- mlr3::as_data_backend(bdt)

    if (is.factor(data_temp[[target_col]])) {
      backend_dt <- if (inherits(backend_data, "DataBackend")) {
        rows <- backend_data$row_ids
        if (is.null(rows)) rows <- seq_len(backend_data$nrow)
        as.data.table(backend_data$data(rows = rows, cols = backend_data$colnames))
      } else {
        as.data.table(backend_data)
      }
      if (anyNA(backend_dt[[target_col]])) {
        mode_value <- names(which.max(table(data_temp[[target_col]], useNA = "no")))
        backend_dt[[target_col]][is.na(backend_dt[[target_col]])] <- mode_value
      }
      pred_task <- TaskClassif$new(
        id = target_col,
        backend = backend_dt,
        target = target_col
      )

      learner$predict_type <- "prob"
      pred_probs <- learner$predict(pred_task)$prob
      pred_probs <- as.matrix(pred_probs)
      bad_probs <- !is.finite(pred_probs)
      if (any(bad_probs)) {
        pred_probs[bad_probs] <- 0
      }
      row_sums <- rowSums(pred_probs)
      bad_rows <- !is.finite(row_sums) | row_sums <= 0
      if (any(bad_rows)) {
        pred_probs[bad_rows, ] <- 1 / ncol(pred_probs)
        row_sums[bad_rows] <- 1
      }
      pred_probs <- pred_probs / row_sums

      # noise-free argmax feeds the convergence criterion; the imputed
      # values are class-probability draws whenever a value-level
      # uncertainty regime is requested (uncert != "none"), so early
      # stopping cannot strip factor targets of between-imputation
      # variability
      factor_noise_free <- levels(data_temp[[target_col]])[
        apply(pred_probs, 1, which.max)]
      if (uncert != "none") {
        preds <- apply(pred_probs, 1, function(probs) {
          sample(levels(data_temp[[target_col]]), size = 1, prob = probs)
        })
      } else {
        preds <- factor_noise_free
      }
    } else if (is.numeric(data_temp[[target_col]])) {
      backend_dt <- if (inherits(backend_data, "DataBackend")) {
        rows <- backend_data$row_ids
        if (is.null(rows)) rows <- seq_len(backend_data$nrow)
        as.data.table(backend_data$data(rows = rows, cols = backend_data$colnames))
      } else {
        as.data.table(backend_data)
      }
      if (anyNA(backend_dt[[target_col]])) {
        backend_dt[[target_col]][is.na(backend_dt[[target_col]])] <-
          median(data_temp[[target_col]], na.rm = TRUE)
      }
      pred_task <- TaskRegr$new(
        id = target_col,
        backend = backend_dt,
        target = target_col
      )
      preds <- NULL
      if (method_var == "ranger" && isTRUE(ranger_median)) {
        preds <- predict_ranger_median(learner, bdt, target_name = target_col)
      }
      if (is.null(preds)) {
        preds <- learner$predict(pred_task)$response
      }
    } else {
      stop("Error: Target variable is neither numeric nor a factor!")
    }
  }

  if (!is.null(lhs_transformation)) {
    preds <- inverse_transform(preds, lhs_transformation)
  }

  if (inherits(preds, "numeric")) {
    decimal_places <- max(sapply(na.omit(data[[var]]), get_decimal_places), na.rm = TRUE)
    preds <- round(preds, decimal_places)
  }

  raw_model_preds <- preds

  if (inherits(preds, "numeric")) {
    score_current <- numeric(nrow(data))
    score_current[obs_idx] <- data[[var]][obs_idx]
    score_current[miss_idx] <- preds
  }

  if (uncert != "none" && !has_any_pmm && inherits(preds, "numeric") && !is_sc) {
    uncert_args <- list(preds = preds, method = uncert)

    if (uncert %in% c("normalerror", "resid") && !is.null(stored_model_info)) {
      uncert_args$scale <- stored_model_info$scale
      uncert_args$residuals <- stored_model_info$residuals
    }

    # true PMM (Little 1988): donors are matched on their PREDICTED values,
    # so score the observed rows with the trained learner (out-of-bag for a
    # no-bootstrap ranger fit); falls back to observed-value matching with
    # a warning if the donors cannot be scored
    donor_scores <- NULL
    if (uncert %in% c("pmm", "midastouch")) {
      donor_scores <- pmm_observed_scores(
        learner = learner, data_temp = data_temp, obs_idx = obs_idx,
        feature_cols = feature_cols, target_col = target_col,
        factor_levels = factor_levels, method_var = method_var,
        boot = boot, lhs_transformation = lhs_transformation)
      if (is.null(donor_scores) || length(donor_scores) != length(obs_idx) ||
          !is.numeric(donor_scores)) {
        warning(sprintf(
          "Could not score the donors of '%s' by prediction; matching on observed values instead.",
          var))
        donor_scores <- score_current[obs_idx]
      }
      if (exists("decimal_places")) {
        donor_scores <- round(donor_scores, decimal_places)
      }
    }

    if (uncert == "pmm") {
      valid_obs <- !is.na(data[[var]][obs_idx]) & is.finite(donor_scores)
      obs_idx_u <- obs_idx[valid_obs]
      if (length(obs_idx_u) == 0L) {
        warning(sprintf("No valid uncertainty PMM donors available for '%s'. Returning point predictions.", var))
        uncert_args$method <- "none"
      } else {
        uncert_args$y_obs <- data[[var]][obs_idx_u]
        uncert_args$score_obs <- donor_scores[valid_obs]
        uncert_args$score_miss <- score_current[miss_idx]
        uncert_args$pmm_k <- 5L
        uncert_args$pmm_k_method <- "random"
      }
    }

    if (uncert == "midastouch") {
      valid_obs <- !is.na(data[[var]][obs_idx]) & is.finite(donor_scores)
      obs_idx_u <- obs_idx[valid_obs]
      if (length(obs_idx_u) == 0L) {
        warning(sprintf("No valid midastouch donors available for '%s'. Returning point predictions.", var))
        uncert_args$method <- "none"
      } else {
        uncert_args$y_obs <- data[[var]][obs_idx_u]
        uncert_args$score_obs <- donor_scores[valid_obs]
        uncert_args$score_miss <- score_current[miss_idx]
        uncert_args$pmm_k <- 5L
        feat_names <- setdiff(names(data), var)
        num_feats <- feat_names[sapply(feat_names, function(f) is.numeric(data[[f]]))]
        if (length(num_feats) > 0) {
          uncert_args$X_obs <- as.matrix(data[obs_idx_u, num_feats, with = FALSE])
          uncert_args$X_miss <- as.matrix(data[miss_idx, num_feats, with = FALSE])
        }
      }
    }

    preds <- do.call(inject_uncertainty, uncert_args)

    if (exists("decimal_places")) {
      preds <- round(preds, decimal_places)
    }
  }

  convergence_preds <- if (inherits(raw_model_preds, "numeric") && (isTRUE(pmm[[var]]) || uncert %in% c("pmm", "midastouch"))) {
    raw_model_preds
  } else if (!is.null(factor_noise_free)) {
    # factor draws are stochastic; track the noise-free argmax instead
    factor_noise_free
  } else {
    preds
  }

  if (pmm[[var]] && is.numeric(data_temp[[var]]) && length(miss_idx) > 0) {
    if (verbose) {
      message("***** PMM / Score-kNN for predictions")
    }

    valid_obs <- !is.na(data[[var]][obs_idx]) & is.finite(score_current[obs_idx])
    obs_idx <- obs_idx[valid_obs]
    y_obs <- data[[var]][obs_idx]
    if (length(y_obs) == 0L) {
      warning(sprintf("No valid PMM donors available for '%s'. Keeping model predictions.", var))
    }

    if (length(y_obs) > 0L && pmm_k[[var]] >= 1 && is.numeric(data_temp[[var]])) {
      score_obs <- score_current[obs_idx]
      score_miss <- score_current[miss_idx]
      k <- min(pmm_k[[var]], length(y_obs))
      pmm_method_var <- pmm_k_method[[var]]

      preds <- sapply(score_miss, function(s) {
        idx <- order(abs(score_obs - s))[1:k]
        neighbors <- y_obs[idx]
        if (is.function(pmm_method_var)) {
          agg <- pmm_method_var(neighbors)
          if (length(agg) != 1L || !is.numeric(agg) || is.na(agg)) {
            stop(sprintf(
              "pmm_k_method function for '%s' must return exactly one non-missing numeric value.",
              var
            ))
          }
          as.numeric(agg)
        } else if (pmm_method_var == "mean") {
          mean(neighbors)
        } else if (pmm_method_var == "median") {
          median(neighbors)
        } else {
          sample(neighbors, 1)
        }
      })
    }

    decimal_places <- max(sapply(na.omit(data[[var]]), get_decimal_places), na.rm = TRUE)
    preds <- round(preds, decimal_places)
  }

  list(
    preds = preds,
    convergence_preds = convergence_preds,
    pred_task = pred_task,
    data = data
  )
}

# Applies final predictions to data, convergence data, and optional imputation flags.
apply_imputations <- function(data, data_new, convergence_data, var, missing_idx,
                              preds, convergence_preds, original_data,
                              factor_levels, imp_var) {
  if (length(missing_idx) > 0) {
    is_target_numeric <- is.numeric(original_data[[var]])

    if (is_target_numeric) {
      if (is.factor(data[[var]])) {
        data[, (var) := as.numeric(as.character(get(var)))]
      }
      if (exists("data_new") && is.factor(data_new[[var]])) {
        data_new[, (var) := as.numeric(as.character(get(var)))]
      }

      data[missing_idx, (var) := as.numeric(preds)]
      convergence_data[missing_idx, (var) := as.numeric(convergence_preds)]
    } else if (is.factor(original_data[[var]])) {
      data[missing_idx, (var) := factor(preds, levels = factor_levels[[var]])]
      convergence_data[missing_idx, (var) := factor(convergence_preds, levels = factor_levels[[var]])]
    } else {
      stop(paste("Unknown data type for variable:", var))
    }

    data <- copy(data)
  }

  if (length(missing_idx) > 0 && imp_var) {
    imp_col <- paste0(var, "_imp")
    if (!is.null(missing_idx) && length(missing_idx) > 0) {
      if (!(imp_col %in% colnames(data_new))) {
        data_new[, (imp_col) := FALSE]
      }

      data_new[, (var) := data[[var]]]

      if (length(preds) == length(missing_idx) && !all(is.na(preds))) {
        set(data_new, i = missing_idx, j = imp_col, value = TRUE)
      } else {
        warning(paste("Warning: `preds` is empty or does not have the same length as `missing_idx` for ", var))
      }
    }
  }

  list(data = data, data_new = data_new, convergence_data = convergence_data)
}

# Evaluates sequential convergence and signals whether the iteration loop
# should stop. Per-variable, scale-free change measure (audit P2.67):
#   numeric v:  d_v = mean((new - old)^2) / var(observed_v)
#   factor v:   d_v = share of imputed cells whose category changed
# The run counts as quiet when max_v d_v < eps (max, not sum, so a large
# change in one variable cannot be masked by others, and a wide-scale
# variable cannot block convergence forever), and stops after two
# consecutive quiet iterations. Returns the per-variable d_v vector so the
# caller can accumulate the iterations x variables convergence matrix.
check_convergence <- function(sequential, i, variables_NA, missing_indices,
                              convergence_prev, convergence_data, original_data,
                              eps, no_change_counter, verbose) {
  if (!sequential || i == 1) {
    return(list(no_change_counter = 0, should_break = FALSE, d = NULL))
  }

  d <- setNames(rep(NA_real_, length(variables_NA)), variables_NA)

  for (v in variables_NA) {
    idx <- missing_indices[[v]]
    if (length(idx) == 0) next

    old_vals <- convergence_prev[[v]][idx]
    new_vals <- convergence_data[[v]][idx]

    if (is.numeric(original_data[[v]])) {
      denom <- stats::var(original_data[[v]], na.rm = TRUE)
      if (!is.finite(denom) || denom <= 0) denom <- 1
      d_var <- mean((new_vals - old_vals)^2, na.rm = TRUE) / denom
    } else if (is.factor(original_data[[v]])) {
      d_var <- mean(as.character(old_vals) != as.character(new_vals), na.rm = TRUE)
    } else {
      next
    }
    if (is.finite(d_var)) d[v] <- d_var
  }

  max_d <- suppressWarnings(max(d, na.rm = TRUE))  # -Inf when nothing measurable

  if (verbose) {
    message(sprintf(
      "Iteration %d: max relative change = %s (eps = %g)",
      i, if (is.finite(max_d)) sprintf("%.6f", max_d) else "none", eps
    ))
  }

  if (is.finite(max_d) && max_d >= eps) {
    return(list(no_change_counter = 0, should_break = FALSE, d = d))
  }

  no_change_counter <- no_change_counter + 1
  if (no_change_counter < 2) {
    return(list(no_change_counter = no_change_counter, should_break = FALSE, d = d))
  }

  if (verbose) {
    message(sprintf(
      "Convergence achieved after two consecutive quiet iterations (stop after %d iterations)", i))
  }

  list(no_change_counter = no_change_counter, should_break = TRUE, d = d)
}

# Determines safe cross-validation folds for sample size and class counts.
safe_cv_folds <- function(task, max_folds = 5L) {
  if (task$nrow < 2L) {
    return(NA_integer_)
  }

  folds <- min(as.integer(max_folds), task$nrow)
  if (identical(task$task_type, "classif")) {
    class_counts <- table(task$truth())
    folds <- min(folds, min(class_counts))
  }

  if (!is.finite(folds) || folds < 2L) {
    return(NA_integer_)
  }

  as.integer(folds)
}

# Normalizes model-matrix column names for mlr3-compatible feature names.
clean_model_matrix_colnames <- function(names) {
  names <- make.names(names, unique = TRUE)
  patterns <- c("\\(", "\\)", ":", "\\*", "\\^", "%in%", "/", "-", "\\+", " ")
  replacements <- c("", "", "_int_", "_cross_", "_pow_", "_nest_", "_sub_", "_minus_", "_plus_", "")
  for (i in seq_along(patterns)) {
    names <- gsub(patterns[i], replacements[i], names)
  }
  names
}

# Deterministically fills remaining predictor missings from reference data.
impute_missing_values <- function(data, ref_data) {
  for (col in colnames(data)) {
    if (any(is.na(data[[col]]))) {
      if (is.numeric(ref_data[[col]])) {
        fill_val <- median(ref_data[[col]], na.rm = TRUE)
        if (!is.finite(fill_val) || is.na(fill_val)) {
          fill_val <- 0
        }
        data[[col]][is.na(data[[col]])] <- fill_val
      } else if (is.factor(ref_data[[col]])) {
        mode_value <- names(which.max(table(ref_data[[col]], useNA = "no")))
        if (is.null(mode_value) || length(mode_value) == 0L || is.na(mode_value)) {
          mode_value <- levels(ref_data[[col]])[1L]
        }
        data[[col]][is.na(data[[col]])] <- mode_value
      } else if (is.character(ref_data[[col]])) {
        mode_value <- names(which.max(table(ref_data[[col]], useNA = "no")))
        if (is.null(mode_value) || length(mode_value) == 0L || is.na(mode_value)) {
          mode_value <- ""
        }
        data[[col]][is.na(data[[col]])] <- mode_value
      }
    }
  }
  data
}

# Sets factor values outside the reference levels to NA.
set_out_of_range_factor_levels_to_na <- function(data, ref_data) {
  for (col in colnames(data)) {
    if (is.factor(data[[col]])) {
      known_levels <- levels(ref_data[[col]])
      unknown_idx <- !data[[col]] %in% known_levels
      if (any(unknown_idx, na.rm = TRUE)) {
        data[[col]][unknown_idx] <- NA
      }
    }
  }
  data
}

# Builds the hyperparameter search space for the selected learner.
build_vimpute_search_space <- function(best_learner_id, task, method = NULL) {
  # A search_space hook in the method's registry entry wins over the built-in
  # per-learner spaces below (register_vimpute_method(search_space = )).
  if (!is.null(method)) {
    entry <- get_vimpute_method(method)
    if (!is.null(entry) && is.function(entry$search_space)) {
      ss <- entry$search_space(best_learner_id, task)
      if (is.list(ss) && !is.null(ss$space)) {
        if (is.null(ss$n_evals)) ss$n_evals <- 10
        return(ss)
      }
    }
  }

  n <- task$nrow
  size <- if (n <= 5000) "small" else if (n <= 100000) "medium" else "large"

  if (best_learner_id %in% c("regr.cv_glmnet", "regr.glmnet", "classif.glmnet")) {
    lambda_upper <- if (size == "small") 1 else 0.5
    if (best_learner_id == "regr.cv_glmnet") {
      upper_nfolds <- if (size == "small") 5L else 3L
      space <- ps(
        alpha  = p_dbl(0, 1),
        lambda = p_dbl(1e-4, lambda_upper, logscale = TRUE),
        nfolds = p_int(3L, upper_nfolds)
      )
    } else {
      space <- ps(
        alpha  = p_dbl(0, 1),
        lambda = p_dbl(1e-4, lambda_upper, logscale = TRUE)
      )
    }
    return(list(space = space, n_evals = if (size == "small") 10 else if (size == "medium") 12 else 8))
  }

  if (best_learner_id %in% c("regr.ranger", "classif.ranger")) {
    space <- ps(
      num.trees       = p_int(if (size == "large") 200L else 300L,
                              if (size == "small") 900L else 600L),
      min.node.size   = p_int(if (size == "small") 3L else 10L,
                              if (size == "small") 10L else 50L),
      sample.fraction = p_dbl(if (size == "large") 0.6 else 0.8, 1.0)
    )
    return(list(space = space, n_evals = if (size == "small") 15 else if (size == "medium") 12 else 10))
  }

  if (best_learner_id %in% c("regr.xgboost", "classif.xgboost")) {
    space <- ps(
      nrounds          = p_int(100L, if (size == "small") 500L else 300L),
      eta              = p_dbl(0.05, 0.3, logscale = TRUE),
      max_depth        = p_int(2L, if (size == "small") 8L else 6L),
      subsample        = p_dbl(0.5, 1.0),
      colsample_bytree = p_dbl(0.5, 1.0)
    )
    return(list(space = space, n_evals = if (size == "small") 20 else if (size == "medium") 15 else 12))
  }

  if (best_learner_id %in% c("regr.lm_rob", "classif.glm_rob")) {
    space <- ps(
      tuning.chi = p_dbl(1.2, 1.5),
      tuning.psi = p_dbl(1.2, 1.5),
      max.it     = p_int(50L, 200L)
    )
    return(list(space = space, n_evals = 8))
  }

  if (best_learner_id %in% c("regr.gam_imp", "classif.gam_imp")) {
    space <- ps(min_unique = p_int(3L, 8L))
    return(list(space = space, n_evals = 5))
  }

  if (best_learner_id %in% c("regr.robgam_imp", "classif.robgam_imp")) {
    space <- ps(
      alpha = p_dbl(0.7, 0.95),
      min_unique = p_int(3L, 8L)
    )
    return(list(space = space, n_evals = 8))
  }

  list(space = NULL, n_evals = 10)
}

# =============================================================================
# Chapter 8: GAM and RobGAM helpers
# =============================================================================

#' Build a GAM formula with automatic smooth terms
#'
#' Constructs a formula for mgcv::gam() by wrapping numeric predictors with
#' sufficient unique values in s() terms and keeping factors as linear terms.
#'
#' @param target Character: target variable name
#' @param features Character vector: predictor variable names
#' @param data Data frame used to check variable types and unique value counts
#' @param min_unique Integer: minimum number of unique values for a numeric
#'   predictor to be wrapped in s(). Default 4 (mgcv needs at least k=3 knots).
#' @return A formula object suitable for mgcv::gam()
#' @keywords internal
# Builds GAM formulas with automatic smooth terms for suitable numeric features.
build_gam_formula <- function(target, features, data, min_unique = 4L, default_k = 10L) {
  terms <- vapply(features, function(f) {
    col <- data[[f]]
    n_unique <- length(unique(col[!is.na(col)]))
    if (is.numeric(col) && n_unique >= min_unique) {
      k <- min(default_k, max(3L, n_unique - 1L))
      if (k >= default_k) {
        paste0("s(", f, ")")
      } else {
        paste0("s(", f, ", k = ", k, ")")
      }
    } else {
      f
    }
  }, character(1))
  if (!length(terms)) {
    return(as.formula(paste(target, "~ 1")))
  }
  as.formula(paste(target, "~", paste(terms, collapse = " + ")))
}

# Fits a GAM model with defensive mgcv fallbacks for numerical issues.
fit_gam_model <- function(formula, data, family = NULL, weights = NULL) {
  args <- list(
    formula = formula,
    data = data,
    control = mgcv::gam.control(maxit = 100)
  )
  if (!is.null(family)) {
    args$family <- family
  }
  if (!is.null(weights)) {
    args$weights <- weights
  }

  fit_once <- function(call_args) {
    withCallingHandlers(
      do.call(mgcv::gam, call_args),
      warning = function(w) {
        warn_msg <- conditionMessage(w)
        if (grepl("Iteration limit reached without full convergence", warn_msg, fixed = TRUE) ||
            grepl("Fitting terminated with step failure", warn_msg, fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  tryCatch(
    fit_once(args),
    error = function(e) {
      retryable <- grepl("failed to converge", e$message, ignore.case = TRUE) ||
        grepl("optimizer", e$message, ignore.case = TRUE)
      if (!retryable) {
        stop(e)
      }

      retry_args <- args
      retry_args$method <- "REML"
      fit_once(retry_args)
    }
  )
}

#' Register GAM-based mlr3 learners for vimpute
#'
#' Creates and registers four custom mlr3 learners:
#' regr.gam_imp, classif.gam_imp, regr.robgam_imp, classif.robgam_imp.
#' Called automatically by vimpute() when method includes "gam" or "robgam".
#'
#' @keywords internal
# Registers GAM and RobGAM learners for regression and classification.
register_gam_learners <- function() {

  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for method = 'gam' or 'robgam'. Please install it.",
         call. = FALSE)
  }

  # ---- Regression GAM Learner ----
  LearnerRegrGAM <- R6::R6Class(
    classname = "LearnerRegrGAM",
    inherit = LearnerRegr,
    public = list(
      initialize = function() {
        param_set <- ps(
          min_unique = p_int(lower = 2L, upper = Inf, default = 4L)
        )
        super$initialize(
          id = "regr.gam_imp",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response"),
          packages = c("mgcv"),
          param_set = param_set
        )
        self$param_set$values <- list(min_unique = 4L)
      }
    ),
    private = list(
      .train = function(task) {
        pv <- self$param_set$get_values()
        data <- as.data.frame(task$data())
        target <- task$target_names
        features <- task$feature_names

        sanitized <- sanitize_model_features(data, target, features)
        data <- sanitized$data
        features <- sanitized$features
        self$state$feature_names <- features
        self$state$factor_levels <- sanitized$factor_levels

        if (length(sanitized$dropped) > 0L) {
          warning(sprintf(
            "Dropping constant or single-level predictors for '%s': %s",
            target, paste(sanitized$dropped, collapse = ", ")
          ))
        }

        form <- build_gam_formula(target, features, data, min_unique = pv$min_unique)

        model <- tryCatch(
          fit_gam_model(form, data = data),
          error = function(e) {
            warning(sprintf("gam() failed for '%s': %s\nFalling back to lm()", target, e$message))
            lm(build_feature_formula(target, features), data = data)
          }
        )

        model
      },

      .predict = function(task) {
        model <- self$model
        newdata <- as.data.frame(task$data())
        feature_names <- if (is.null(self$state$feature_names)) character(0) else self$state$feature_names

        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
              newdata[[var]] <- factor(newdata[[var]], levels = self$state$factor_levels[[var]])
            }
          }
        }
        newdata <- newdata[, feature_names, drop = FALSE]

        response <- tryCatch(
          as.numeric(predict(model, newdata = newdata, type = "response")),
          error = function(e) {
            warning("GAM prediction failed: ", e$message)
            rep(NA_real_, nrow(newdata))
          }
        )

        PredictionRegr$new(task = task, response = response)
      }
    )
  )

  mlr3::mlr_learners$add("regr.gam_imp", LearnerRegrGAM)

  # ---- Classification GAM Learner (binary + multiclass via OvR) ----
  LearnerClassifGAM <- R6::R6Class(
    classname = "LearnerClassifGAM",
    inherit = LearnerClassif,
    public = list(
      initialize = function() {
        param_set <- ps(
          min_unique = p_int(lower = 2L, upper = Inf, default = 4L)
        )
        super$initialize(
          id = "classif.gam_imp",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response", "prob"),
          packages = c("mgcv"),
          properties = c("twoclass", "multiclass"),
          param_set = param_set
        )
        self$param_set$values <- list(min_unique = 4L)
        self$state$models <- NULL
        self$state$classes <- NULL
      }
    ),
    private = list(
      .train = function(task) {
        pv <- self$param_set$get_values()
        data <- as.data.frame(task$data())
        target_name <- task$target_names
        y <- task$truth()
        features <- task$feature_names
        classes <- task$class_names
        self$state$classes <- classes

        data[[target_name]] <- y
        sanitized <- sanitize_model_features(data, target_name, features)
        data <- sanitized$data
        y <- data[[target_name]]
        features <- sanitized$features
        self$state$feature_names <- features
        self$state$factor_levels <- sanitized$factor_levels

        if (length(sanitized$dropped) > 0L) {
          warning(sprintf(
            "Dropping constant or single-level predictors for '%s': %s",
            target_name, paste(sanitized$dropped, collapse = ", ")
          ))
        }

        if (length(classes) == 2L) {
          df <- data.frame(y_bin = as.integer(y == classes[1]), data[, features, drop = FALSE])
          # Guard against single-level factor predictors before fitting GAM.
          for (feat in features) {
            if (is.factor(df[[feat]])) {
              df[[feat]] <- droplevels(df[[feat]])
              if (nlevels(df[[feat]]) < 2) {
                warning(sprintf("Feature '%s' collapsed to single level for class '%s'. Removing feature.", feat, classes[1]))
                features <- setdiff(features, feat)
              }
            }
          }

          if (length(features) == 0) {
            warning(sprintf("No valid features remain for class '%s'. Using constant model.", classes[1]))
            return(list(models = list(new_constant_binomial_model(mean(df$y_bin))), classes = classes, binary = TRUE))
          }

          prob <- mean(df$y_bin == 1)
          if (prob %in% c(0, 1)) {
            mod <- new_constant_binomial_model(prob)
          } else {
            form <- build_gam_formula("y_bin", features, df, min_unique = pv$min_unique)
            mod <- tryCatch(
              fit_gam_model(form, data = df, family = binomial()),
              error = function(e) {
                error_msg <- e$message
                if (grepl("contrasts", error_msg, ignore.case = TRUE)) {
                  warning(sprintf(
                    "gam(binomial) failed due to single-level factor: %s\nFalling back to glm()",
                    error_msg
                  ))
                } else {
                  warning(sprintf("gam(binomial) failed: %s\nFalling back to glm()", error_msg))
                }
                tryCatch(
                  glm(build_feature_formula("y_bin", features), data = df, family = binomial()),
                  error = function(e2) {
                    # Ultimate fallback: constant model
                    new_constant_binomial_model(mean(df$y_bin))
                  }
                )
              }
            )
          }
          return(list(models = list(mod), classes = classes, binary = TRUE))
        }

        # Multiclass: One-vs-Rest
        mods <- vector("list", length(classes))
        names(mods) <- classes
        for (k in classes) {
          df <- data.frame(y_bin = as.integer(y == k), data[, features, drop = FALSE])
          # Guard against single-level factor predictors before fitting GAM.
          features_k <- features
          for (feat in features_k) {
            if (is.factor(df[[feat]])) {
              df[[feat]] <- droplevels(df[[feat]])
              if (nlevels(df[[feat]]) < 2) {
                warning(sprintf("Feature '%s' collapsed to single level for class '%s'. Removing feature.", feat, k))
                features_k <- setdiff(features_k, feat)
              }
            }
          }

          if (length(features_k) == 0) {
            mods[[k]] <- new_constant_binomial_model(mean(df$y_bin))
            next
          }

          prob <- mean(df$y_bin == 1)
          if (prob %in% c(0, 1)) {
            mods[[k]] <- new_constant_binomial_model(prob)
          } else {
            form <- build_gam_formula("y_bin", features_k, df, min_unique = pv$min_unique)
            mods[[k]] <- tryCatch(
              fit_gam_model(form, data = df, family = binomial()),
              error = function(e) {
                warning(sprintf("gam(binomial) OvR class '%s' failed: %s\nFalling back to glm()", k, e$message))
                glm(build_feature_formula("y_bin", features_k), data = df, family = binomial())
              }
            )
          }
        }
        list(models = mods, classes = classes, binary = FALSE)
      },

      .predict = function(task) {
        model_info <- self$model
        newdata <- as.data.frame(task$data(cols = task$feature_names))
        classes <- model_info$classes
        feature_names <- if (is.null(self$state$feature_names)) character(0) else self$state$feature_names

        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
              newdata[[var]] <- factor(newdata[[var]], levels = self$state$factor_levels[[var]])
            }
          }
        }
        newdata <- newdata[, feature_names, drop = FALSE]

        if (model_info$binary) {
          mod <- model_info$models[[1]]
          p1 <- predict_binomial_response(mod, newdata, fallback = 0.5)
          p1 <- sanitize_binary_probs(p1, fallback = 0.5)
          probs <- cbind(p1, 1 - p1)
          colnames(probs) <- classes
        } else {
          Pk <- matrix(NA_real_, nrow(newdata), length(classes))
          colnames(Pk) <- classes
          for (k in classes) {
            Pk[, k] <- predict_binomial_response(model_info$models[[k]], newdata, fallback = 0.5)
            Pk[, k] <- sanitize_binary_probs(Pk[, k], fallback = 0.5)
          }
          probs <- normalize_multiclass_probs(Pk)
        }

        if (self$predict_type == "prob") {
          PredictionClassif$new(task = task, prob = probs)
        } else {
          resp <- classes[max.col(probs, ties.method = "first")]
          PredictionClassif$new(task = task, response = resp)
        }
      }
    )
  )

  mlr3::mlr_learners$add("classif.gam_imp", LearnerClassifGAM)

  # ---- Regression Robust GAM Learner ----
  LearnerRegrRobGAM <- R6::R6Class(
    classname = "LearnerRegrRobGAM",
    inherit = LearnerRegr,
    public = list(
      initialize = function() {
        param_set <- ps(
          min_unique = p_int(lower = 2L, upper = Inf, default = 4L),
          robust_method = p_fct(c("simple", "irw"), default = "simple"),
          alpha = p_dbl(lower = 0.7, upper = 0.95, default = 0.8),
          max_iter = p_int(lower = 1L, upper = 100L, default = 20L),
          tol = p_dbl(lower = 1e-8, upper = 1e-1, default = 1e-4),
          psi_k = p_dbl(lower = 1, upper = 10, default = 4.685)
        )
        super$initialize(
          id = "regr.robgam_imp",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response"),
          packages = c("mgcv"),
          param_set = param_set
        )
        self$param_set$values <- list(
          min_unique = 4L, robust_method = "simple", alpha = 0.8,
          max_iter = 20L, tol = 1e-4, psi_k = 4.685
        )
      }
    ),
    private = list(
      .train = function(task) {
        pv <- self$param_set$get_values()
        data <- as.data.frame(task$data())
        target <- task$target_names
        features <- task$feature_names
        n <- nrow(data)

        sanitized <- sanitize_model_features(data, target, features)
        data <- sanitized$data
        features <- sanitized$features
        self$state$feature_names <- features
        self$state$factor_levels <- sanitized$factor_levels

        if (length(sanitized$dropped) > 0L) {
          warning(sprintf(
            "Dropping constant or single-level predictors for '%s': %s",
            target, paste(sanitized$dropped, collapse = ", ")
          ))
        }

        form <- build_gam_formula(target, features, data, min_unique = pv$min_unique)

        if (pv$robust_method == "simple") {
          mod_init <- tryCatch(
            fit_gam_model(form, data = data),
            error = function(e) {
              warning(sprintf("Initial gam() failed for '%s': %s", target, e$message))
              return(NULL)
            }
          )
          if (is.null(mod_init)) {
            mod <- lm(build_feature_formula(target, features), data = data)
            return(list(mod = mod, subset_good = seq_len(n), subset_bad = integer(0),
                        scale = sd(residuals(mod))))
          }

          resids <- residuals(mod_init)
          cutoff <- quantile(abs(resids), probs = pv$alpha)
          good_idx <- which(abs(resids) <= cutoff)
          bad_idx <- which(abs(resids) > cutoff)

          mod <- tryCatch(
            fit_gam_model(form, data = data[good_idx, , drop = FALSE]),
            error = function(e) {
              warning(sprintf("robGAM refit on good subset failed: %s. Using initial model.", e$message))
              mod_init
            }
          )

          scale_est <- tryCatch(sqrt(summary(mod)$scale), error = function(e) sd(residuals(mod)))
          list(mod = mod, subset_good = good_idx, subset_bad = bad_idx, scale = scale_est)

        } else {
          # IRW: Iterative Reweighting
          mod <- tryCatch(
            fit_gam_model(form, data = data),
            error = function(e) {
              warning(sprintf("Initial gam() for irw failed: %s", e$message))
              return(NULL)
            }
          )
          if (is.null(mod)) {
            mod <- lm(build_feature_formula(target, features), data = data)
            return(list(mod = mod, subset_good = seq_len(n), subset_bad = integer(0),
                        scale = sd(residuals(mod))))
          }

          weights <- rep(1, n)
          for (iter in seq_len(pv$max_iter)) {
            resids <- residuals(mod)
            s <- mad(resids, constant = 1.4826)
            if (s < .Machine$double.eps) break
            u <- resids / (s * pv$psi_k)
            new_weights <- ifelse(abs(u) > 1, 0, (1 - u^2)^2)

            if (max(abs(new_weights - weights)) < pv$tol) break
            weights <- new_weights

            mod <- tryCatch(
              fit_gam_model(form, data = data, weights = weights),
              error = function(e) {
                warning(sprintf("robGAM irw iteration %d failed: %s", iter, e$message))
                mod
              }
            )
          }

          good_idx <- which(weights > 0.5)
          bad_idx <- which(weights <= 0.5)
          scale_est <- tryCatch(sqrt(summary(mod)$scale), error = function(e) sd(residuals(mod)))
          list(mod = mod, subset_good = good_idx, subset_bad = bad_idx,
               scale = scale_est, weights = weights)
        }
      },

      .predict = function(task) {
        model_info <- self$model
        newdata <- as.data.frame(task$data())
        feature_names <- if (is.null(self$state$feature_names)) character(0) else self$state$feature_names

        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
              newdata[[var]] <- factor(newdata[[var]], levels = self$state$factor_levels[[var]])
            }
          }
        }
        newdata <- newdata[, feature_names, drop = FALSE]

        response <- tryCatch(
          as.numeric(predict(model_info$mod, newdata = newdata, type = "response")),
          error = function(e) {
            warning("robGAM prediction failed: ", e$message)
            rep(NA_real_, nrow(newdata))
          }
        )

        PredictionRegr$new(task = task, response = response)
      }
    )
  )

  mlr3::mlr_learners$add("regr.robgam_imp", LearnerRegrRobGAM)

  # ---- Classification Robust GAM Learner (binary + multiclass via OvR) ----
  LearnerClassifRobGAM <- R6::R6Class(
    classname = "LearnerClassifRobGAM",
    inherit = LearnerClassif,
    public = list(
      initialize = function() {
        param_set <- ps(
          min_unique = p_int(lower = 2L, upper = Inf, default = 4L),
          robust_method = p_fct(c("simple", "irw"), default = "simple"),
          alpha = p_dbl(lower = 0.7, upper = 0.95, default = 0.8),
          max_iter = p_int(lower = 1L, upper = 100L, default = 20L),
          tol = p_dbl(lower = 1e-8, upper = 1e-1, default = 1e-4),
          psi_k = p_dbl(lower = 1, upper = 10, default = 4.685)
        )
        super$initialize(
          id = "classif.robgam_imp",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response", "prob"),
          packages = c("mgcv"),
          properties = c("twoclass", "multiclass"),
          param_set = param_set
        )
        self$param_set$values <- list(
          min_unique = 4L, robust_method = "simple", alpha = 0.8,
          max_iter = 20L, tol = 1e-4, psi_k = 4.685
        )
        self$state$models <- NULL
        self$state$classes <- NULL
      }
    ),
    private = list(
      .train = function(task) {
        pv <- self$param_set$get_values()
        data <- as.data.frame(task$data())
        y <- task$truth()
        features <- task$feature_names
        classes <- task$class_names
        self$state$classes <- classes

        for (col in names(data)) {
          if (is.factor(data[[col]])) data[[col]] <- droplevels(data[[col]])
        }

        factor_cols <- sapply(data[, features, drop = FALSE], is.factor)
        self$state$factor_levels <- lapply(data[, names(which(factor_cols)), drop = FALSE], levels)

        fit_one_robgam <- function(df, form) {
          n <- nrow(df)
           if (pv$robust_method == "simple") {
             mod_init <- tryCatch(
               fit_gam_model(form, data = df, family = binomial()),
               error = function(e) {
                 tryCatch(
                   glm(build_feature_formula("y_bin", features), data = df, family = binomial()),
                   error = function(e2) new_constant_binomial_model(mean(df$y_bin))
                 )
               }
             )

            if (inherits(mod_init, "vimpute_constant_binomial")) {
              return(mod_init)
            }

            dev_resids <- residuals(mod_init, type = "deviance")
            cutoff <- quantile(abs(dev_resids), probs = pv$alpha)
            good_idx <- which(abs(dev_resids) <= cutoff)

            mod <- tryCatch(fit_gam_model(form, data = df[good_idx, , drop = FALSE], family = binomial()),
              error = function(e) mod_init)
            return(mod)

          } else {
            weights <- rep(1, n)
            mod <- tryCatch(fit_gam_model(form, data = df, family = binomial()),
              error = function(e) glm(form, data = df, family = binomial()))

            for (iter in seq_len(pv$max_iter)) {
              dev_resids <- residuals(mod, type = "deviance")
              s <- mad(dev_resids, constant = 1.4826)
              if (s < .Machine$double.eps) break
              u <- dev_resids / (s * pv$psi_k)
              new_weights <- ifelse(abs(u) > 1, 0, (1 - u^2)^2)
              if (max(abs(new_weights - weights)) < pv$tol) break
              weights <- new_weights
              mod <- tryCatch(fit_gam_model(form, data = df, weights = weights, family = binomial()),
                error = function(e) mod)
            }
            return(mod)
          }
        }

        if (length(classes) == 2L) {
          df <- data.frame(y_bin = as.integer(y == classes[1]), data[, features, drop = FALSE])
          # Guard against single-level factor predictors before fitting robust GAM.
          for (feat in features) {
            if (is.factor(df[[feat]])) {
              df[[feat]] <- droplevels(df[[feat]])
              if (nlevels(df[[feat]]) < 2) {
                features <- setdiff(features, feat)
              }
            }
          }
          if (length(features) == 0) {
            return(list(models = list(new_constant_binomial_model(mean(df$y_bin))), classes = classes, binary = TRUE))
          }

          form <- build_gam_formula("y_bin", features, df, min_unique = pv$min_unique)
          mod <- fit_one_robgam(df, form)
          return(list(models = list(mod), classes = classes, binary = TRUE))
        }

        # Multiclass OvR
        mods <- vector("list", length(classes))
        names(mods) <- classes
        for (k in classes) {
          df <- data.frame(y_bin = as.integer(y == k), data[, features, drop = FALSE])
          # Guard against single-level factor predictors before fitting robust GAM.
          features_k <- features
          for (feat in features_k) {
            if (is.factor(df[[feat]])) {
              df[[feat]] <- droplevels(df[[feat]])
              if (nlevels(df[[feat]]) < 2) {
                features_k <- setdiff(features_k, feat)
              }
            }
          }
          if (length(features_k) == 0) {
            mods[[k]] <- new_constant_binomial_model(mean(df$y_bin))
            next
          }

          form <- build_gam_formula("y_bin", features_k, df, min_unique = pv$min_unique)
          mods[[k]] <- fit_one_robgam(df, form)
        }
        list(models = mods, classes = classes, binary = FALSE)
      },

      .predict = function(task) {
        model_info <- self$model
        newdata <- as.data.frame(task$data(cols = task$feature_names))
        classes <- model_info$classes

        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
              newdata[[var]] <- factor(newdata[[var]], levels = self$state$factor_levels[[var]])
            }
          }
        }

        if (model_info$binary) {
          p1 <- tryCatch(as.numeric(predict(model_info$models[[1]], newdata = newdata, type = "response")),
            error = function(e) rep(0.5, nrow(newdata)))
          p1 <- sanitize_binary_probs(p1, fallback = 0.5)
          probs <- cbind(p1, 1 - p1)
          colnames(probs) <- classes
        } else {
          Pk <- matrix(NA_real_, nrow(newdata), length(classes))
          colnames(Pk) <- classes
          for (k in classes) {
            Pk[, k] <- tryCatch(as.numeric(predict(model_info$models[[k]], newdata = newdata, type = "response")),
              error = function(e) rep(0.5, nrow(newdata)))
            Pk[, k] <- sanitize_binary_probs(Pk[, k], fallback = 0.5)
          }
          probs <- normalize_multiclass_probs(Pk)
        }

        if (self$predict_type == "prob") {
          PredictionClassif$new(task = task, prob = probs)
        } else {
          resp <- classes[max.col(probs, ties.method = "first")]
          PredictionClassif$new(task = task, response = resp)
        }
      }
    )
  )

  mlr3::mlr_learners$add("classif.robgam_imp", LearnerClassifRobGAM)
}
