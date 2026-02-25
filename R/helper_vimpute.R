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
        
        factor_cols = sapply(data, is.factor)
        if (any(factor_cols)) {
          for (col in names(data)[factor_cols]) {
            data[[col]] = droplevels(data[[col]])
          }
        }
        
        formula = reformulate(features, response = target)
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
        
        factor_col_names = names(data)[factor_cols]  # Namen der Faktor-Spalten
        self$state$factor_levels = lapply(data[, factor_col_names, drop = FALSE], levels)
        return(model)
      },
      
      .predict = function(task) {
        model = self$model
        newdata = as.data.frame(task$data())
        
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
  
  
  # robust Classification Learner
  # LearnerClassifGlmRob <- R6::R6Class(
  #   inherit = mlr3::LearnerClassif,
  #   public = list(
  #     initialize = function() {
  #       super$initialize(
  #         id = "classif.glm_rob",
  #         feature_types = c("numeric", "integer", "factor", "ordered"),
  #         predict_types = c("response", "prob"),
  #         packages = c("mlr3learners"),
  #         properties = c("twoclass", "multiclass")
  #       )
  #       self$state$learner = NULL
  #     }
  #   ),
  #   
  #   private = list(
  #     .train = function(task) {
  #       n_classes = length(task$class_names)
  #       if (n_classes == 2) {
  #         self$state$learner = mlr3::lrn("classif.log_reg", predict_type = self$predict_type)
  #       } else {
  #         self$state$learner = mlr3::lrn("classif.multinom", predict_type = self$predict_type)
  #       }
  #       self$state$learner$train(task)
  #     },
  #     
  #     .predict = function(task) {
  #       if (is.null(self$state$learner)) stop("Model not trained yet")
  #       pred = self$state$learner$predict(task)
  #       return(pred)
  #     }
  #   )
  # )
  
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
        X    <- data[, task$feature_names, drop = FALSE]
        classes <- task$class_names
        self$state$classes <- classes
        
        # robust factor handling: droplevels etc. (optional nach Bedarf)
        # ...
        
        if (length(classes) == 2L) {
          # Binär: ein robustes Logit
          df <- data.frame(y = as.factor(y), X)
          mod <- robustbase::glmrob(y ~ ., data = df, family = binomial())
          self$state$models <- list(mod)
        } else {
          # Multiclass: One-vs-Rest
          mods <- vector("list", length(classes))
          names(mods) <- classes
          for (k in classes) {
            # y_k = 1{y==k} vs rest
            yk <- factor(ifelse(y == k, 1L, 0L))
            df <- data.frame(y = yk, X)
            # robustes Logit
            mods[[k]] <- robustbase::glmrob(y ~ ., data = df, family = binomial())
          }
          self$state$models <- mods
        }
        invisible(TRUE)
      },
      
      .predict = function(task) {
        data <- as.data.frame(task$data(cols = task$feature_names))
        classes <- self$state$classes
        
        if (length(classes) == 2L) {
          mod <- self$state$models[[1]]
          p1  <- tryCatch(
            stats::predict(mod, newdata = data, type = "response"),
            error = function(e) rep(NA_real_, nrow(data))
          )
          # p(y=positive), definiere Klassenreihenfolge wie in task$class_names
          # Nehmen wir classes[1] als "positive" für Konsistenz:
          probs <- cbind(p1, 1 - p1)
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
            Pk[, k] <- tryCatch(
              stats::predict(mods[[k]], newdata = data, type = "response"),
              error = function(e) rep(NA_real_, nrow(data))
            )
            # clamp to avoid 0/1 pathological odds
            Pk[, k] <- pmin(pmax(Pk[, k], 1e-6), 1 - 1e-6)
          }
          # OvR-Normalisierung: q_k = p_k/(1-p_k); p_tilde_k = q_k / sum_j q_j
          Q <- Pk / (1 - Pk)
          row_sums <- rowSums(Q)
          probs <- Q / row_sums
          
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

### +++++++++++++++++++++++++++++++++ Helper Functions +++++++++++++++++++++++++++++++++ ###


#
#
#
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
#
#
#
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
#
#
#
# left handside formula
# extracts the left side of a formula and returns the name of the applied transformation (e.g. log)
identify_lhs_transformation <- function(formula) {
  lhs <- as.character(formula)[2]  # Extract the left side of the formula
  
  # Permitted transformations
  transformations <- c("log", "sqrt", "exp", "I\\(1/", "boxcox")
  
  for (t in transformations) {
    if (grepl(paste0("^", t), lhs)) {
      return(ifelse(t == "I\\(1/", "inverse", gsub("\\\\", "", t)))  # "I(1/" wird als "inverse" benannt
    }
  }
  
  return(NULL)  # No transformation 
}
#
#
#
# Transformation
# Identifies which variables need to be transformed in a formula, extracts transformations and operators, checks for existing variables and missing values and returns the results in a list.
identify_variables <- function(formula, data, target_col) {
  data <- as.data.frame(data)
  
  if (is.list(formula)) {
    results <- lapply(formula, function(f) identify_variables(f, data, target_col))
    return(results)
  }
  
  # Extract formula as character string
  formchar <- as.character(formula)
  lhs <- gsub("^I\\(1/|log\\(|sqrt\\(|boxcox\\(|exp\\(|\\)$| ", "", formchar[2])   # Entferne Transformationen und Leerzeichen von der linken Seite
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
#
#
#
# Find formula  
# Selects from a list of formulas the one whose left side corresponds to the cleaned answer variable by removing transformations and spaces.
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
#
#
#
#
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
#
#
#
#
# Maps PMM-k values per NA variable, respecting PMM on/off flags.
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
#
#
#
#
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
#
#
#
#
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
#
#
#
# Checking learner_params
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
  
  # 2) GLOBAL-MODUS: only okay if ONE Method and NO var-/method-Key 
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
  
  # 5) VARIABLE-MODUS
  if (has_var_keys && !has_meth_keys) {
    out <- setNames(vector("list", length(variables)), variables)
    for (v in variables) {
      out[[v]] <- if (v %in% var_keys) learner_params[[v]] else list()
    }
    return(out)
  }
  
  # 6) METHOD-MODUS
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
# Precheck 
# Performs a series of preliminary checks, including checking for missing values, data types, formulas, PMM settings and supported methods, and returns the checked data and variable information.
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
    default_method = NULL
) {
  # -------------------------------------------------------------------------
  # 1) Identify variables and variables containing missing values
  # -------------------------------------------------------------------------
  variables      <- colnames(data)
  variables_NA   <- variables[apply(data, 2, function(x) any(is.na(x)))]
  
  if (length(variables_NA) == 0) {
    stop("Error: No variables with missing data found.")
  }
  
  # -------------------------------------------------------------------------
  # 2) Warn if any variable name equals a reserved method name
  #    (this would cause ambiguity in learner_params)
  # -------------------------------------------------------------------------
  reserved_methods <- c("ranger", "xgboost", "regularized", "robust")
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
  if (is.data.table(data)) {
    message("data is data.table")
  } else if (is.matrix(data) || is.data.frame(data)) {
    data <- as.data.table(data)
  } else {
    stop("Error: Input must be a data.frame, matrix, or data.table.")
  }
  
  # -------------------------------------------------------------------------
  # 4) Validate formula list (if provided)
  # -------------------------------------------------------------------------
  if (!identical(formula, FALSE)) {
    if (!is.list(formula)) stop("'formula' must be FALSE or a named list of formulas.")
    
    for (var in names(formula)) {
      if (!inherits(formula[[var]], "formula"))
        stop(sprintf("Element for variable '%s' is not a valid formula.", var))
      
      # Check that all variables referenced exist in the dataset
      form_vars <- all.vars(formula[[var]])
      missing_vars <- setdiff(form_vars, names(data))
      if (length(missing_vars) > 0) {
        stop(sprintf("Formula for '%s' references unknown variables: %s",
                     var, paste(missing_vars, collapse=", ")))
      }
    }
  }
  
  # -------------------------------------------------------------------------
  # 5) Normalize 'method' argument so every variable has a method
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # 5) Normalize 'method' argument so every NA-variable gets a valid method
  # -------------------------------------------------------------------------
  supported_methods <- c("ranger", "regularized", "xgboost", "robust")
  
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
      stop(sprintf("Unsupported method '%s'.", method))
    }
    
    method <- setNames(as.list(rep(method, length(variables_NA))), variables_NA)
    
  } else if (is.list(method) && length(method) == 1L && is.character(method[[1]])) {
    
    m <- method[[1]]
    if (!(m %in% supported_methods)) {
      stop(sprintf("Unsupported method '%s'.", m))
    }
    
    method <- setNames(as.list(rep(m, length(variables_NA))), variables_NA)
    
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
      stop("One or more unsupported methods found in 'method'.")
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
      stop("One or more unsupported methods found in 'method'.")
    }
    
    # mapping
    method <- setNames(as.list(all_methods[seq_along(variables_NA)]), variables_NA)
    
  } else {
    stop("Invalid 'method' specification: must be a single method or a (named) list.")
  }
  
  # -------------------------------------------------------------------------
  # 6) Validate 'regularized' method compatibility (glmnet)
  # -------------------------------------------------------------------------
  for (var in variables_NA) {
    y_obs <- data[[var]][!is.na(data[[var]])]
    
    if (method[[var]] %in% c("regularized", "glmnet")) {
      
      if (is.factor(y_obs) && any(table(y_obs) <= 1)) {
        warning(sprintf("Target '%s' has too few observations per class for 'regularized'. Falling back to 'robust'.", var))
        method[[var]] <- "robust"
        next
      }
      
      if (is.numeric(y_obs) && length(unique(y_obs)) < 3) {
        warning(sprintf("Target '%s' has too few unique values for 'regularized'. Falling back to 'robust'.", var))
        method[[var]] <- "robust"
        next
      }
      
      predictors <- setdiff(names(data), var)
      for (col in predictors) {
        x_obs <- data[[col]][!is.na(data[[col]])]
        
        if (is.factor(x_obs) && any(table(x_obs) <= 1)) {
          warning(sprintf("Predictor '%s' unsuitable for glmnet. Falling back to 'robust' for '%s'.", col, var))
          method[[var]] <- "robust"
          break
        }
        
        if (is.numeric(x_obs) && length(unique(x_obs)) < 2) {
          warning(sprintf("Predictor '%s' unsuitable for glmnet. Falling back to 'robust' for '%s'.", col, var))
          method[[var]] <- "robust"
          break
        }
      }
    }
  }
  
  # -------------------------------------------------------------------------
  # 7) Warning for variables with > 50% missing values
  # -------------------------------------------------------------------------
  missing_counts <- colSums(is.na(data))
  na_cols <- names(missing_counts[missing_counts > 0.5 * nrow(data)])
  if (length(na_cols) > 0) {
    warning(sprintf("Variables with >50%% missing values: %s", paste(na_cols, collapse=", ")))
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
  
  message("Precheck done.")
  
  return(list(
    data           = data,
    variables      = variables,
    variables_NA   = variables_NA,
    method         = method,
    learner_params = checked_learner_params,
    pmm            = checked_pmm,
    pmm_k          = checked_pmm_k,
    pmm_k_method   = checked_pmm_k_method,
    tune           = checked_tune
  ))
}
#
#
#
# Semicontinous variables
is_semicontinuous <- function(x) {
  is.numeric(x) &&
    any(x == 0, na.rm = TRUE) &&
    any(x > 0, na.rm = TRUE) &&
    sum(x == 0, na.rm = TRUE) / sum(!is.na(x)) > 0.1 &&
    !all(na.omit(x) %in% c(0, 1))  # check that it is not binary
}

#
#
#
# factor levels
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

#
#
#
# check levels
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

#
#
#
# new levels -> NA
set_new_levels_to_na <- function(df, factor_levels, data_y_fill_final, skip_methods = c("xgboost"), method_var = NULL) {
  if (!is.null(method_var) && !(method_var %in% skip_methods)) {
    for (col in names(factor_levels)) {
      if (col %in% names(df) && is.factor(df[[col]]) && col %in% names(data_y_fill_final)) {
        valid_levels <- levels(data_y_fill_final[[col]])
        df[[col]][!df[[col]] %in% valid_levels & !is.na(df[[col]])] <- NA
      }
    }
  }
  return(df)
}

#
#
#
#
check_factor_levels <- function(data, original_levels) {
  for (colname in names(original_levels)) {
    if (colname %in% names(data)) {
      if (is.factor(data[[colname]])) {
        data_levels <- levels(data[[colname]])
        ref_levels <- original_levels[[colname]]
        
        # 1. Check for missing levels (levels in the original that are not in the current factor)
        missing_levels <- setdiff(ref_levels, data_levels)
        if (length(missing_levels) > 0) {
          warning(sprintf("Column '%s': Missing levels in factor: %s", colname, paste(missing_levels, collapse = ", ")))
        }
        
        # 2. Check for new levels (levels in the data factor that are not in the original)
        new_levels <- setdiff(data_levels, ref_levels)
        if (length(new_levels) > 0) {
          warning(sprintf("Column '%s': New levels in factor: %s", colname, paste(new_levels, collapse = ", ")))
        }
        
        # 3. Check whether values in the data factor are outside the defined levels (can happen if factors are not set correctly)
        invalid_values <- setdiff(unique(as.character(data[[colname]])), ref_levels)
        if (length(invalid_values) > 0) {
          warning(sprintf("Column '%s': Invalid value: %s", colname, paste(invalid_values, collapse = ", ")))
        }
      }
    }
  }
}
#
#
#
# helper: inverse Transformation
inverse_transform <- function(x, method) {
  switch(method,
         exp = log(x),
         log = exp(x),
         sqrt = x^2,
         inverse = 1 / x,
         stop("Unknown transformation: ", method)
  )
}
#
#
#
# helper decimal places
get_decimal_places <- function(x) {
  if (is.na(x)) return(0)
  if (x == floor(x)) return(0)
  nchar(sub(".*\\.", "", as.character(x)))
}
#
#
#
# helper: ranger regression prediction via per-tree median
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
