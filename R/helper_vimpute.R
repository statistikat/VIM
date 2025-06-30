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
  LearnerClassifGlmRob <- R6::R6Class(
    inherit = mlr3::LearnerClassif,
    public = list(
      initialize = function() {
        super$initialize(
          id = "classif.glm_rob",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response", "prob"),
          packages = c("mlr3learners"),
          properties = c("twoclass", "multiclass")
        )
        self$state$learner = NULL
      }
    ),
    
    private = list(
      .train = function(task) {
        n_classes = length(task$class_names)
        if (n_classes == 2) {
          self$state$learner = mlr3::lrn("classif.log_reg", predict_type = self$predict_type)
        } else {
          self$state$learner = mlr3::lrn("classif.multinom", predict_type = self$predict_type)
        }
        self$state$learner$train(task)
      },
      
      .predict = function(task) {
        if (is.null(self$state$learner)) stop("Model not trained yet")
        pred = self$state$learner$predict(task)
        return(pred)
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
# left handside formula
# extracts the left side of a formula and returns the name of the applied transformation (e.g. “log”)
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
# Precheck 
# Performs a series of preliminary checks, including checking for missing values, data types, formulas, PMM settings and supported methods, and returns the checked data and variable information.
precheck <- function(
    data,
    pmm,
    formula,
    method,
    sequential
) {
  
  # check missing data
  variables = colnames(data)
  variables_NA  = colnames(data)[apply(data, 2, function(x) any(is.na(x)))]   # alle Variablen die missind data haben
  if (length(variables_NA) == 0) {
    stop ("Error: No missing data available")
  } else {
    message ("Variables with Missing Data: ", paste (variables_NA, collapse = ","))
  }
  
  # check data structure
  if (is.data.table(data)) {
    message("data is data.table")  
  } else if (is.matrix(data) || is.data.frame(data)) {
    data <- as.data.table(data)  
  } else {
    stop("Error: Input must be a dataframe or a data.table.")
  }
  
  # check formula
  if (!identical(formula, FALSE)) {
    if (!is.list(formula)) {
      stop("Error: 'formula' must be either FALSE or a list of formulas.")
    }
    
    for (var in names(formula)) {
      if (!inherits(formula[[var]], "formula")) {
        stop(paste("Error: Element for ", var, " is not a valid formula!"))
      }
      
      # Extract variables from the formula
      form_vars <- all.vars(formula[[var]])
      
      # Check whether all variables are present in the data
      missing_vars <- setdiff(form_vars, names(data))
      if (length(missing_vars) > 0) {
        stop(paste("Missing variables in data:", paste(missing_vars, collapse = ", ")))
      }
    }
  }
  
  # check pmm
  check_pmm <- function(
    pmm, 
    variables
  ) {
    if (!(is.logical(pmm) && length(pmm) == 1) &&
        !(is.list(pmm) && (length(pmm) == length(variables) || length(pmm) == length(variables_NA)) && all(sapply(pmm, is.logical)))) {
      stop("Error: pmm must contain a list of the length of 'variables' or 'considered_variables' (if specified) with TRUE/FALSE values.")
    }
  }
  check_pmm(pmm, variables)
  
  # check methods
  supported_methods <- c("ranger", "regularized", "xgboost", "robust")
  
  # proove methods
  if (length(method) == 0) {
    message("Methods are empty, no imputation is used.")
  } else if ((is.character(method) && length(method) == 1) ||
            (is.list(method) && length(method) == 1 && is.character(method[[1]]))) {
    # If `method` is a single string or a single-element list, set for all variables
    method_value <- if (is.list(method)) method[[1]] else method
    if (!(method_value %in% supported_methods)) {
      stop("Error: The provided method is not supported.")
    }
    method <- setNames(as.list(rep(method_value, length(variables))), variables)
  } else if (length(method) == length(variables) || length(method) == length(variables_NA)) {
    # If `method` has the same length as `variables` or the number of variables matches NAs
    if (is.list(method) && all(sapply(method, is.character))) {
      # Check whether all elements in `method` are strings and contain valid methods
      if (!all(unlist(method) %in% supported_methods)) {
        stop("Error: One or more methods in the list are not supported.")
      }
    } else {
      stop("Error: 'method' must be a list of string values.")
    }
  } else {
    stop("Error: 'method' must either be empty, a single string, a single-element list, have the same length as 'variables' or 'considered_variables' (if specified), or the number of variables must match NAs.")
  }
  
  # warning if more than 50% missing values
  if (nrow(data) == 0) stop("Error: Data has no rows.")
  missing_counts <- colSums(is.na(data))
  na_cols <- names(missing_counts[missing_counts > 0.5 * nrow(data)])
  if (length(na_cols) > 0) {
    warning(paste("Warning: The following variables have more than 50% missing values:", paste(na_cols, collapse = ", ")))
  }
  
  # Datatypes
  data[, (variables) := lapply(.SD, function(x) {
    if (is.numeric(x)) {
      as.numeric(x)  # Integer & Double in Numeric 
    } else if (is.character(x)) {
      as.factor(x)  # Strings in Factors
    } else if (is.logical(x)) {
      as.numeric(x)  # TRUE/FALSE ->  1/0
    } else if (is.factor(x)) {
      x  
    } else {
      stop("Error: Unknown datatype")
    }
  }), .SDcols = variables]
  
  message("Precheck done.")
  return(list(data=data, variables=variables, variables_NA=variables_NA, method=method))
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
    !all(na.omit(x) %in% c(0, 1))  # explizit keine binären Variablen
}






# ## alt
# register_robust_learners <- function() {
#   
#   # Robust Regression Learner
#   LearnerRegrRobustLM = R6::R6Class(
#     classname = "LearnerRegrRobustLM",
#     inherit = LearnerRegr,
#     public = list(
#       initialize = function() {
#         param_set = ps(
#           method = p_fct(c("M", "MM"), default = "MM"),
#           psi = p_fct(c("bisquare", "lqq", "optimal"), default = "bisquare"),
#           tuning.chi = p_dbl(lower = 0, upper = Inf, default = 1.55),
#           tuning.psi = p_dbl(lower = 0, upper = Inf, default = 4.69),
#           setting = p_fct(c("KS2014", "KS2011"), default = "KS2014"),
#           max.it = p_int(lower = 1, upper = Inf, default = 50),
#           k.max = p_int(lower = 1, upper = Inf, default = 200),
#           nResample = p_int(lower = 1, upper = Inf, default = 500),
#           subsampling = p_fct(c("simple", "nonsingular"), default = "nonsingular"),
#           ridge_lambda = p_dbl(lower = 0, upper = 1, default = 1e-4),
#           refine.tol = p_dbl(lower = 0, upper = Inf, default = 1e-7),
#           solve.tol = p_dbl(lower = 0, upper = Inf, default = 1e-7),
#           trace.lev = p_int(lower = 0, upper = Inf, default = 0)
#         )
#         
#         super$initialize(
#           id = "regr.lm_rob",
#           feature_types = c("numeric", "integer", "factor", "ordered"),
#           predict_types = c("response"),
#           packages = c("robustbase", "stats"),
#           man = "robustbase::lmrob",
#           param_set = param_set
#         )
#         
#         self$param_set$values = list(
#           method = "MM",
#           psi = "bisquare",
#           tuning.chi = 1.55,
#           tuning.psi = 4.69,
#           setting = "KS2014",
#           max.it = 50,
#           k.max = 200,
#           nResample = 500,
#           subsampling = "nonsingular",
#           ridge_lambda = 1e-4,
#           refine.tol = 1e-7,
#           solve.tol = 1e-7,
#           trace.lev = 0
#         )
#       }
#     ),
#     
#     private = list(
#       .train = function(task) {
#         pv = self$param_set$get_values()
#         data = as.data.frame(task$data())
#         target = task$target_names
#         features = task$feature_names
#         
#         factor_cols = sapply(data, is.factor)
#         if (any(factor_cols)) {
#           for (col in names(data)[factor_cols]) {
#             data[[col]] = droplevels(data[[col]])
#           }
#         }
#         
#         formula = reformulate(features, response = target)
#         control = do.call(robustbase::lmrob.control, pv)
#         
#         model = tryCatch(
#           robustbase::lmrob(formula, data = data, control = control),
#           error = function(e) {
#             warning(sprintf("lmrob() failed for '%s': %s\nFalling back to lm()", target, e$message))
#             NULL
#           }
#         )
#         
#         if (is.null(model)) {
#           # Fallback: lm
#           model = lm(formula, data = data)
#           class(model) = c("lm_fallback", class(model))
#           self$state$used_fallback = TRUE
#         } else {
#           self$state$used_fallback = FALSE
#         }
#         
#         factor_col_names = names(data)[factor_cols]  # Namen der Faktor-Spalten
#         self$state$factor_levels = lapply(data[, factor_col_names, drop = FALSE], levels)
#         return(model)
#       },
#       
#       .predict = function(task) {
#         model = self$model
#         newdata = as.data.frame(task$data())
#         
#         if (!is.null(self$state$factor_levels)) {
#           for (var in names(self$state$factor_levels)) {
#             if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
#               new_levels = setdiff(levels(newdata[[var]]), self$state$factor_levels[[var]])
#               if (length(new_levels) > 0) {
#                 warning(sprintf("New levels (%s) in factor '%s' replaced with NA",
#                                 paste(new_levels, collapse = ", "), var))
#               }
#               newdata[[var]] = factor(newdata[[var]], levels = self$state$factor_levels[[var]])
#             }
#           }
#         }
#         
#         response = tryCatch({
#           predict(model, newdata = newdata)
#         }, error = function(e) {
#           warning("Vorhersage fehlgeschlagen: ", e$message)
#           rep(NA_real_, nrow(newdata))
#         })
#         
#         PredictionRegr$new(task = task, response = response)
#       }
#     )
#   )
#   
#   mlr3::mlr_learners$add("regr.lm_rob", LearnerRegrRobustLM)
#   
#   
#   # robust Classification Learner
#   LearnerClassifGlmRob <- R6::R6Class(
#     classname = "LearnerClassifGlmRob",
#     inherit = mlr3::LearnerClassif,
#     public = list(
#       initialize = function() {
#         param_set = ps(
#           method = p_fct(c("Mqle", "WBY"), default = "Mqle"),
#           acc = p_dbl(lower = 0, upper = Inf, default = 1e-4),
#           test.acc = p_fct(c("coef", "resid"), default = "coef"),
#           family = p_fct(c("binomial"), default = "binomial"),
#           maxit = p_int(lower = 1, upper = 500, default = 50),
#           tcc = p_dbl(lower = 1, upper = 2, default = 1.345)
#         )
#         
#         super$initialize(
#           id = "classif.glm_rob",
#           feature_types = c("numeric", "integer", "factor", "ordered"),
#           predict_types = c("response", "prob"),
#           packages = c("robustbase", "nnet", "MASS"),
#           properties = c("twoclass", "multiclass"),
#           man = "robustbase::glmrob",
#           param_set = param_set
#         )
#         
#         self$param_set$values = list(
#           method = "Mqle",
#           acc = 1e-4,
#           test.acc = "coef",
#           family = "binomial",
#           maxit = 50,
#           tcc = 1.345
#         )
#       }
#     ),
#     
#     private = list(
#       .train = function(task) {
#         pv = self$param_set$get_values()
#         data = task$data()
#         data = as.data.frame(data)
#         
#         target = task$target_names
#         n_classes = length(unique(task$truth()))
#         
#         self$state$n_classes = n_classes
#         self$state$is_multiclass = n_classes > 2
#         self$state$target_levels = task$class_names
#         
#         if (self$state$is_multiclass) {
#           message("Target has more than 2 classes. Falling back to classif.glmnet.")
#           #return(private$.fallback_model_glmnet(task))
#           model = private$.fallback_model_glmnet(task)
#           return(model)
#         }
#         
#         formula = task$formula()
#         family_fun = match.fun(pv$family)
#         family_obj = family_fun()
#         
#         model = tryCatch({
#           robustbase::glmrob(
#             formula,
#             data = data,
#             family = family_obj,
#             method = pv$method,
#             control = robustbase::glmrobMqle.control(
#               acc = pv$acc,
#               test.acc = pv$test.acc,
#               maxit = pv$maxit,
#               tcc = pv$tcc
#             )
#           )
#         }, warning = function(w) {
#           message("Warning in glmrob: ", w$message)
#           NULL
#         }, error = function(e) {
#           message("Error in glmrob: ", e$message)
#           NULL
#         })
#         
#         if (is.null(model) || is.null(model$fitted.values) || length(model$fitted.values) == 0) {
#           message("glmrob returned invalid model. Falling back to glmnet.")
#           model = private$.fallback_model_glmnet(task)
#         }
#         
#         if (is.null(model)) {
#           message("Falling back to glmnet due to glmrob issues")
#           model = private$.fallback_model_glmnet(task)
#         }
#         
#         factor_cols = sapply(data, is.factor)
#         if (any(factor_cols)) {
#           factor_col_names = names(which(factor_cols))
#           self$state$factor_levels = lapply(data[, factor_col_names, drop = FALSE], levels)
#         } else {
#           self$state$factor_levels = NULL
#         }
#         
#         self$model = model
#         return(model)
#         #return(model)
#       },
#       
#       
#       .fallback_model_glmnet = function(task) {
#         
#         data = as.data.frame(task$data(cols = task$feature_names))
#         target = task$target_names
#         # features = task$feature_names
#         
#         # Prüfen, ob Faktoren in Features vorhanden sind
#         if (any(sapply(data, is.factor))) {
#           X = model.matrix(~ . - 1, data = data)
#           data_encoded = as.data.frame(X)
#           data_encoded[[target]] = task$data(cols = target)[[1]]
#           
#           # Zielvariable anhängen
#           # target = task$data(cols = task$target_names)
#           # data_encoded[[task$target_names]] = target[[1]]
#           
#           # Neuer Task mit numerischen Features
#           if (length(task$class_names) == 2) {
#             task_encoded = mlr3::TaskClassif$new(
#               id = paste0(task$id, "_encoded"),
#               backend = data_encoded,
#               target = target,
#               positive = task$class_names[2]
#             )
#           } else {
#             task_encoded = mlr3::TaskClassif$new(
#               id = paste0(task$id, "_encoded"),
#               backend = data_encoded,
#               target = target
#             )
#           }
#           
#           # Store column names for prediction alignment
#           self$state$train_matrix_colnames = colnames(X)
#           self$state$factor_levels = NULL  # Not needed for encoded data
#         } else {
#           task_encoded = task$clone()
#           # Store factor levels for non-encoded data
#           factor_cols = sapply(data, is.factor)
#           if (any(factor_cols)) {
#             self$state$factor_levels = lapply(data[, factor_cols, drop = FALSE], levels)
#           } else {
#             self$state$factor_levels = NULL
#           }
#           # Store column names from model matrix
#           model_matrix = model.matrix(~ ., data)[, -1, drop = FALSE]
#           self$state$train_matrix_colnames = colnames(model_matrix)
#         }
#         
#         learner = mlr3::lrn("classif.glmnet",
#                             alpha = 0,
#                             lambda = 0.1,
#                             predict_type = self$predict_type)
#         learner$train(task_encoded)
#         return(learner)
#       },
#       
#       
#       .predict = function(task) {
#         if (is.null(self$model)) stop("Learner has not been trained yet")
#         
#         newdata = task$data()
#         
#         # Faktor-Levels korrekt setzen, um Konsistenz mit Trainingsdaten sicherzustellen
#         if (!is.null(self$state$factor_levels)) {
#           for (colname in names(self$state$factor_levels)) {
#             if (colname %in% colnames(newdata)) {
#               lvls = self$state$factor_levels[[colname]]
#               newdata[[colname]] = factor(as.character(newdata[[colname]]), levels = lvls)
#             }
#           }
#         }
#         
#         # Wenn das Modell ein mlr3-Learner ist (z. B. glmnet fallback)
#         if (inherits(self$model, "Learner")) {
#           
#           # Repliziere die Struktur der Trainingsdaten (Matrix-Kodierung)
#           if (!is.null(self$state$train_matrix_colnames)) {
#             mm = model.matrix(~ . - 1, data = newdata)  # ohne Intercept
#             X_new = as.matrix(mm)
#             
#             # Fehlende Spalten mit 0 auffüllen
#             missing_cols = setdiff(self$state$train_matrix_colnames, colnames(X_new))
#             if (length(missing_cols) > 0) {
#               X_new = cbind(X_new, matrix(0, nrow = nrow(X_new), ncol = length(missing_cols),
#                                           dimnames = list(NULL, missing_cols)))
#             }
#             
#             # Spaltenreihenfolge anpassen
#             X_new = X_new[, self$state$train_matrix_colnames, drop = FALSE]
#             newdata = as.data.frame(X_new)
#           }
#           
#           # Vorhersage durch mlr3 Learner
#           pred = self$model$predict_newdata(newdata)
#           
#           if (self$predict_type == "prob") {
#             return(mlr3::PredictionClassif$new(task = task, prob = pred$prob))
#           } else {
#             return(mlr3::PredictionClassif$new(task = task, response = pred$response))
#           }
#           
#         } else {
#           # Klassische Vorhersage über glmrob-Modell (nur binär)
#           prob = predict(self$model, newdata = newdata, type = "response")
#           prob_matrix = cbind(1 - prob, prob)
#           colnames(prob_matrix) = self$state$target_levels
#           
#           if (self$predict_type == "prob") {
#             return(mlr3::PredictionClassif$new(task = task, prob = prob_matrix))
#           } else {
#             response = ifelse(prob > 0.5, self$state$target_levels[2], self$state$target_levels[1])
#             return(mlr3::PredictionClassif$new(task = task, response = response))
#           }
#         }
#       },
#       
#       .handle_new_levels = function(newdata) {
#         if (is.null(self$state$factor_levels)) return(newdata)
#         
#         for (var in names(self$state$factor_levels)) {
#           if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
#             current_levels = levels(newdata[[var]])
#             valid_levels = self$state$factor_levels[[var]]
#             new_levels = setdiff(current_levels, valid_levels)
#             
#             if (length(new_levels) > 0) {
#               # Replace new levels with the most common level
#               mode_level = names(which.max(table(newdata[[var]][newdata[[var]] %in% valid_levels])))
#               newdata[[var]] = as.character(newdata[[var]])
#               newdata[[var]][newdata[[var]] %in% new_levels] = mode_level
#               newdata[[var]] = factor(newdata[[var]], levels = valid_levels)
#             }
#           }
#         }
#         return(newdata)
#       }
#     )
#   )
#   
#   # Register the learner
#   mlr3::mlr_learners$add("classif.glm_rob", LearnerClassifGlmRob)
#   
# }
