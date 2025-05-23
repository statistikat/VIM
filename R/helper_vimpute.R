register_robust_learners <- function() {
  
  # Robust Regression Learner
  LearnerRegrRobustLM = R6::R6Class(
    classname = "LearnerRegrRobustLM",
    inherit = LearnerRegr,
    public = list(
      initialize = function() {
        
        # definition hyperparameter
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
          #compute.rd = p_lgl(default = FALSE),
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
          #compute.rd = FALSE,
          refine.tol = 1e-7,
          solve.tol = 1e-7,
          trace.lev = 0
        )
      }
    ),
    
    private = list(
      .train = function(task) {
        
        # train data
        pv = self$param_set$get_values() 
        data = as.data.frame(task$data())
        target = task$target_names
        features = task$feature_names
        
        # handle factors
        factor_cols = sapply(data, is.factor)
        if (any(factor_cols)) {
          for (col in names(data)[factor_cols]) {
            data[[col]] = droplevels(data[[col]])
          }
        }
        
        # model matrix
        formula = reformulate(features, response = target)
        #new
        # control = robustbase::lmrob.control(
        #   method = pv$method,
        #   psi = pv$psi,
        #   tuning.chi = pv$tuning.chi,
        #   tuning.psi = pv$tuning.psi,
        #   setting = pv$setting,
        #   max.it = pv$max.it,
        #   k.max = pv$k.max,
        #   nResample = pv$nResample,
        #   subsampling = pv$subsampling
        # )
        control = do.call(robustbase::lmrob.control, pv)
        model = tryCatch(
          robustbase::lmrob(formula, data = data, control = control),
          error = function(e) {
            warning(sprintf("lmrob() failed for target '%s': %s", target, e$message))
            return(NULL)
          }
        )
        
        if (is.null(model)) {
          stopf("Robuste Regression mit lmrob() ist fehlgeschlagen für Zielvariable '%s'", target)
        }
        #new end
        
        # store factor levels 
        self$state$factor_levels = lapply(data[, factor_cols, drop = FALSE], levels)
        return(model)
      },
      
      .predict = function(task) {
        model = self$model
        newdata = as.data.frame(task$data())
        
        # handle factor levels
        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
              new_levels = setdiff(levels(newdata[[var]]), self$state$factor_levels[[var]])
              if (length(new_levels) > 0) {
                warning(sprintf("New levels (%s) in factor '%s' replaced with NA", 
                                paste(new_levels, collapse = ", "), var))
              }
              # Faktor mit Trainingslevels forcieren
              newdata[[var]] = factor(newdata[[var]], levels = self$state$factor_levels[[var]])
            }
          }
        }
        # Standard prediction
        response = predict(model, newdata = newdata)
        
        PredictionRegr$new(task = task, response = response)
      }
    )
  )
  
  #       # prediction
  #       if (inherits(model, "ridge_lm")) {
  #         X_new = model.matrix(delete.response(terms(model)), newdata) #Falls das Modell eine Ridge-Regression (ridge_lm) ist
  #         response = X_new %*% model$coefficients
  #       } else {
  #         response = predict(model, newdata = newdata)
  #       }
  #       
  #       PredictionRegr$new(task = task, response = response)
  #     }
  #   )
  # )
  # register the learner
  mlr3::mlr_learners$add("regr.lm_rob", LearnerRegrRobustLM)
  
  
  
  # robust Classification Learner
  LearnerClassifGlmRob <- R6::R6Class(
    classname = "LearnerClassifGlmRob",
    inherit = LearnerClassif,
    public = list(
      initialize = function() {
        
        # parameter
        param_set = ps(
          method = p_fct(c("Mqle", "WBY"), default = "Mqle"), #standard = Maximum Quasi-Likelihood Estimation, Weighted Bianco-Yohai Estimator = especially recommended for binary data
          #psi = p_fct(c("bisquare", "lqq", "hampel", "optimal"), default = "bisquare"), #Funktion für die Gewichtung der Residuen
          #tuning.chi = p_dbl(lower = 0, upper = Inf, default = 1.547), #Tuning-Parameter χ-function
          #tuning.psi = p_dbl(lower = 0, upper = Inf, default = 1.547), #Tuning-Parameter  ψ-function
          acc = p_dbl(lower = 0, upper = Inf, default = 1e-4), #Tolerance for the convergence of the algorithm. The algorithm stops if the changes in the estimates are smaller than this value
          test.acc = p_fct(c("coef", "resid"), default = "coef"), #Criterion for the convergence test, based on the changes in the coefficients vs. residuals
          #max.it = p_int(lower = 1, upper = Inf, default = 50), 
          #trace.lev = p_int(lower = 0, upper = 10, default = 0), 
          #weights.on.x = p_fct(c("none", "hat", "robCov"), default = "none"), 
          family = p_fct(c("binomial", "poisson", "Gamma"), default = "binomial"),  
          maxit = p_int(lower = 1, upper = 500, default = 50), # Maximum number of iterations that the algorithm may perform to find a solution.
          tcc = p_dbl (lower = 1, upper = 2, default = 1.345), #Tuning constant
          ridge_lambda = p_dbl(lower = 0, upper = 1, default = 1e-4)
        )
        #param_set
        super$initialize(
          id = "classif.glm_rob",
          feature_types = c("numeric", "integer", "factor", "ordered"),
          predict_types = c("response", "prob"),
          packages = c("robustbase"),
          properties = c( "twoclass"),
          man = "robustbase::glmrob",
          param_set = param_set
        )
        
        self$param_set$values = list(
          method = "Mqle",
          #psi = "bisquare",
          #tuning.chi = 1.548,
          #tuning.psi = 4.685,
          acc = 1e-4,
          test.acc = "coef",
          #max.it = 100,
          #trace.lev = 0,
          #weights.on.x = "none",
          family = "binomial",
          maxit = 50,
          tcc = 1.345,
          ridge_lambda = 1e-4 
        )
        
      }
    ),
    
    private = list(
      .train = function(task) {
        pv = self$param_set$get_values()
        data = as.data.frame(task$data())
        target = task$target_names
        features = task$feature_names
        features <- setdiff(features, target)
        
        # Handle factors properly
        factor_cols = sapply(data, is.factor)
        
        # Designmatrix
        formula = reformulate(features, response = target)
        X = tryCatch({
          model.matrix(formula, data)
        }, error = function(e) {
          stop("Failed to create design matrix: ", e$message)
        })
        
        if (ncol(X) == 0) {
          stop("Design matrix has 0 columns - check factor levels and collinearity")
        }
        
        # Check for separation
        if (any(apply(X, 2, function(x) max(abs(tapply(data[[target]], x, mean, na.rm=TRUE)) %in% c(0,1))))) {
          warning("Perfect separation detected - applying ridge regularization")
        }
        
        
        # Fit with regularization fallback
        model = tryCatch({
          robustbase::glmrob(
            formula,
            data = data,
            family = binomial(),
            method = pv$method,
            control = glmrobMqle.control(
              acc = pv$acc,
              test.acc = pv$test.acc,
              maxit = pv$maxit,
              tcc = pv$tcc
            )
          )
        }, error = function(e) {
          warning("Falling back to ridge-regularized solution: ", e$message)
          
          # Manual ridge implementation
          y = as.numeric(data[[target]]) - 1
          if (ncol(X) == 0) stop("No features available for ridge regression")
          lambda = ifelse(is.null(pv$ridge_lambda), 1e-4, pv$ridge_lambda)
          XtX = crossprod(X)
          diag(XtX) = diag(XtX) + lambda
          
          print("lambda: ")
          print(lambda)
          
          # Ridge penalty
          beta = tryCatch(
            solve(XtX, crossprod(X, y)),
            error = function(e) {
              warning("Using pseudoinverse due to singular system")
              MASS::ginv(XtX) %*% crossprod(X, y)
            }
          )
          
          # Create dummy model object
          structure(
            list(
              coefficients = beta,
              fitted.values = plogis(X %*% beta),
              formula = formula,
              data = data,
              levels = levels(data[[target]])
            ),
            class = "ridge_glm"
          )
        })
        
        # Store factor levels
        self$state$factor_levels = lapply(data[, factor_cols, drop = FALSE], levels)
        
        return(model)
      },
      
      .predict = function(task) {
        model = self$model
        newdata = as.data.frame(task$data())
        
        # unknown levels
        if (!is.null(self$state$factor_levels)) {
          for (var in names(self$state$factor_levels)) {
            if (var %in% colnames(newdata) && is.factor(newdata[[var]])) {
              new_levels = setdiff(levels(newdata[[var]]), self$state$factor_levels[[var]])
              if (length(new_levels) > 0) {
                # max level
                mode_level = names(which.max(table(self$model$data[[var]])))
                newdata[[var]] = ifelse(newdata[[var]] %in% new_levels, 
                                        mode_level, 
                                        as.character(newdata[[var]]))
                newdata[[var]] = factor(newdata[[var]], levels = self$state$factor_levels[[var]])
              }
            }
          }
        }
        
        if (inherits(model, "ridge_glm")) {
          X_new = tryCatch(
            model.matrix(delete.response(terms(model$formula)), newdata),
            error = function(e) stop("Prediction matrix creation failed: ", e$message)
          )
          prob = plogis(X_new %*% model$coefficients)
        } else {
          prob = predict(model, newdata = newdata, type = "response")
        }
        
        # prediction
        prob_matrix = cbind(1 - prob, prob)
        colnames(prob_matrix) = levels(task$truth())
        
        PredictionClassif$new(
          task = task,
          response = ifelse(prob > 0.5, levels(task$truth())[2], levels(task$truth())[1]),
          prob = prob_matrix
        )
      }
    )
  )
  # Register the robust classification learner
  mlr3::mlr_learners$add("classif.glm_rob", LearnerClassifGlmRob)
}


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
  } else if (is.character(method) && length(method) == 1) {
    # If `method` is a single string, create a list with this value for all variables
    method <- setNames(as.list(rep(method, length(variables))), variables)
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
    stop("Error: 'method' must either be empty, have the same length as 'variables' or 'considered_variables' (if specified) or the number of variables must match NAs.")
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
