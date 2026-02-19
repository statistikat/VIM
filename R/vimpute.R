
#' Impute missing values with prefered Model, sequentially, with hyperparametertuning and with PMM (if wanted)
#' Need of 'helper_vimpute' script

## PARAMETERS ##
#' @param data
#'  Dataset with missing values. Provide as a data.table.
#' @param considered_variables
#'  A character vector of variable names to be either imputed or used as predictors, excluding irrelevant columns from the imputation process.
#' @param method
#'  Specifies the imputation method for each variable.
#'  Can be provided either:
#'    - as a **single global method** (e.g. "ranger"), applied to all variables, or
#'    - as a **named list** (e.g. as.list(var1 = "xgboost", var2="robust")), assigning a method to each variable individually.
#'  Supported methods:
#     - ranger (Random Forest)
#     - xgboost (Gradient Boosting)
#     - regularized (glmnet regression/classification)
#     - robust (robustbase::lmrob / glmrob) 
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
#' @param learner_params 
#'  Hyperparameters for the chosen methods.
#'  Can be provided in **three ways**:
#'    - **Per variable**  (e.g. list(mpg = list(num.trees = 500)))  
#'    - **Per method**    (e.g. list(ranger = list(num.trees = 600)))  
#'    - **Global**, applied to all variables using the same method
#' @param formula
#'  Optional modeling formula to restrict or transform predictor variables.  
#'  Only supported for **regularized** (glmnet) and **robust** (lmrob/glmrob) methods
#'  Provide as a named list, e.g.:
#'    - list(mpg = mpg ~ hp + drat)  
#'    - list(hp  = log(hp) ~ wt + cyl) 
#'  For X: follows the rules of model.matrix
#'  For Y: transformations supported are log(), exp(), sqrt(), I(1/..). Only applicable for numeric variables.
#' @param sequential
#'  If TRUE, all variables with missing data are imputed sequentially across iterations.
#' @param nseq
#'  Maximum number of iterations (if sequential is TRUE).
#' @param eps
#'  Convergence threshold: the imputation process stops early if predictions change less than this amount across iterations.
#' @param imp_var
#'  If TRUE, additional columns indicating imputed values (VAR_imp) are added.
#' @param pred_history 
#'  If TRUE, all predicted values across all iterations are stored.
#' @param tune
#'  Hyperparameter tuning flag. Can be:
#'    - TRUE/FALSE globally  
#'    - or a list specifying tuning per variable, e.g. list(var1 = TRUE)
#'  Tuning is performed halfway through nseq iterations.
#' @param verbose
#'  If TRUE additional debugging output is provided
#' @return
#'  Either:
#'    - the imputed dataset (default), or  
#'    - a list containing the imputed dataset and prediction history, depending on the pred_history and tune settings.
#' @export
#'
#' @family imputation methods
#' @examples
#' \dontrun{
#' x <- vimpute(data = sleep, sequential = FALSE)
#' y <- vimpute(data = sleep, sequential = TRUE, nseq = 3)
#' z <- vimpute(data = sleep, considered_variables =
#'        c("Sleep", "Dream", "Span", "BodyWgt"), sequential = FALSE)
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
    learner_params = NULL,
    formula = FALSE, 
    sequential = TRUE,
    nseq = 10,
    eps = 0.005, 
    imp_var = TRUE,
    pred_history = FALSE,
    tune = FALSE,
    verbose = FALSE
) {

  ..cols <- ..feature_cols <- ..reg_features <- ..relevant_features <- NULL
  # save plan
  old_plan <- future::plan()  # Save current plan
  on.exit(future::plan(old_plan), add = TRUE)  # Restore on exit, even if error

  # dots <- list(...)

  # only defined variables
  data_all_variables <- as.data.table(data)
  data <-  as.data.table(data)[, considered_variables, with = FALSE]
  
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
  
### ***** Check Data Start ***** ###################################################################################################
  if(verbose){
    message(paste("***** Check Data"))  
  }
  
  
  method_vector <- unlist(method, use.names = FALSE)
  
  default_method <- NULL
  if (length(method_vector) > 0 && length(unique(method_vector)) == 1L) {
    default_method <- unique(method_vector)
  }
  
  checked_data   <- precheck(data, pmm, formula, method, sequential, pmm_k, learner_params, tune, default_method)
  data           <- checked_data$data
  variables      <- checked_data$variables
  variables_NA   <- checked_data$variables_NA
  method         <- checked_data$method
  learner_params <- checked_data$learner_params
  pmm            <- checked_data$pmm
  pmm_k          <- checked_data$pmm_k
  tune           <- checked_data$tune
  
  if (!sequential && nseq > 1) {
    if (verbose) message ("'nseq' was set to 1 because 'sequential = FALSE'.")
    nseq <- 1
  }
### Check Data End ###
  
### ***** Learner START ***** ################################################################################################### 
  
  # Possible extension
  # LightGBM via mlr3extralearners:
  # classif.lightgbm, regr.lightgbm
  
  no_change_counter <- 0
  robust_required <- any(unlist(method) == "robust")
  
  if (robust_required) {
    register_robust_learners()
  }
  
  learner_ids <- c(
    "regr.cv_glmnet", "regr.glmnet", "classif.glmnet",
    "regr.ranger", "classif.ranger",
    "regr.xgboost", "classif.xgboost"
  )
  
  if (robust_required) {
    learner_ids <- c(learner_ids, "regr.lm_rob", "classif.glm_rob")
  }
  
  learners <- lapply(learner_ids, function(id) lrn(id))
  names(learners) <- learner_ids
### Learner End ###
  
### ***** Def missing indices Start ***** ###################################################################################################
  if(verbose){
    message(paste("***** Find Missing Indices"))
  }
  missing_indices <- setNames(lapply(variables, function(var) { #original NAs
    na_idx <- which(is.na(data[[var]]))
    if (length(na_idx) > 0) return(na_idx) else return(integer(0))
  }), variables)
  missing_indices <- missing_indices[!sapply(missing_indices, is.null)]
  ### Def missing indices End ###
  
  po_ohe <- NULL # set ohe to zero, becomes true if ohe is needed
  data_new <- copy(data)
  original_data <- copy(data)  # Saves original structure of data
  
  if (pred_history == TRUE) {
    history <- list() # save history of predicted values
  }
  
  count_tuned_better <- 0
  count_default_better <- 0
  
  hyperparameter_cache <- setNames(vector("list", length(variables_NA)), variables_NA)
  tuning_status <- setNames(rep(FALSE, length(variables_NA)), variables_NA)
  
  tuning_log <- list()
  
  # Iterative Imputation for nseq iterations
  for (i in seq_len(nseq)) {
    if(verbose){
      message(paste("ITERATION", i, "von", nseq))
    }
    iteration_times <- list()
    data_prev <- copy(data)
    
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
        data <- enforce_factor_levels(data, factor_levels)  
        data_clean <- na.omit(data)
        
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
        
        clean_colnames <- function(names) {
          names <- make.names(names, unique = TRUE)
          patterns <- c("\\(", "\\)", ":", "\\*", "\\^", "%in%", "/", "-", "\\+", " ")
          replacements <- c("", "", "_int_", "_cross_", "_pow_", "_nest_", "_sub_", "_minus_", "_plus_", "")
          for (i in seq_along(patterns)) {
            names <- gsub(patterns[i], replacements[i], names)
          }
          
          return(names)
        }
        setnames(data_temp, clean_colnames(names(data_temp)))
        data_temp <- enforce_factor_levels(data_temp, factor_levels)  
        check_all_factor_levels(data_temp, factor_levels)
        
        # Impute missing values (Median/Mode)  -> for prediction 
        if (is_target_numeric) {
          task_mm <- TaskRegr$new(id = "imputation_task_mm", backend = data, target = target_col)
        } else {
          task_mm <- TaskClassif$new(id = "imputation_task_mm", backend = data, target = target_col)
        }
        
        pipeline_impute <- po("imputehist") %>>%  # Histogram-based imputation for numeric variables (Median)
          po("imputemode") %>>%                  # Mode imputation for categorical variables
          po("modelmatrix", formula = rewrited_formula) #rewrited_formula  # Create design matrix
        
        pipeline_impute$train(task_mm)
        po_task_mm <- pipeline_impute$predict(task_mm)[[1]]
        mm_data <- po_task_mm$data() # mm_data = transformed data with missings filled in, data_temp = transformed data without missings
        mm_data <- as.data.table(mm_data)
        setnames(mm_data, clean_colnames(names(mm_data)))
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
      
      if (!isFALSE(selected_formula) && "Intercept" %in% colnames(data_temp)) {
        data_temp <- data_temp[, !colnames(data_temp) %in% "Intercept", with = FALSE]
        if (exists("mm_data", inherits = FALSE)) {
          mm_data <- mm_data[, !colnames(mm_data) %in% "Intercept", with = FALSE]
        }
      }
      
      if (!isFALSE(selected_formula)) {
        if (!method[[var]] %in% c("robust", "regularized")) {
          stop("Error: A formula can only be used with the 'robust' or 'regularized' methods.")
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
          xgboost = list(learners[["regr.xgboost"]])
        )
      } else if (is.factor(data[[target_col]])) {
        learners_list <- list(
          regularized = list(learners[["classif.glmnet"]]),
          robust = list(learners[["classif.glm_rob"]]),
          ranger = list(learners[["classif.ranger"]]),
          xgboost = list(learners[["classif.xgboost"]])
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
          train_task <- as_task_regr(data_temp, target = target_col)  
        } else {
          train_task <- as_task_classif(data_temp, target = target_col)  
        }
        
        po_ohe$train(list(train_task))  # Train Encoder
        
        # Apply the encoding to the training data
        data_temp <- po_ohe$predict(list(train_task))[[1]]$data()
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
      
      # If NA in target variable --> only train with the data that has no NA in Y
      data_y_fill <- data_y_fill[!is.na(get(target_col))]
      
      # If the learner does not support missing values -> use na.omit()
      data_y_fill_final <- if (supports_missing) data_y_fill else na.omit(data_y_fill)
      data_y_fill_final <- enforce_factor_levels(data_y_fill_final, factor_levels) 
      
      
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
      max_threads <- future::availableCores() -1
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
        valid_xgb_params <- learners[["regr.xgboost"]]$param_set$ids()
        
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
        valid_ranger_params <- learners[["regr.ranger"]]$param_set$ids()
        
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
      
      is_regr_task <- is.numeric(data_y_fill_final[[target_col]])
      measure <- if (is_regr_task) msr("regr.rmse") else msr("classif.acc")

      if (length(learner_candidates) > 1) {
        resample_results <- lapply(learner_candidates, function(lrn) {
          
          if (grepl("xgboost", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, xgboost_params)
          } else if (grepl("ranger", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, ranger_params)
          } else if (grepl("glmnet", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, glmnet_params)
          } else if (grepl("lm_rob|glm_rob", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, robust_params)
          }
          
          # Klassiication: probabilistic
          if (!is_regr_task) {
            if ("prob" %in% lrn$predict_types) {
              lrn$predict_type <- "prob"
            } else {
              lrn$predict_type <- "response"
            }
          }
          resample(task, lrn, rsmp("cv", folds = 5))
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
      
      # Per Variable tune once in the middle of the seq
      if (!tuning_status[[var]] && isTRUE(tune[[var]]) && i == round(nseq / 2)) {
        
        # ------------------------------------------------------------
        # Adaptive Search Space Builder (small vs large dataset)
        build_search_space <- function(best_learner_id, task) {
          n <- task$nrow
          size <- if (n <= 5000) "small" else if (n <= 100000) "medium" else "large"
          
          # GLMNET
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
          
          # RANGER
          if (best_learner_id %in% c("regr.ranger", "classif.ranger")) {
            space <- ps(
              num.trees       = p_int(if (size == "large") 200L else 300L, 
                                      if (size == "small") 900L else 600L),
              min.node.size   = p_int(if (size == "small") 3L else 10L,   
                                      if (size == "small") 10L else 50L),
              sample.fraction = p_dbl(if (size == "large") 0.6  else 0.8, 1.0)
              # Optional: mtry relative to p
              # mtry = p_int(max(1L, floor(sqrt(length(task$feature_names))*0.8)),
              #              max(1L, floor(sqrt(length(task$feature_names))*1.2)))
            )
            return(list(space = space, n_evals = if (size == "small") 15 else if (size == "medium") 12 else 10))
          }
          
          # XGBOOST
          if (best_learner_id %in% c("regr.xgboost", "classif.xgboost")) {
            space <- ps(
              nrounds          = p_int(100L, if (size == "small") 500L else 300L),
              eta              = p_dbl(0.05, 0.3, logscale = TRUE),
              max_depth        = p_int(2L,   if (size == "small") 8L    else 6L),
              subsample        = p_dbl(0.5,  1.0),
              colsample_bytree = p_dbl(0.5,  1.0)
            )
            return(list(space = space, n_evals = if (size == "small") 20 else if (size == "medium") 15 else 12))
          }
          
          # ROBUST
          if (best_learner_id %in% c("regr.lm_rob", "classif.glm_rob")) {
            space <- ps(
              tuning.chi = p_dbl(1.2, 1.5),
              tuning.psi = p_dbl(1.2, 1.5),
              max.it     = p_int(50L, 200L)
            )
            return(list(space = space, n_evals = 8))
          }
          
          # Fallback
          list(space = NULL, n_evals = 10)
        }
        # ------------------------------------------------------------
        
        best_learner_id <- best_learner$id
        ss <- build_search_space(best_learner_id, task)
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
                folds <- if (task$nrow <= 3000) 5L else 3L
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
                batch <- max(1, parallel::detectCores() - 1)
                tuner <- tnr("random_search", batch_size = batch)
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
              folds <- if (task$nrow <= 3000) 5L else 3L
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
              
              batch <- max(1, parallel::detectCores() - 1)
              tuner <- tnr("random_search", batch_size = batch)
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
        
        future::plan("sequential")
        
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
        tuned_better = tuned_flag
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
        method_var <- "robust"
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
        class_data <- data_temp[!is.na(data_temp[[var]]), c(relevant_features, var, zero_flag_col), with = FALSE] # not using NAs for training
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
          na_rows <- !complete.cases(class_data[, ..relevant_features])
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
                     affect_columns = selector_type("factor"),
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
        reg_data <- data_temp[data_temp[[var]] > 0,]  # only positive values
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
            affect_columns = selector_name(reg_features),
            which = "all",
            type = "factor"
          ))
        }
        
        if (has_na_in_features && !supports_missing) {
          cols <- c(reg_features, var)
          reg_data <- na.omit(reg_data[, ..cols])
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
        
      }
### Train Model End ###
      
### *****Identify NAs Start***** ###################################################################################################
      if(verbose){
        message("***** Identify missing values *****")
      }
      
      # Impute missing values
      impute_missing_values <- function(data, ref_data) {
        for (col in colnames(data)) {
          if (any(is.na(data[[col]]))) {
            if (is.numeric(ref_data[[col]])) {
              data[[col]][is.na(data[[col]])] <- median(ref_data[[col]], na.rm = TRUE)
            } else if (is.factor(ref_data[[col]])) {
              mode_value <- names(which.max(table(ref_data[[col]], useNA = "no")))
              data[[col]][is.na(data[[col]])] <- mode_value
            }
          }
        }
        return(data)
      }
      
      # Imputeoor for Ranger (Out-of-Range-Level)
      imputeoor <- function(data, ref_data) {
        for (col in colnames(data)) {
          if (is.factor(data[[col]])) {
            known_levels <- levels(ref_data[[col]])
            unknown_idx <- !data[[col]] %in% known_levels
            if (any(unknown_idx, na.rm = TRUE)) {
              # All unknown levels to NA 
              data[[col]][unknown_idx] <- NA
            }
          }
        }
        return(data)
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
            backend_data <- imputeoor(backend_data, data_temp)
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
            backend_data <- imputeoor(backend_data, data_temp)
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
            class_pred_data <- imputeoor(class_pred_data, data_temp)
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
          reg_pred_data <- imputeoor(reg_pred_data, data_temp) #Nnew
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
      
### *****Select suitable task type Start***** ###################################################################################################
      
      if (!is_sc) {
        
        if (is.numeric(data_temp[[target_col]])) {
          pred_task <- TaskRegr$new(
            id = target_col,
            backend = backend_data,
            target = target_col
          )
        } else if (is.factor(data_temp[[target_col]])) {
          pred_task <- TaskClassif$new(
            id = target_col,
            backend = backend_data,
            target = target_col
          )
        } else {
          stop("Error: Target variable is neither numeric nor a factor!")
        }
      }
      
### Select suitable task type End ####
      
### *****Predict Start***** ###################################################################################################
      if(verbose){
        message("***** Predict")
      }
      
      if (is_sc) {

        zero_flag_col <- paste0(var, "_zero_flag")
        zero_prob_col <- paste0(var, "_zero_prob")     # numeric Prob-column
        relevant_features <- setdiff(names(data_temp), c(var, zero_flag_col, zero_prob_col))
        
        # 1) classification (null vs positive) P(Y > 0 | X)
        class_learner <- learner$classifier
        
        # Safety: classifier -> probabilities 
        if ("prob" %in% class_learner$predict_types) {
          class_learner$predict_type <- "prob"
        } else {
          class_learner$predict_type <- "response"  # Fallback
          warning(sprintf("predict_type 'prob' not supported by learner '%s'; fallback to 'response'", class_learner$id))
        }
        
        class_pred_data <- data_temp[missing_idx, feature_cols, with = FALSE]

        # Factor Level Handling
        class_pred_data <- enforce_factor_levels(class_pred_data, factor_levels)
        check_all_factor_levels(class_pred_data, factor_levels)
        
        supports_missing_cls <- "missings" %in% class_learner$properties
        # If no missings accepted -> NAs in Newdata 
        
        if (anyNA(class_pred_data)) {
          class_pred_data <- impute_missing_values(class_pred_data, data_temp)
        }
        
        factor_cols <- names(class_pred_data)[sapply(class_pred_data, is.factor)]

        # ensure reserve level exists in prediction data ###
        reserve_level <- ".__IMPUTEOOR_NEW__"
        for (col in factor_cols) {
          train_levels <- factor_levels[[col]]
          if (!(reserve_level %in% train_levels)) train_levels <- c(train_levels, reserve_level)
          # All unknown levels -> reserve_level
          class_pred_data[[col]][!class_pred_data[[col]] %in% train_levels] <- reserve_level
          # Set factor levels 
          class_pred_data[[col]] <- factor(class_pred_data[[col]], levels = train_levels)
        }
        
        # Prediction
        pred_probs <- class_learner$predict_newdata(class_pred_data)$prob
        # pred_probs[, "positive"] = P(Y>0 | X)
        pi_hat <- if (!is.null(pred_probs) && "positive" %in% colnames(pred_probs)) {
          pred_probs[, "positive"]
        } else {
          # Fallback 'response'
          as.numeric(class_learner$predict_newdata(class_pred_data)$response == "positive")
        }
        
        
        # Save probability in the main dataset (stable row count)
        if (!zero_prob_col %in% names(data)) {
          data[[zero_prob_col]] <- NA_real_
        }
        data[[zero_prob_col]][missing_idx] <- pi_hat
        
        # 2) regression: for positive predictions E[Y | Y > 0, X]
        reg_learner <- learner$regressor
        reg_pred_data <- data_temp[missing_idx, feature_cols, with = FALSE]
        
        # Factorlevels
        reg_pred_data <- enforce_factor_levels(reg_pred_data, factor_levels)
        check_all_factor_levels(reg_pred_data, factor_levels)
        
        # Fill NA in feature
        if (anyNA(reg_pred_data)) {
          reg_pred_data <- impute_missing_values(reg_pred_data, data_temp[missing_idx])
        }
        
        preds_reg <- NULL
        if (method_var == "ranger" && isTRUE(ranger_median) && !is.null(reg_learner)) {
          preds_reg <- predict_ranger_median(reg_learner, reg_pred_data, target_name = NULL)
        }
        if (is.null(preds_reg)) {
          preds_reg <- reg_learner$predict_newdata(reg_pred_data)$response  # E[Y | Y>0, X]
        }
        
        # Combine results, Impuatation = deterministic
        # Y_imputed = P(Y > 0 | X) * E[Y | Y > 0, X]
        preds <- pi_hat * preds_reg
        # data_temp[[var]][missing_idx] <- preds
        
      } else {
        # Not semicontinuous
        bdt <- as.data.table(backend_data)
        bdt <- enforce_factor_levels(bdt, factor_levels)
        check_all_factor_levels(bdt, factor_levels)
        
        if (method_var == "ranger") { 
          bdt <- imputeoor(bdt, data_temp) 
        }
        
        if (anyNA(bdt)) {
          bdt <- impute_missing_values(bdt, data_temp)
        }
        
        backend_data <- mlr3::as_data_backend(bdt)
        
        if (is.factor(data_temp[[target_col]])) {
          pred_task <- TaskClassif$new(
            id = target_col,
            backend = backend_data,
            target = target_col
          )
          
          mod <- switch(method_var,
                        ranger = "classif.ranger",
                        xgboost = "classif.xgboost",
                        regularized = "classif.glmnet",
                        robust = "classif.glm_rob",
                        stop("Unknown method for classification:", method_var))
          
          # learner$model[[mod]]$param_set$values$predict_type <- "prob"
          # pred_probs <- learner$predict(pred_task)$prob
          
          learner$predict_type <- "prob"
          pred_probs <- learner$predict(pred_task)$prob
          
          if (isFALSE(sequential) || i == nseq) {
            preds <- apply(pred_probs, 1, function(probs) {
              sample(levels(data_temp[[target_col]]), size = 1, prob = probs)
            })
          } else {
            preds <- apply(pred_probs, 1, which.max)
            preds <- levels(data_temp[[target_col]])[preds]
          }
          
        } else {
          pred_task <- TaskRegr$new(
            id = target_col,
            backend = backend_data,
            target = target_col
          )
          preds <- NULL
          if (method_var == "ranger" && isTRUE(ranger_median)) {
            preds <- predict_ranger_median(learner, bdt, target_name = target_col)
          }
          if (is.null(preds)) {
            preds <- learner$predict(pred_task)$response
          }
        }
      }
      
      if (!is.null(lhs_transformation)) {
        preds <- inverse_transform(preds, lhs_transformation)
      }
      
      if (inherits(preds, "numeric")) {
        decimal_places <- max(sapply(na.omit(data[[var]]), get_decimal_places), na.rm = TRUE)
        preds <- round(preds, decimal_places)
      }
      
      # Store model score used for prediction for pmm (numeric targets only)
      if (inherits(preds, "numeric")) {
        miss_idx <- missing_indices[[var]]
        obs_idx <- setdiff(seq_len(nrow(data)), miss_idx)
        
        # Store score for PMM
        score_current <- numeric(nrow(data))
        score_current[obs_idx] <- data[[var]][obs_idx]       # observed values
        score_current[miss_idx] <- preds        # predictions for missing rows
      }
### Predict End ###################################################################################################
      
### ***** PMM / Score-kNN Start ***** ###################################################################################################
      if(verbose){
        message("***** PMM / Score-kNN for predictions")
      }
      
      if (pmm[[var]] && is.numeric(data_temp[[var]]) && length(miss_idx) > 0) {
        
        # Observed values
        y_obs <- data[[var]][obs_idx]
        
        if (pmm_k[[var]] == 1 && is.numeric(data_temp[[var]])) {
          # Standard PMM (1D at Y) only for numeric variables
          preds<- sapply(preds, function(x) {
            idx <- which.min(abs(y_obs - x))
            y_obs[idx]
          })
          
        } else if (pmm_k[[var]] > 1 && is.numeric(data_temp[[var]])) {
          # Score-based kNN PMM (1D score, numeric targets only)
          score_obs  <- score_current[obs_idx]
          score_miss <- score_current[miss_idx]
          k <- min(pmm_k[[var]], length(y_obs))
          
          preds <- sapply(score_miss, function(s) {
            idx <- order(abs(score_obs - s))[1:k]  # find k nearest neighbors
            mean(y_obs[idx])                       # smooth
            # stochastic alternative: sample(y_obs[idx], 1)
          })
        }
        
        # Round only final imputation
        decimal_places <- max(sapply(na.omit(data[[var]]), get_decimal_places), na.rm = TRUE)
        preds <- round(preds, decimal_places)
        #preds <- preds[miss_idx]
      }
### PMM / Score-kNN End ###
      
      
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
      # preds <- preds[miss_idx]
      if (length(missing_idx) > 0) {
        
        # orginal numeric
        is_target_numeric <- is.numeric(original_data[[var]])
        
        if (is_target_numeric) {
          # if factor because of semicontinous -> numeric
          if (is.factor(data[[var]])) {
            data[, (var) := as.numeric(as.character(get(var)))]
          }
          if (exists("data_new") && is.factor(data_new[[var]])) {
            data_new[, (var) := as.numeric(as.character(get(var)))]
          }
          
          # numeric imputation
          data[missing_idx, (var) := as.numeric(preds)]
          
        } else if (is.factor(original_data[[var]])) {
          
          # factor imputation
          data[missing_idx, (var) := factor(preds, levels = factor_levels[[var]])]
          
        } else {
          stop(paste("Unknown data type for variable:", var))
        }
        
        data <- copy(data)
      }
      
### Replace missing values with predicted values End ###
      
### *****Import Variable Start***** ###################################################################################################
      if(verbose){
        message(paste("***** Import Variable (imp_var = TRUE)"))
      }
      if (length(missing_idx) > 0) {
        if (imp_var) {
          imp_col <- paste0(var, "_imp")    # Name  _imp-Spalte
          
          # Check whether the variable contains missing values using `missing_idx`.
          if (!is.null(missing_idx) && length(missing_idx) > 0) {
            # If `_imp` column does not exist, create it as a boolean variable
            if (!(imp_col %in% colnames(data_new))) {
              data_new[, (imp_col) := FALSE]
            }
            
            data_new[, (var) := data[[var]]]
            
            # Ensure that `preds` is not NULL or empty
            if (length((preds)) == length(missing_idx) && !all(is.na((preds)))) {
              # Set the imputation as TRUE for missing values
              set(data_new, i = missing_idx, j = imp_col, value = TRUE)
            } else {
              warning(paste("Warning: `preds` is empty or does not have the same length as `missing_idx` for ", var))
            }
          }
        }
      }
      var_end_time <- Sys.time()
      var_time <- difftime(var_end_time, var_start_time, units = "secs")
      iteration_times[[var]] <- round(as.numeric(var_time), 2)
      if(verbose){
        message(paste("time used for", var, ":", iteration_times[[var]], "Sekunden"))
      }
    }
### Import Variable END ###
    
### *****Stop Criteria Start***** ###################################################################################################
    if (sequential && i != 1) {
      is_dt <- inherits(data, "data.table")
      
      # Automatic detection of the variable types
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      factor_cols <- names(data)[sapply(data, is.factor)]
      
      # Calculation of numerical differences
      if (length(numeric_cols) > 0) {  
        epsilon <- 1e-8  # Avoid division by zero  
        
        if (is_dt) {  
          # Calculate standard deviation of each column in data_prev  
          std_dev <- sapply(data_prev[, mget(numeric_cols)], sd, na.rm = TRUE)  
          
          # Calculate normalized relative change  
          num_diff <- sum(abs(data[, mget(numeric_cols)] - data_prev[, mget(numeric_cols)]) /  
                            (std_dev + epsilon), na.rm = TRUE)
        } else {  
          std_dev <- sapply(data_prev[, numeric_cols, drop = FALSE], sd, na.rm = TRUE)  
          num_diff <- sum(abs(data[, numeric_cols, drop = FALSE] - data_prev[, numeric_cols, drop = FALSE]) /  
                            (std_dev + epsilon), na.rm = TRUE)
        }  
      } else {  
        num_diff <- 0  
      }
      
      
      if (length(factor_cols) > 0) {
        
        # comparison as character
        data_fac_chr      <- data[,       lapply(.SD, as.character), .SDcols = factor_cols]
        data_prev_fac_chr <- data_prev[,  lapply(.SD, as.character), .SDcols = factor_cols]
        
        # Differences
        cat_changes <- sum(data_fac_chr != data_prev_fac_chr, na.rm = TRUE)
        total_cat_values <- sum(!is.na(data_prev_fac_chr))
        
        cat_diff <- if (total_cat_values > 0) {
          cat_changes / (total_cat_values + 1e-8)
        } else {
          0
        }
        
      } else {
        cat_diff <- 0  # If there are no categorical columns
      }
      
      # Calculate total change
      total_diff <- num_diff + cat_diff
      
      # Prove convergence
      if (total_diff < eps) {
        no_change_counter <- no_change_counter + 1
        if (no_change_counter >= 2) {
          if(verbose){
            message("Convergence achieved after two consecutive iterations without changes")
            print(paste("stop after", i, "iterations"))
          }
          
          if (is.factor(data_temp[[target_col]])) {
            
            if (method_var == "ranger") {
              mod <- "classif.ranger"
            }
            
            if (method_var == "xgboost") {
              mod <- "classif.xgboost"
            }
            
            if (method_var == "regularized") {
              mod <- "classif.glmnet"
            }
            
            if (method_var == "robust") {
              mod <- "classif.glm_rob"
            }
            
            learner$model[[mod]]$param_set$values$predict_type <- "prob"
            pred_probs <- learner$predict(pred_task)$prob
            
            formatted_output <- capture.output(print(pred_probs))
            
            if (is.null(pred_probs)) {
              stop("Error in the calculation of prediction probabilities.")
            }
            
            preds <- apply(pred_probs, 1, function(probs) {
              sample(levels(data_temp[[target_col]]), size = 1, prob = probs) # at last iteration: stochastic class assignment
            })
          }
          
          break
        }
      } else {
        no_change_counter <- 0
      }
    } else {
      no_change_counter <- 0
    }
  }
### Stop criteria END ###
  
  result <- as.data.table(if (imp_var) data_new else data)  # Default: Return `data` only
  result <- enforce_factor_levels(result, factor_levels)
  
  
  any_tuned <- any(unlist(tune))
  
  if (!pred_history && !any_tuned) {
    return(result)
  }
  
  
  output <- list(data = result)   
  
  if (pred_history) {
    pred_result <- rbindlist(history, fill = TRUE)
    output$pred_history <- pred_result
  }
  if (any_tuned) {
    output$tuning_log <- tuning_log
    
  } 
  return(output)    
}
