
#' Impute missing values with prefered Model, sequentially, with hyperparametertuning and with PMM (if wanted)
#' Need of 'helper_vimpute' script

## PARAMETERS ##
#' @param data - Dataset with missing values. Can be provided as a data.table or data.frame.
#' @param considered_variables - A character vector of variable names to be either imputed or used as predictors, excluding irrelevant columns from the imputation process.
#' @param method - A named list specifying the imputation method for each variable:
# - ranger
# - xgboost 
# - regularized
# - robust
#' @param pmm - TRUE/FALSE indicating whether predictive mean matching is used. Provide as a list for each variable.
#' @param formula - If not all variables are used as predictors, or if transformations or interactions are required (applies to all X, for Y only transformations are possible). Only applicable for the methods "robust" and "regularized". Provide as a list for each variable that requires specific conditions.
#   - formula format:                     list(variable_1 ~ age + lenght, variable_2 ~ width + country) 
#   - formula format with transformation: list(log(variable_1) ~ age + inverse(lenght), variable_2 ~ width + country)
#   - For X: follows the rules of model.matrix
#   - For Y: transformations supported are log(), exp(), sqrt(), I(1/..). Only applicable for numeric variables.
#' @param sequential - If TRUE, all variables are imputed sequentially.
#' @param nseq - Maximum number of iterations (if sequential is TRUE).
#' @param eps - Threshold for convergence.
#' @param imp_var - If TRUE, the imputed values are stored.
#' @param pred_history - If TRUE, all predicted values across all iterations are stored.
#' @param tune - Tunes hyperparameters halfway through iterations, TRUE or FALSE.
#' @param verbose - If TRUE additional debugging output is provided
#' @return imputed data set or c(imputed data set, prediction history)
#' @export
#'
#' @examples
#' x <- vimpute(data = sleep, sequential = FALSE)
#' y <- vimpute(data = sleep, sequential = TRUE, nseq = 3)
#' z <- vimpute(data = sleep, considered_variables =
#'        c("Sleep", "Dream", "Span", "BodyWgt"), sequential = FALSE)
#########################################################################################
#########################################################################################
#########################################################################################

vimpute <- function(
    data,
    considered_variables = names(data), 
    method = setNames(as.list(rep("ranger", length(considered_variables))), considered_variables),
    pmm = setNames(as.list(rep(TRUE, length(considered_variables))), considered_variables),
    formula = FALSE, 
    sequential = TRUE,
    nseq = 10,
    eps = 0.005, 
    imp_var = TRUE,
    pred_history = FALSE,
    tune = FALSE,
    verbose = FALSE
) {

  # save plan
  old_plan <- future::plan()  # Save current plan
  on.exit(future::plan(old_plan), add = TRUE)  # Restore on exit, even if error
    
### ***** Learner START ***** ################################################################################################### 
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
    
    # only defined variables
    data_all_variables <- as.data.table(data)
    data <-  as.data.table(data)[, considered_variables, with = FALSE]
    
### ***** Check Data Start ***** ###################################################################################################
    if(verbose){
      message(paste("***** Check Data"))  
    }
    checked_data <- precheck(data, pmm, formula, method, sequential)
    data         <- checked_data$data
    variables    <- checked_data$variables
    variables_NA <- checked_data$variables_NA
    method       <- checked_data$method

    if (!sequential && nseq > 1) {
        if (verbose) message ("'nseq' was set to 1 because 'sequential = FALSE'.")
        nseq <- 1
    }
    
    orig_data <- data
### Check Data End ###
    
### ***** Def missing indices Start ***** ###################################################################################################
    if(verbose){
      message(paste("***** Find Missing Indices"))
    }
    missing_indices <- setNames(lapply(variables, function(var) { #original NAs
      na_idx <- which(is.na(data[[var]]))
      if (length(na_idx) > 0) return(na_idx) else return(integer(0))
    }), variables)
    missing_indices <- missing_indices[!sapply(missing_indices, is.null)]
    names(missing_indices)
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
        
        data_before <- copy(data)
        variables    <- checked_data$variables
        if(verbose){
          message(paste("***** Select predictors"))
        }
        if (!isFALSE(formula)) {
          selected_formula <- select_formula(formula, var)  # formula on left handsite
          if (verbose) {
            message(paste("Selected formula for variable", var, ":", selected_formula))
          }
        }
        
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
          is_target_numeric <- is.numeric(data[[target_col]])
          if (is_target_numeric) {
            task_mm_na_omit <- TaskRegr$new(id = "imputation_task_na_omit", backend = na.omit(data), target = target_col)
          } else {
            task_mm_na_omit <- TaskClassif$new(id = "imputation_task_na_omit", backend = na.omit(data), target = target_col)
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
          feature_cols <- setdiff(variables, var)  
          target_col <- var
          selected_cols <- c(target_col, feature_cols)
          data <- data[, selected_cols, with = FALSE]
          data_temp <- as.data.table(data)
        }
        
        if ("Intercept" %in% colnames(data_temp)) {
          data_temp <- data_temp[, !colnames(data_temp) %in% "Intercept", with = FALSE]
          mm_data <- mm_data[, !colnames(mm_data) %in% "Intercept", with = FALSE]
        }
        
        if (!isFALSE(selected_formula)) {
          if (!method[[var]] %in% c("robust", "regularized")) {
            stop("Error: A formula can only be used with the 'robust' or 'regularized' methods.")
          }
        }
        
        method_var <- method[[var]]
        
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
          if (method_var == "robust" && length(levels(data[[target_col]])) != 2) {
            warning(paste0("Variable not binary", target_col, "': use alternative method than glm.rob"))
          }
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
          #print("One-Hot-Encoding notwendig")
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
        data_y_fill <- copy(data_temp)
        supports_missing <- all(sapply(learner_candidates, function(lrn) "missings" %in% lrn$properties))
        
        if (method_var == "ranger") {
          supports_missing <- FALSE
        }
        
        # If NA in target variable --> only train with the data that has no NA in Y
        data_y_fill <- data_y_fill[!is.na(get(target_col))]
        
        # If the learner does not support missing values -> use na.omit()
        data_y_fill_final <- if (supports_missing) data_y_fill else na.omit(data_y_fill)
        
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
        # XGBoost Parameter
        xgboost_params <- list(
          nrounds = 100,
          max_depth = 3,
          eta = 0.1,
          min_child_weight = 1,
          subsample = 1,
          colsample_bytree = 1,
          #tree_method = "hist", 
          #early_stopping_rounds = 10,
          verbose = 1,
          nthread = optimal_threads
        )
        if(verbose){
          print(paste("nthread is set to:", optimal_threads))
        }
        # Ranger Parameter 
        ranger_params <- list(
          num.trees = 500,
          num.threads = 4
        )
        
        if (length(learner_candidates) > 1) {
          resample_results <- lapply(learner_candidates, function(lrn) {
            
            if (grepl("xgboost", lrn$id)) {
              lrn$param_set$values <- modifyList(lrn$param_set$values, xgboost_params)
            } else if (grepl("ranger", lrn$id)) {
              lrn$param_set$values <- modifyList(lrn$param_set$values, ranger_params)
            }
            resample(task, lrn, rsmp("cv", folds = 5))
          })
          
          mse_values <- sapply(resample_results, function(res) res$aggregate(msr("regr.rmse")))
          best_learner <- learner_candidates[[which.min(mse_values)]]
          
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
        }
        
        if (is.factor(data_temp[[target_col]])) {
          best_learner$predict_type <- "prob"
          default_learner$predict_type <- "prob"
          current_learner$predict_type <- "prob"
          tuned_learner$predict_type <- "prob"
        }
        
### Create Learner End ### 
        
### *****Hyperparameter Start***** ###################################################################################################
        if(verbose){
          message(paste("***** Parametertuning"))
        }
        #print(paste("i:", i))
        
        
        if (!tuning_status[[var]] && nseq >= 2 && tune) {
          
          if ((nseq > 2 && i == round(nseq / 2)) || (nseq == 2 && i == 2)) {
            #print("Starte Hyperparameter-Tuning")
            tuner = tnr("random_search")
            p = length(task$feature_names)
            
            search_spaces <- list(
              "regr.cv_glmnet" = ps(alpha = p_dbl(0, 1), nfolds = p_int(7, 20)),  
              "regr.glmnet" = ps(alpha = p_dbl(0, 1), lambda = p_dbl(10^-4, 10^2, logscale = TRUE)),
              "classif.glmnet" = ps(alpha = p_dbl(0, 1), lambda = p_dbl(10^-4, 10^2, logscale = TRUE)),
              # alpha = mixture between lasso (1) and ridge (0)
              # lambda = the larger the stronger the regulation
              
              # Ranger
              "regr.ranger" = ps(num.trees = p_int(500, 700 ,default = 500), min.node.size = p_int(3, 10, default=5), sample.fraction = p_dbl(0.8,1)),
              "classif.ranger" = ps(num.trees = p_int(500, 700), min.node.size = p_int(3, 10), sample.fraction = p_dbl(0.8,1)),
              # min.node.size = minimum node size 
              # sample.fraction = proportion of data sampled per tree
              
              # XGBoost
              "regr.xgboost" = ps(nrounds = p_int(100,500), eta = p_dbl(0.01, 0.3), max_depth = p_int(3, 9),  colsample_bytree = p_dbl(0.7, 0.9)),
              "classif.xgboost" = ps(nrounds = p_int(100,500), eta = p_dbl(0.01, 0.3), max_depth = p_int(3, 9), subsample = p_dbl(0.7, 0.9), colsample_bytree = p_dbl(0.7, 0.9)),
              # eta = learning rate, low values --> more stable but slower models
              # max_depth = Maximum tree depth, large values --> more complex patterns but possibly overfitting
              # subsample = proportion of data used per boosting iteration | colsample_bytree = proportion of features used per tree          
              
              # Robust Models
              "regr.lm_rob" = ps(tuning.chi = p_dbl(1.2,1.8), tuning.psi = p_dbl(1.2,1.8),  max.it = p_int(60,300)), #psi = p_fct(c("bisquare", "optimal")),
              "classif.glm_rob" = ps(method = p_fct(c("Mqle", "WBY")), acc = p_dbl(0, 0.1), test.acc = p_fct(c("coef", "resid")), tcc = p_dbl(1,2), maxit = p_int(30,300))
              # psi = function for weighting the residuals 
              # acc = tolerance for the convergence of the algorithm | test.acc = criterion for the convergence test | tcc = tuning constant
            )
            
            best_learner_id = best_learner$id  
            #best_learner = learners[[best_learner_id]]
            search_space = search_spaces[[best_learner_id]]
            
            #future::plan("multisession") 
            
            tryCatch({
              # train default model
              if (best_learner_id == "classif.xgboost" || best_learner_id == "regr.xgboost") {
                default_learner$param_set$values$nrounds = 100  # Set a default value for nrounds
              }
              
              # Tuning-Instance
              instance = TuningInstanceBatchSingleCrit$new(
                task = task,  
                learner = best_learner,
                resampling = rsmp("cv", folds = 5,repeats = 3),
                measure = if (task$task_type == "regr") msr("regr.rmse") else msr("classif.acc"),
                search_space = search_space,
                terminator = trm("evals", n_evals = 20)
              )
              
              # tuning
              tuner <- tnr("random_search", batch_size = parallel::detectCores() - 1)
              tuner$optimize(instance)
              
              # save best parameters
              best_params <- as.list(instance$result[, get("learner_param_vals")][[1]])
              tuning_status[[var]] <- TRUE
              
              # compare with default
              tuned_learner$param_set$values <- best_params
              
              default_result <- resample(task, default_learner, rsmp("cv", folds = 5))
              tuned_result <- resample(task, tuned_learner, rsmp("cv", folds = 5))
              
              
              # which model is better
              if (task$task_type == "regr") {
                if (tuned_result$aggregate(msr("regr.rmse")) < default_result$aggregate(msr("regr.rmse"))) {
                  current_learner$param_set$values <- best_params
                  count_tuned_better <- count_tuned_better + 1
                  
                  hyperparameter_cache[[var]] <- list(
                    params = best_params,
                    is_tuned = TRUE
                  )
                  if (verbose) {
                    message(sprintf("Tuned parameters for variable '%s': %s", var, paste(names(best_params), best_params, sep = "=", collapse = ", ")))
                  }

                } else {
                  current_learner$param_set$values <- default_learner$param_set$values
                  count_default_better <- count_default_better + 1
                  hyperparameter_cache[[var]] <- list(
                    params = default_learner$param_set$values,
                    is_tuned = FALSE
                  )
                  if (verbose) {
                    message(sprintf("Default parameters for variable '%s': %s", var, paste(names(default_learner$param_set$values), default_learner$param_set$values, sep = "=", collapse = ", ")))
                  }
                }
                
              } else {
                if (tuned_result$aggregate(msr("classif.acc")) > default_result$aggregate(msr("classif.acc"))) {
                  current_learner$param_set$values <- best_params
                  count_tuned_better <- count_tuned_better + 1
                  
                  hyperparameter_cache[[var]] <- list(
                    params = best_params,
                    is_tuned = TRUE
                  )
                  
                } else {
                  current_learner$param_set$values <- default_learner$param_set$values
                  count_default_better <- count_default_better + 1
                  hyperparameter_cache[[var]] <- list(
                    params = default_learner$param_set$values,
                    is_tuned = FALSE
                  )
                }
              }
              
            }, error = function(e) {
              warning(sprintf("Tuning failed for variable '%s': %s. Using default parameters.", var, e$message))
              current_learner$param_set$values <- default_learner$param_set$values
              tuning_status[[var]] <- FALSE
              hyperparameter_cache[[var]] <- list(
                params = default_learner$param_set$values,
                is_tuned = FALSE
              )
            })
            
            future::plan("sequential")
          } else if (tuning_status[[var]]) {
            # Use cached parameters
            current_learner$param_set$values <- hyperparameter_cache[[var]]$params
          }
        } else {
          # No tuning - use default parameters
          current_learner$param_set$values <- list()
        }
        
### Hyperparameter End ###
        
### ***** NAs Start***** ###################################################################################################
        if(verbose){
          message(paste("***** NAs in feature variables bearbeiten"))
        }
        if (method_var == "xgboost") {
          po_x_miss <- NULL  # No modification necessary as xgboost accepts missings as NA
          
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
        
        # Check semicontinous
        is_sc <- is_semicontinuous(data_temp[[var]])
        
        if (is_sc) {

          # 1) Classification: 0 or > 0
          zero_flag_col <- paste0(var, "_zero_flag")

          data_temp[[zero_flag_col]] <- factor(ifelse(data_temp[[var]] == 0, "zero", "positive"))
          relevant_features <- setdiff(names(data_temp), c(var, zero_flag_col))
          class_data <- data_temp[!is.na(data_temp[[var]]) & complete.cases(data_temp[, ..relevant_features]),]
          train_data <- class_data 
          
          feature_cols <- setdiff(names(train_data), c(var, zero_flag_col))
          
          # classification task
          class_task <- TaskClassif$new(
            id = paste0(zero_flag_col, "_task"),
            backend = train_data,
            target = zero_flag_col
          )
          class_task$select(setdiff(names(train_data), c(var, zero_flag_col)))
          
          
          # learner
          regr_learner_id <- best_learner$id
          classif_learner <- lrn("classif.log_reg")
          regr_learner <- lrn(regr_learner_id)
          if (grepl("xgboost", best_learner$id)) {
            regr_learner$param_set$values <- modifyList(regr_learner$param_set$values, xgboost_params)
          } else if (grepl("ranger", best_learner$id)) {
            regr_learner$param_set$values <- modifyList(regr_learner$param_set$values, ranger_params)
          }
          
          # support missings? 
          supports_missing_classif <- "missings" %in% classif_learner$properties
          if (method_var == "ranger") supports_missing_classif <- FALSE
          
          
          # if support missings
          po_x_miss_classif <- NULL
          if (sum(is.na(data_temp[[var]])) > 0 && supports_missing_classif && method_var != "xgboost") {
            po_x_miss_classif <- po("missind", param_vals = list(
              affect_columns = mlr3pipelines::selector_all(),
              which = "all",
              type = "factor"
            ))
          }
          
          class_pipeline <- if (!is.null(po_x_miss_classif)) po_x_miss_classif %>>% classif_learner else classif_learner
          class_learner <- GraphLearner$new(class_pipeline)
          
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
          
          # train classificationmodel
          class_learner$train(class_task)
          
          
          # 2) Regression
          reg_data <- data_temp[data_temp[[var]] > 0,]
          reg_features <- setdiff(names(reg_data), c(var, zero_flag_col))
          reg_data <- reg_data[!is.na(reg_data[[var]]),] #only without NA
          has_na_in_features <- anyNA(reg_data[, ..reg_features])
          
          # support missings?
          supports_missing <- "missings" %in% regr_learner$properties
          if (method_var == "ranger") supports_missing <- FALSE
          
          # Missings as Indikator-Variables
          po_x_miss_reg <- NULL
          if (has_na_in_features && supports_missing && method_var != "xgboost") {
            po_x_miss_reg <- po("missind", param_vals = list(
              affect_columns = mlr3pipelines::selector_all(),
              which = "all",
              type = "factor"
            ))
          }
          
          # Fallback: if learner canot handle missings
          if (has_na_in_features && !supports_missing) {
            cols <- c(reg_features, var)
            reg_data <- na.omit(reg_data[, ..cols])
          }
          
          # Task
          reg_task <- TaskRegr$new(id = var, backend = reg_data, target = var)
          reg_task$select(reg_features)
          
          # Pipeline
          reg_pipeline <- if (!is.null(po_x_miss_reg)) po_x_miss_reg %>>% regr_learner else regr_learner
          reg_learner <- GraphLearner$new(reg_pipeline)
          
          # Hyperparameter-Cache
          if (isTRUE(tuning_status[[var]]) && !is.null(tuning_status[[zero_flag_col]]) && isTRUE(tuning_status[[zero_flag_col]])) {
            if (!is.null(hyperparameter_cache[[var]]) && isTRUE(hyperparameter_cache[[var]]$is_tuned)) {
              params <- hyperparameter_cache[[var]]$params
              if (verbose) {
                cat(sprintf("Use optimized parameters from the cache for %s\n", var))
              }
              
              pipeline_valid <- intersect(names(params), reg_pipeline$param_set$ids())
              reg_pipeline$param_set$values <- modifyList(reg_pipeline$param_set$values, params[pipeline_valid])
              
              prefixed_names <- paste0(best_learner$id, ".", names(params))
              learner_valid <- prefixed_names %in% reg_learner$param_set$ids()
              if (any(learner_valid)) {
                prefixed_params <- setNames(params[learner_valid], prefixed_names[learner_valid])
                reg_learner$param_set$values <- modifyList(reg_learner$param_set$values, prefixed_params)
              }
              
              missing_in_pipeline <- setdiff(names(params), reg_pipeline$param_set$ids())
              missing_in_learner <- setdiff(names(params), 
                                            sub(paste0("^", best_learner$id, "\\."), "", 
                                                reg_learner$param_set$ids()[startsWith(reg_learner$param_set$ids(), best_learner$id)]))
              if (length(missing_in_pipeline) > 0) warning("Missing in Pipeline (regression): ", paste(missing_in_pipeline, collapse = ", "))
              if (length(missing_in_learner) > 0) warning("Missing in Learner (regression): ", paste(missing_in_learner, collapse = ", "))
            }
          }
          
          # Train regressionmodel
          reg_learner$train(reg_task)
          
          # save models
          learner <- list(classifier = class_learner, regressor = reg_learner)
          
          # if not semicontinous
        } else {
          
          # Basis-Pipeline with best learner
          full_pipeline <- current_learner
          if (grepl("xgboost", best_learner$id)) {
            current_learner$param_set$values <- modifyList(current_learner$param_set$values, xgboost_params)
          } else if (grepl("ranger", best_learner$id)) {
            current_learner$param_set$values <- modifyList(current_learner$param_set$values, ranger_params)
          }
          
          # Handling of missing values
          if (method_var != "xgboost" && supports_missing && !is.null(po_x_miss)) { #xgboost can handle NAs directly, support_missings are learners that can handle missings if they are marked as such
            full_pipeline <- po_x_miss %>>% full_pipeline
          }
          
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
          message("***** Identidy missing values *****")
        }
        
        # impute missing values
        impute_missing_values <- function(data, ref_data) {
          for (col in colnames(data)) {
            if (any(is.na(data[[col]]))) {
              if (is.numeric(ref_data[[col]])) {
                data[[col]][is.na(data[[col]])] <- median(ref_data[[col]], na.rm = TRUE)  # Median
              } else if (is.factor(ref_data[[col]])) {
                mode_value <- names(which.max(table(ref_data[[col]], useNA = "no")))      # Modus
                data[[col]][is.na(data[[col]])] <- mode_value
              }
            }
          }
          return(data)
        }
        
        # missing indices
        missing_idx <- missing_indices[[var]]
        if (length(missing_idx) == 0) {
          next
        }
        
        if (!is_sc) {
          # not semicontinous
          
          variables <- colnames(data_temp)
          feature_cols <- setdiff(variables, var)
          
          if (!isFALSE(selected_formula)) {
            backend_data <- mm_data[missing_idx, ]
            
            # Impute if NA in backend_data
            if (any(is.na(backend_data))) {
              backend_data <- impute_missing_values(backend_data, data_temp)
            }
            
          } else {
            # without formula
            backend_cols <- union(feature_cols, var)
            backend_data <- data_temp[missing_idx, backend_cols, with = FALSE]
            
            if (!supports_missing) {
              backend_data <- impute_missing_values(backend_data, data_y_fill)
            }
          }
          
        } else {
          # semicontinous
          zero_flag_col <- paste0(var, "_zero_flag")
          variables <- colnames(data_temp)
          feature_cols <- setdiff(variables, c(var, zero_flag_col))
          
          print("feature_cols")
          print(feature_cols)
          
          if (!isFALSE(selected_formula)) {
            class_pred_data <- mm_data[missing_idx,]
            if (any(is.na(class_pred_data))) {
              class_pred_data <- impute_missing_values(class_pred_data, data_temp)
            }
            
          } else {
            class_pred_data <- data_temp[missing_idx, c(feature_cols, zero_flag_col), with = FALSE]
            
            if (!supports_missing && any(is.na(class_pred_data))) {
              class_pred_data <- impute_missing_values(class_pred_data, data_temp)
            }
          }
          
          reg_pred_data <- data_temp[data_temp[[var]] > 0, ]
          
          if (!supports_missing && any(is.na(reg_pred_data))) {
            reg_pred_data <- impute_missing_values(reg_pred_data, data_temp)
          }
          
        }
### Identify NAs End ###
        
### *****Select suitable task type Start***** ###################################################################################################
        if (!is_sc) {
          if (is.numeric(data_temp[[target_col]])) {
            pred_task <- TaskRegr$new(
              id = target_col,
              backend =  backend_data,
              target = target_col
            )
          } else if (is.factor(data_temp[[target_col]])) {
            pred_task <- TaskClassif$new(
              id = target_col,
              backend =  backend_data,
              target = target_col
            )
          } else {
            stop("Error: Target variable is neither numeric nor a factor!")
          }
        }

### Select suitable task type End ####
        
### *****Predict Start***** ###################################################################################################
        if(verbose){
          message(paste("***** Predict"))
        }
        
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
        
        # helper decimal places
        get_decimal_places <- function(x) {
          if (is.na(x)) return(0)
          if (x == floor(x)) return(0)
          nchar(sub(".*\\.", "", as.character(x)))
        }
        
        if (is_sc) {
          zero_flag_col <- paste0(var, "_zero_flag")
          variables <- colnames(data_temp)
          feature_cols <- setdiff(variables, c(var, zero_flag_col))
          
          # 1) classification (null vs positive)
          class_learner <- learner$classifier
          
          # no NAs
          class_pred_data <- data_temp[missing_idx, feature_cols, with = FALSE]
          if (anyNA(class_pred_data)) {
            class_pred_data <- impute_missing_values(class_pred_data, data_temp)
          }

          # Prediction without Task (weil Zielvariable nicht vorhanden)
          pred_probs <- class_learner$predict_newdata(class_pred_data)$prob
          
          # predict
          if (isFALSE(sequential) || i == nseq) {
            preds_class <- apply(pred_probs, 1, function(probs) {
              sample(colnames(pred_probs), size = 1, prob = probs)
            })
          } else {
            preds_class <- colnames(pred_probs)[max.col(pred_probs)]
          }
          
          levels_zero_flag <- levels(data_temp[[zero_flag_col]])
          data_temp[[zero_flag_col]][missing_idx] <- ifelse(preds_class == "positive",
                                                            levels_zero_flag[levels_zero_flag == "positive"],
                                                            levels_zero_flag[levels_zero_flag == "zero"])
          
          # 2) regression: for positive predictions
          reg_learner <- learner$regressor
          
          reg_rows <- missing_idx[which(data_temp[[zero_flag_col]][missing_idx] == "positive")]
          if (length(reg_rows) > 0) {
            reg_pred_data <- data_temp[reg_rows, feature_cols, with = FALSE]
            
            if (anyNA(reg_pred_data)) {
              reg_pred_data <- impute_missing_values(reg_pred_data, data_temp[reg_rows])
            }
            
            preds_reg <- reg_learner$predict_newdata(reg_pred_data)$response
          } else {
            preds_reg <- numeric(0)
          }
          
          preds <- data_temp[[var]]  
          preds[missing_idx] <- 0    
          preds[reg_rows] <- preds_reg  
          preds <- preds[missing_idx]
          
          print ("preds")
          print (preds)

          
        } else {
          # not semicontinous 
          
          if (is.factor(data_temp[[target_col]])) {
            
            mod <- switch(method_var,
                          ranger = "classif.ranger",
                          xgboost = "classif.xgboost",
                          regularized = "classif.glmnet",
                          robust = "classif.glm_rob",
                          stop("Unbekannte Methode für Klassifikation:", method_var))
            
            learner$model[[mod]]$param_set$values$predict_type <- "prob"
            
            pred_probs <- learner$predict(pred_task)$prob
            if (is.null(pred_probs)) {
              stop("Fehler bei der Berechnung der Wahrscheinlichkeiten.")
            }
            
            if (isFALSE(sequential) || i == nseq) {
              preds <- apply(pred_probs, 1, function(probs) {
                sample(levels(data_temp[[target_col]]), size = 1, prob = probs)
              })
            } else {
              preds <- apply(pred_probs, 1, which.max)
              preds <- levels(data_temp[[target_col]])[preds]
            }
            
          } else {
            preds <- learner$predict(pred_task)$response
          }
        }
        
        if (!is.null(lhs_transformation)) {
          preds <- inverse_transform(preds, lhs_transformation)
        }
        
        if (inherits(preds, "numeric")) {
          decimal_places <- max(sapply(na.omit(data[[var]]), get_decimal_places), na.rm = TRUE)
          preds <- round(preds, decimal_places)
        }
### Predict End ###################################################################################################
        
### *****PMM Start***** ###################################################################################################
        if(verbose){
          message(paste("***** pmm for predictions"))
        }
        if (pmm[[var]] && is.numeric(data_temp[[var]])) {
          if(verbose){
            print(paste("PMM is applied to", var, "."))
          }
          
          # True observed values from the original data
          observed_values <- na.omit(data[[var]])
          
          # Find the next true value for each prediction
          preds <- sapply(preds, function(x) {
            observed_values[which.min(abs(observed_values - x))]
          })
        }
### PMM End ###  
        
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

        if (length(missing_idx) > 0) {
          if (is.numeric(data_prev[[var]])) {  
            data[missing_idx, (var) := as.numeric(preds)]
          } else if (is.factor(data[[var]])) {
            orig_levels <- levels(data_prev[[var]])
            pred_levels <- unique(as.character(preds))
            # Add any new levels from preds to the original levels
            all_levels <- union(orig_levels, pred_levels)
            data[missing_idx, (var) := factor(preds, levels = all_levels)]
          } else {
            stop(paste("Unknown data type for variable:", var))
          }
          data <- copy(data)
        }
### Replace missing values with predicted values Start End ###
        
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
              if (length(preds) == length(missing_idx) && !all(is.na(preds))) {
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
          if (is_dt) {
            # Calculate number of changed values
            cat_changes <- sum(data[, factor_cols, with = FALSE] != data_prev[, factor_cols, with = FALSE], na.rm = TRUE)
            
            # Calculate total number of categorical values
            total_cat_values <- sum(!is.na(data_prev[, factor_cols, with = FALSE]))
          } else {
            # If 'data' is data.frame 
            cat_changes <- sum(data[factor_cols] != data_prev[factor_cols], na.rm = TRUE)
            total_cat_values <- sum(!is.na(data_prev[factor_cols]))
          }
          
          # Calculate normalized rate of change
          cat_diff <- cat_changes / (total_cat_values + 1e-8)  # +epsilon to avoid division by zero
        } else {
          cat_diff <- 0  # If there are no categorical columns, the difference is 0  
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
       
    if (pred_history) {
      pred_result <- rbindlist(history, fill = TRUE)
      return(list(data = result, pred_history = pred_result))
    }
    
    return(result)
}
  




