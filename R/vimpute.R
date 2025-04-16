# Run helper script first
# source("helper_vimpute.R")

#' Impute missing values with prefered Model, sequentially, with hyperparametertuning and with PMM (if wanted)

## PARAMETERS ##
#' @param data - Dataset with missing values. Can be provided as a data.table or data.frame.
#' @param considered_variables - Parameters that should be considered in the model
#' @param method - Specifies which learner is used for imputation. Provide as a list for each variable.
# - ranger
# - xgboost 
# - regularized
# - robust
#' @param pmm - TRUE/FALSE indicating whether predictive mean matching is used. Provide as a list for each variable.
#' @param formula - If not all variables are to be used as predictors, or if transformations or interactions are required (applies to all X, for Y only transformations are possible). Only applicable for the methods "robust" and "regularized".
# - Provide as a list for each variable that requires specific conditions.
# - Example 1: log(Y) ~ exp(X1) + X3 + X4:X5
# - Example 2: Y ~ sqrt(X1) + I(1/X2) + X3*X5
# - For X: follows the rules of model.matrix
# - For Y: transformations supported are log(), exp(), sqrt(), I(1/..). Only applicable for numeric variables.
#' @param sequential - If TRUE, all variables are imputed sequentially.
#' @param nseq - Maximum number of iterations.
#' @param eps - Threshold for convergence.
#' @param imp_var - If TRUE, the imputed values are stored.
#' @param pred_history - If TRUE, all predicted values across all iterations are stored.
#' @param tune - Tunes hyperparameters halfway through iterations, TRUE or FALSE.
#' @return imputed data set or c(imputed data set, prediction history)
#' @export
#'
#' @examples
#' x <- vimpute(data = as.data.table(sleep), sequential = FALSE, pred_history = FALSE)
#' y <- vimpute(data = as.data.table(sleep), sequential = TRUE, nseq = 3, pred_history = FALSE)
#' z <- vimpute(data = as.data.table(sleep), considered_variables = c("Sleep", "Dream", "Span", "BodyWgt"), sequential = FALSE, pred_history = FALSE)
#########################################################################################
#########################################################################################
#########################################################################################

vimpute <- function(
    data,
    considered_variables = names(data), # wenn zB nur 5 von 8 Variablen berücksichtigt werden sollen
    method = setNames(as.list(rep("ranger", length(considered_variables))), considered_variables),
    pmm = setNames(as.list(rep(TRUE, length(considered_variables))), considered_variables),
    formula = FALSE, 
    # formula format:                    list(variable_1 ~ age + lenght, variable_2 ~ width + country) 
    # formula format with transformation: list(log(variable_1) ~ age + inverse(lenght), variable_2 ~ width + country)
    sequential = TRUE,
    nseq = 10,
    eps = 0.005, # Stop: if Imputation changes less than 0.5%
    imp_var = TRUE,
    pred_history = TRUE,
    tune = FALSE
) {
  
  ### ***** Learners START ***** ################################################################################################### 
  
  # Initialisierung von considered_variables, falls NULL
  if (is.null(considered_variables) || length(considered_variables) == 0) {
    considered_variables <- names(data)
  }
  
  # für früheren Abbruch
  no_change_counter <- 0
  # Prüfe ob "robust" in method enthalten ist
  robust_required <- any(unlist(method) == "robust")
  # Falls "robust" benötigt wird --> registriere die robusten Learner
  if (robust_required) {
    register_robust_learners()
  }
  
  learners <- list(
    "regr.cv_glmnet" = lrn("regr.cv_glmnet"), #Regularisierte lineare Regression mit Kreuzvalidierung zur automatischen Auswahl des besten lambda-Werts (Lasso/Ridge-Regression).
    "regr.glmnet" = lrn("regr.glmnet"), #Regularisierte lineare Regression mit Lasso/Ridge-Regularisierung (glmnet), aber ohne automatische lambda-Auswahl.
    "classif.glmnet" = lrn("classif.glmnet"), #Regularisierte logistische Regression für Klassifikationsprobleme mit Lasso/Ridge-Regularisierung.
    "regr.ranger" = lrn("regr.ranger"), #Random-Forest-Regressionsmodell mit schnellem ranger-Algorithmus für effiziente Berechnungen.
    "classif.ranger" = lrn("classif.ranger"), #Random-Forest-Klassifikationsmodell basierend auf ranger für hohe Performance und Skalierbarkeit.
    "regr.xgboost" = lrn("regr.xgboost"), #Gradient Boosting Trees für Regression mit effizientem xgboost-Algorithmus zur Optimierung von Vorhersagen.
    "classif.xgboost" = lrn("classif.xgboost") #Gradient Boosting Trees für Klassifikation, optimiert für hohe Vorhersagegenauigkeit und Skalierbarkeit.
  )
  
  # Falls robuste Learner benötigt werden, füge sie hinzu
  if (robust_required) {
    learners <- c(learners, list(
      "regr.lm_rob" = lrn("regr.lm_rob"), #Robuste lineare Regression, die weniger empfindlich gegenüber Ausreißern und nicht-normalverteilten Fehlern ist.
      "classif.glm_rob" = lrn("classif.glm_rob") #Robuste verallgemeinerte lineare Klassifikation (glm), die robust gegen Ausreißer und nicht-normalverteilte Daten ist.
    ))
  }
  ### LEARNERS Ende ###
  
  # nur definierte Variablen mit rein
  data_all_variables <- as.data.table(data)
  data <-  data[, ..considered_variables]
  
  ### ***** Check Data Start ***** ###################################################################################################
  message(paste("***** Check Data"))
  checked_data <- precheck(data, pmm, formula, method, sequential)
  data         <- checked_data$data
  variables    <- checked_data$variables
  variables_NA <- checked_data$variables_NA
  method       <- checked_data$method
  
  print(paste("data after check:"))
  print(data)
  
  orig_data <- data
  ### Check Data Ende ###
  
  if (!sequential) {
    nseq = 1
  }
  
  ### ***** Def missing indices Start ***** ###################################################################################################
  message(paste("***** Find Missing Indices"))
  missing_indices <- setNames(lapply(variables, function(var) { #stellen der ursprünglichen NAs
    na_idx <- which(is.na(data[[var]]))
    if (length(na_idx) > 0) return(na_idx) else return(integer(0))
  }), variables)
  missing_indices <- missing_indices[!sapply(missing_indices, is.null)]
  names(missing_indices)
  
  print(paste("missing indices before Füllung NAs:"))
  print(missing_indices)
  ### Def missing indices Ende ###
  
  po_ohe <- NULL # one-hot-encoding pipeline (if not needed: NULL)
  data_new <- copy(data)
  original_data <- copy(data)  
  
  if (pred_history == TRUE) {
    history <- list() # prediction history if wanted
  }
  
  count_tuned_better <- 0
  count_default_better <- 0
  
  hyperparameter_cache <- setNames(vector("list", length(variables_NA)), variables_NA)
  tuning_status <- setNames(rep(FALSE, length(variables_NA)), variables_NA)
  
  # Iterative Imputation für nseq iterationen
  for (i in seq_len(nseq)) {
    message(paste("ITERATION", i, "von", nseq))
    data_prev <- copy(data)
    
    for (var in variables_NA) {
      message(paste("***** Imputiere Variable:", var))
      
      data_before <- copy(data)
      variables    <- checked_data$variables
      
      message(paste("***** Prädiktoren wählen"))
      if (!isFALSE(formula)) {
        selected_formula <- select_formula(formula, var)  # formula in der target variable auf der linken Seite steht
        print(paste("formula: ", selected_formula))
      }
      
      ### ***** Formula Extraction Start ***** ###################################################################################################
      if (!isFALSE(formula) && (!isFALSE(selected_formula))) {
        identified_variables <- identify_variables(selected_formula, data, var)
        target_col <- var
        feature_cols <- identified_variables$predictor_variables # alle Variablen die Prädiktoren sind
        selected_cols <- c(target_col, feature_cols)
        
        rewrite_formula <- function(formula, target_variable) {
          formula_str <- as.character(formula)
          new_formula_str <- paste0(target_variable, " ~ ", formula_str[3])
          as.formula(new_formula_str)
        }
        
        rewrited_formula <- rewrite_formula (selected_formula, target_col) # formula in richtiger Art und Weise schreiben
        
        # Version 1: Remove missing values (na.omit)  -> für Training
        is_target_numeric <- is.numeric(data[[target_col]])
        if (is_target_numeric) {
          task_mm_na_omit <- TaskRegr$new(id = "imputation_task_na_omit", backend = na.omit(data), target = target_col)
        } else {
          task_mm_na_omit <- TaskClassif$new(id = "imputation_task_na_omit", backend = na.omit(data), target = target_col)
        }
        
        # modelmatrix für x Variablen
        po_mm_na_omit <- PipeOpModelMatrix$new()
        po_mm_na_omit$param_set$values$formula <- rewrited_formula
        rewrited_formula <- as.formula(paste("~", as.character(rewrited_formula)[3]))
        po_mm_na_omit$param_set$values$formula <- rewrited_formula
        
        mm_task_na_omit <- po_mm_na_omit$train(list(task_mm_na_omit))[[1]]
        data_temp <- mm_task_na_omit$data()
        
        print(paste("data_temp cols", colnames(data_temp)))
        
        # Clean column names
        clean_colnames <- function(names) {
          names <- gsub("\\(", "", names)  # Remove parentheses
          names <- gsub("\\)", "", names)
          names <- gsub(":", "_int_", names)   # Interaction (a:b → a_int_b)
          names <- gsub("\\*", "_cross_", names)  # Crossing (a*b → a_cross_b)
          names <- gsub("\\^", "_pow_", names)  # Power ((a+b)^2 → a_pow_2_b)
          names <- gsub("%in%", "_nest_", names)  # Nesting (b %in% a → b_nest_a)
          names <- gsub("/", "_sub_", names)  # Sub-nested (a / b → a_sub_b)
          names <- gsub("-", "_minus_", names)  # Remove terms (a - b → a_minus_b)
          names <- gsub("\\+", "_plus_", names)  # Explicitly replace plus (a + b → a_plus_b)
          names <- gsub(" ", "", names)  # Remove spaces
          make.names(names, unique = TRUE)  # Ensure valid and unique names
        }
        
        setnames(data_temp, clean_colnames(names(data_temp)))
        
        # Version 2: Impute missing values (Median/Mode)  -> für prediction 
        if (is_target_numeric) {
          task_mm <- TaskRegr$new(id = "imputation_task_mm", backend = data, target = target_col)
        } else {
          task_mm <- TaskClassif$new(id = "imputation_task_mm", backend = data, target = target_col)
        }
        
        pipeline_impute <- po("imputehist") %>>%  # Histogram-based imputation for numeric variables (Median)
          po("imputemode") %>>%                  # Mode imputation for categorical variables
          po("modelmatrix", formula = rewrited_formula) #rewrited_formula) 
        
        pipeline_impute$train(task_mm)
        po_task_mm <- pipeline_impute$predict(task_mm)[[1]]
        mm_data <- po_task_mm$data() # mm_data = transformed data mit auffüllen von missings, data_temp = transformed data ohne missings
        setnames(mm_data, clean_colnames(names(mm_data)))
        
        # Identify target transformation
        lhs_transformation <- identify_lhs_transformation(selected_formula)  # transformationen auf linker seite (log, exp, iverse, sqrt geht nur wenn Y numerisch ist
        
        if (!is.numeric(data_temp[[var]]) && !is.null(lhs_transformation)) {
          stop(paste("Fehler: Die Zielvariable muss numerisch sein, wenn eine Transformation angewendet werden soll. Aktuelle Klasse der Zielvariable:", class(data_temp[[var]])))
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
        ### Formula Extraction Ende ###   
        
      } else {
        lhs_transformation <- NULL
        selected_formula <- FALSE
        feature_cols <- setdiff(variables, var)  
        target_col <- var
        selected_cols <- c(target_col, feature_cols)
        data <- data[, ..selected_cols]
        data_temp <- data
      }
      
      if ("Intercept" %in% colnames(data_temp)) {
        data_temp <- data_temp[, !colnames(data_temp) %in% "Intercept", with = FALSE]
        mm_data <- mm_data[, !colnames(mm_data) %in% "Intercept", with = FALSE]
      }
      
      if (!isFALSE(selected_formula)) {
        if (!method[[var]] %in% c("robust", "regularized")) {
          stop("Fehler: Eine Formula kann nur bei den Methoden 'robust' oder 'regularized' angewendet werden.")
        }
      }
      
      method_var <- method[[var]]
      print(paste("Gewählte Methode für", var, ":", method_var))
      
      ### ***** Passenden Learner wählen Start ***** ###################################################################################################
      message(paste("***** Learner Wählen"))
      if (is.numeric(data[[target_col]])) {
        learners_list <- list(
          regularized = list(learners[["regr.cv_glmnet"]], learners[["regr.glmnet"]]),
          robust = list(learners[["regr.lm_rob"]]),
          ranger = list(learners[["regr.ranger"]]),
          xgboost = list(learners[["regr.xgboost"]])
        )
      } else if (is.factor(data[[target_col]])) {
        if (method_var == "robust" && length(levels(data[[target_col]])) != 2) {
          stop("Fehler: 'classif.glm_rob' kann nur für binäre Klassifikationsprobleme verwendet werden.")
        }
        learners_list <- list(
          regularized = list(learners[["classif.glmnet"]]),
          robust = list(learners[["classif.glm_rob"]]),
          ranger = list(learners[["classif.ranger"]]),
          xgboost = list(learners[["classif.xgboost"]])
        )
      } else {
        next
      }
      learner_candidates <- learners_list[[method_var]]
      ### Passenden Learner wählen Ende ***** ####
      
      ### *****OHE Start***** ###################################################################################################
      message(paste("***** OHE"))
      needs_ohe <- any(sapply(learner_candidates, function(lrn) {
        supports_factors <- "factor" %in% lrn$feature_types
        has_factors <- any(sapply(feature_cols, function(col) is.factor(data_temp[[col]])))
        cat("Learner supports factors:", supports_factors, "\n")
        !supports_factors && has_factors
      }))
      
      # wenn selected formular false dann braucht es kein ohe 
      if (!isFALSE(selected_formula)) {   #model.matrix macht ohe automatisch in mle3
        cat("Selected formula exists, no OHE needed.\n")
        needs_ohe <- FALSE
      }
      
      cat("needs_ohe:", needs_ohe, "\n")
      #BEDINGUNG FÜR OHE
      # (1) Mindestens ein Learner in learner_candidates unterstützt keine Faktoren (factor).
      # (2) Mindestens eine der Feature-Spalten (feature_cols) ist ein Faktor (factor).
      # Prüfe, ob das Ziel eine kategoriale Variable (Factor) oder numerisch ist
      if (is.factor(data_temp[[target_col]])) {
        task_type <- "classif"
      } else {
        task_type <- "regr"
      }
      
      if (needs_ohe) {
        print("One-Hot-Encoding notwendig")
        po_ohe <- po("encode", method = "one-hot")
        
        # OHE auf data anwenden
        if (task_type == "regr") {
          train_task <- as_task_regr(data_temp, target = target_col)  
        } else {
          train_task <- as_task_classif(data_temp, target = target_col)  
        }
        
        po_ohe$train(list(train_task))  # Trainiere den Encoder
        
        # Wende das Encoding auf die Trainingsdaten an
        data_temp <- po_ohe$predict(list(train_task))[[1]]$data()
      }
      ### OHE Ende ###
      
      ### *****Task erstellen Start***** ###################################################################################################Y muss für alle learner gefüllt sein
      message(paste("***** Task Erstellen"))
      data_y_fill <- copy(data_temp)
      supports_missing <- all(sapply(learner_candidates, function(lrn) "missings" %in% lrn$properties))
      if (supports_missing) {
        # Der Learner kann mit `NA`s umgehen → NA-Indikatoren hinzufügen
        print("Der Learner unterstützt `NA`s")
      } else {
        print("Der Learner unterstützt KEINE `NA`s")
      }
      
      if (method_var == "ranger") {
        supports_missing <- FALSE
      }
      
      # # Wenn NA in Zielvariable --> nur mit den Daten trainieren, die keine NA in Y haben
      data_y_fill <- data_y_fill[!is.na(get(target_col))]
      
      # Falls der Learner fehlende Werte nicht unterstützt → na.omit() verwenden
      data_y_fill_final <- if (supports_missing) data_y_fill else na.omit(data_y_fill)
      
      # if (method_var == "robust") {
      #   message("Robuste Methode erkannt - Konvertiere ordered Faktoren in normale Faktoren.")
      #   data_y_fill_final[] <- lapply(data_y_fill_final, function(x) {
      #     if (is.ordered(x)) factor(x, ordered = FALSE) else x
      #   })
      # }
      
      # Task erstellen
      if (is.numeric(data_y_fill_final[[target_col]])) {
        task <- TaskRegr$new(id = target_col, backend = data_y_fill_final, target = target_col)
      } else if (is.factor(data_y_fill_final[[target_col]])) {
        task <- TaskClassif$new(id = target_col, backend = data_y_fill_final, target = target_col)
      } else {
        stop("Fehler: Zielvariable ist weder numerisch noch ein Faktor!")
      }
      
      print(paste("DEBUG"))
      print(task$target_names)  
      print(task$col_roles$feature)  
      print(task$missings())
      
      ### *****Learner erstellen Start***** ###################################################################################################
      message(paste("***** Learner erstellen"))
      
      # XGBoost Parameter setzen
      xgboost_params <- list(
        nrounds = 500,
        max_depth = 7,
        eta = 0.05,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 1
        #verbose = TRUE,
        #nthread = 4
      )
      
      # Ranger Parameter setzen
      ranger_params <- list(
        num.trees = 500,
        num.threads = 4
      )
      
      if (length(learner_candidates) > 1) {
        resample_results <- lapply(learner_candidates, function(lrn) {
          # Setze Parameter basierend auf Learner-Typ
          if (grepl("xgboost", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, xgboost_params)
          } else if (grepl("ranger", lrn$id)) {
            lrn$param_set$values <- modifyList(lrn$param_set$values, ranger_params)
          }
          resample(task, lrn, rsmp("cv", folds = 5))
        })
        
        mse_values <- sapply(resample_results, function(res) res$aggregate(msr("regr.rmse")))
        best_learner <- learner_candidates[[which.min(mse_values)]]
        
        # Print MSE values
        for (j in seq_along(learner_candidates)) {
          print(paste("MSE für", learner_candidates[[j]]$id, ":", mse_values[j]))
        }
      } else {
        best_learner <- learner_candidates[[1]]
      }
      
      # Learner initialisieren und Parameter setzen
      default_learner <- lrn(best_learner$id)
      current_learner <- lrn(best_learner$id)
      best_learner <- lrn(best_learner$id)
      tuned_learner <- lrn(best_learner$id)
      
      # Setze Parameter für den besten Learner
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
        
        print(paste("best_learner$predict_type:", best_learner$predict_type))
      }
      
      # if (length(learner_candidates) > 1) {
      #   resample_results <- lapply(learner_candidates, function(lrn) {
      #     resample(task, lrn, rsmp("cv", folds = 5))
      #   })
      #   mse_values <- sapply(resample_results, function(res) res$aggregate(msr("regr.rmse")))
      #   best_learner <- learner_candidates[[which.min(mse_values)]]
      #   
      #   # Print MSE values
      #   for (j in seq_along(learner_candidates)) {
      #     print(paste("MSE für", learner_candidates[[j]]$id, ":", mse_values[j]))
      #   }
      # } else {
      #   best_learner <- learner_candidates[[1]]
      # }
      # 
      # default_learner <- lrn(best_learner$id)
      # current_learner <- lrn(best_learner$id)
      # best_learner <- lrn(best_learner$id)
      # tuned_learner <- lrn(best_learner$id)
      # 
      # if (is.factor(data_temp[[target_col]])) {
      #   best_learner$predict_type <- "prob"
      #   default_learner$predict_type <- "prob"
      #   current_learner$predict_type <- "prob"
      #   
      #   print(paste("best_learner$predict_type:", best_learner$predict_type))
      # }
      
      ### Learner erstellen Ende ### 
      
      
      ### *****Hyperparameter Start***** ###################################################################################################
      message(paste("***** Parametertuning"))
      print(paste("i:", i))
      
      
      if (!tuning_status[[var]] && nseq >= 2 && tune) {
        #cached_params <- hyperparameter_cache[[var]]
        
        if ((nseq > 2 && i == round(nseq / 2)) || (nseq == 2 && i == 2)) {
          print("Starte Hyperparameter-Tuning")
          tuner = tnr("random_search")
          p = length(task$feature_names)
          
          search_spaces <- list(
            "regr.cv_glmnet" = ps(alpha = p_dbl(0, 1), nfolds = p_int(7, 20)),  
            "regr.glmnet" = ps(alpha = p_dbl(0, 1), lambda = p_dbl(10^-4, 10^2, logscale = TRUE)),
            "classif.glmnet" = ps(alpha = p_dbl(0, 1), lambda = p_dbl(10^-4, 10^2, logscale = TRUE)),
            # alpha = Mischung zwischen lasso (1) und ridge (0)
            # lambda = je größer desto stärker die Regulierung
            
            # Ranger
            "regr.ranger" = ps(num.trees = p_int(500, 700 ,default = 500), min.node.size = p_int(3, 10, default=5), sample.fraction = p_dbl(0.8,1)),
            "classif.ranger" = ps(num.trees = p_int(500, 700), min.node.size = p_int(3, 10), sample.fraction = p_dbl(0.8,1)),
            # min.node.size= Minimale Knotengröße 
            # sample.fraction = Anteil der Daten, die pro Baum gesampled werden
            
            # XGBoost
            "regr.xgboost" = ps(nrounds = p_int(100,500), eta = p_dbl(0.01, 0.3), max_depth = p_int(3, 9),  colsample_bytree = p_dbl(0.7, 0.9)),
            "classif.xgboost" = ps(nrounds = p_int(100,500), eta = p_dbl(0.01, 0.3), max_depth = p_int(3, 9), subsample = p_dbl(0.7, 0.9), colsample_bytree = p_dbl(0.7, 0.9)),
            # eta = Lernrate, niedrige Werte --> stabilere aber langsamere Modelle
            # max_depth = Maximale Baumtiefe, große Werte --> komplexere Muster aber evtl überanpassung
            # subsample = Anteil der Daten, die pro Boosting-Iteration verwendet werden | colsample_bytree = Anteil der Features, die pro Baum verwendet werden
            
            # Robust Models
            "regr.lm_rob" = ps(tuning.chi = p_dbl(1.2,1.8), tuning.psi = p_dbl(1.2,1.8),  max.it = p_int(60,300)), #psi = p_fct(c("bisquare", "optimal")),
            "classif.glm_rob" = ps(method = p_fct(c("Mqle", "WBY")), acc = p_dbl(0, 0.1), test.acc = p_fct(c("coef", "resid")), tcc = p_dbl(1,2), maxit = p_int(30,300))
            # psi = Funktion für die Gewichtung der Residuen 
            # acc = Toleranz für die Konvergenz des Algorithmus | test.acc = Kriterium für die Konvergenzprüfung | tcc = Tuning-Konstante
          )
          
          best_learner_id = best_learner$id  # Beispiel: Ranger ist der beste Learner
          #best_learner = learners[[best_learner_id]]
          search_space = search_spaces[[best_learner_id]]
          
          tryCatch({
            
            # Trainiere ein Modell mit den Default-Parametern
            if (best_learner_id == "classif.xgboost" || best_learner_id == "regr.xgboost") {
              default_learner$param_set$values$nrounds = 100  # Setze einen Standardwert für nrounds
            }
            
            # Erstelle die Tuning-Instanz
            instance = TuningInstanceBatchSingleCrit$new(
              task = task,  # Deine Regression-Aufgabe
              learner = best_learner,
              resampling = rsmp("cv", folds = 5,repeats = 3),
              measure = if (task$task_type == "regr") msr("regr.rmse") else msr("classif.acc"),
              search_space = search_space,
              terminator = trm("evals", n_evals = 50)
            )
            
            # Tuning 
            tuner$optimize(instance)
            
            # Beste Parameter speichern
            best_params <- as.list(instance$result[, learner_param_vals][[1]])
            tuning_status[[var]] <- TRUE
            
            # 4. Vergleich mit Default
            #tuned_learner <- best_learner$clone()
            tuned_learner$param_set$values <- best_params
            
            default_result <- resample(task, default_learner, rsmp("cv", folds = 5))
            tuned_result <- resample(task, tuned_learner, rsmp("cv", folds = 5))
            
            
            # Entscheidung welches Modell besser ist
            if (task$task_type == "regr") {
              if (tuned_result$aggregate(msr("regr.rmse")) < default_result$aggregate(msr("regr.rmse"))) {
                current_learner$param_set$values <- best_params
                count_tuned_better <- count_tuned_better + 1
                
                hyperparameter_cache[[var]] <- list(
                  params = best_params,
                  is_tuned = TRUE
                )
                
              } else {
                current_learner$param_set$values <- default_learner$param_set$values
                count_default_better <- count_default_better + 1
                # Setze is_tuned explizit auf FALSE im Cache
                hyperparameter_cache[[var]] <- list(
                  params = default_learner$param_set$values,
                  is_tuned = FALSE
                )
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
                # Setze is_tuned explizit auf FALSE im Cache
                hyperparameter_cache[[var]] <- list(
                  params = default_learner$param_set$values,
                  is_tuned = FALSE
                )
              }
            }
            
          }, error = function(e) {
            message(paste("Tuning fehlgeschlagen für", var, ":", e$message))
            current_learner$param_set$values <- list() # Fallback zu Defaults
          })
        } else if (tuning_status[[var]]) {
          # Verwende zwischengespeicherte Parameter
          current_learner$param_set$values <- hyperparameter_cache[[var]]$params
        }
      } else {
        # Kein Tuning - verwende Default-Parameter
        current_learner$param_set$values <- list()
      }
      
      ### Hyperparameter Ende ###
      
      ### *****X NAs markieren/Ersetzen Start***** ###################################################################################################
      message(paste("***** NAs in feature variables bearbeiten"))
      if (method_var == "xgboost") {
        print("XGBoost erkennt `NA`s automatisch → Keine NA-Indikatoren nötig.")
        po_x_miss <- NULL  # Keine Modifikation notwendig da xgboost missings als NA akzeptiert
        
      } else if (supports_missing) {
        print("Der Learner unterstützt `NA`s → Markiere fehlende Werte.")  # andere learner erkennen missings nur wenn sie explizit als missing angegeben werden
        if (sum(is.na(data_temp)) > 0)  {  #task$data_temp()
          po_x_miss <- po("missind", param_vals = list(
            affect_columns = mlr3pipelines::selector_all(),
            which = "all",
            type = "factor"
          ))
        } else {
          po_x_miss <- NULL  # Keine Indikatoren hinzufügen, wenn keine NAs existieren. Learner die nicht mit NAs umgehen können: weiter oben wird schon dafür gesort dass entweder na.omit oder auffüllen mit median/modus
        }
        
      } 
      ### X NAs markieren/Ersetzen Ende ###     
      
      ### *****Trainiere Modell Start***** ###################################################################################################
      message("***** Train Model")
      
      if (!is.null(lhs_transformation)) {
        print(paste("Erkannte Transformation:", lhs_transformation))
      } else {
        print("Keine Transformation erkannt (lhs_transformation nicht gefunden).")
      }
      
      # Basis-Pipeline mit bestem Learner
      full_pipeline <- default_learner
      
      # Behandlung von fehlenden Werten
      if (method_var != "xgboost" && supports_missing && !is.null(po_x_miss)) { #xgboost kann direkt mit NAs umgehen, support_missings sind learner die mit missings umgehen können wenn diese so gekennzeichnet sind
        full_pipeline <- po_x_miss %>>% full_pipeline
      }
      
      # param set von tuning
      print("best_learner$param_set$values") 
      print(best_learner$param_set$values) 
      print("full_pipeline$param_set$values")
      print(full_pipeline$param_set$values)
      
      # GraphLearner erstellen und trainieren
      learner <- GraphLearner$new(full_pipeline)
      
      # Überprüfen, ob Hyperparameter optimiert wurden und im Cache sind
      if (isTRUE(tuning_status[[var]])) {
        # Wenn Tuning durchgeführt wurde
        if (!is.null(hyperparameter_cache[[var]]) && isTRUE(hyperparameter_cache[[var]]$is_tuned)) {
          # Wenn optimierte Parameter im Cache existieren
          params <- hyperparameter_cache[[var]]$params
          cat(sprintf("Verwende optimierte Parameter aus dem Cache für %s\n", var))
          
          # 1. Parameter in full_pipeline setzen (ohne Präfix)
          pipeline_valid <- intersect(names(params), full_pipeline$param_set$ids())
          full_pipeline$param_set$values <- modifyList(
            full_pipeline$param_set$values,
            params[pipeline_valid]
          )
          
          # 2. Parameter im GraphLearner setzen (mit Präfix)
          prefixed_names <- paste0(best_learner$id, ".", names(params))
          learner_valid <- prefixed_names %in% learner$param_set$ids()
          
          if (any(learner_valid)) {
            prefixed_params <- setNames(params[learner_valid], prefixed_names[learner_valid])
            learner$param_set$values <- modifyList(
              learner$param_set$values,
              prefixed_params
            )
          }
          
          # 3. Ausgabe der gesetzten Parameter
          cat("In Pipeline gesetzt:\n")
          print(full_pipeline$param_set$values[pipeline_valid])
          cat("In Learner gesetzt:\n")
          print(learner$param_set$values[prefixed_names[learner_valid]])
          
          # 4. Warnungen für nicht existierende Parameter
          missing_in_pipeline <- setdiff(names(params), full_pipeline$param_set$ids())
          missing_in_learner <- setdiff(names(params), 
                                        sub(paste0("^", best_learner$id, "\\."), "", 
                                            learner$param_set$ids()[startsWith(learner$param_set$ids(), best_learner$id)]))
          
          if (length(missing_in_pipeline) > 0) {
            warning("Fehlend in Pipeline: ", paste(missing_in_pipeline, collapse = ", "))
          }
          if (length(missing_in_learner) > 0) {
            warning("Fehlend in Learner: ", paste(missing_in_learner, collapse = ", "))
          }
          
        } else {
          # Wenn kein Cache vorhanden oder nicht getunt
          cat("Keine optimierten Parameter im Cache gefunden, verwende Standardparameter\n")
        }
      } else {
        # Wenn kein Tuning durchgeführt wurde
        cat("Kein Hyperparameter-Tuning durchgeführt, verwende Standard-Parameter\n")
      }
      
      # Predict-Type für Klassifikationsprobleme setzen
      if (exists("target_col") && is.factor(data_temp[[target_col]])) {
        learner$predict_type <- "prob"
        cat("Predict-Type auf 'prob' gesetzt für Klassifikationsaufgabe\n")
      }
      
      # Debug-Ausgaben
      print("Aktuelle Learner-Parameter:")
      print(learner$param_set$values)
      
      learner$train(task)
      
      ### Trainiere Modell Ende ###
      
      ### *****Identifiziere NAs Start***** ###################################################################################################
      message("***** Identifiziere fehlende Werte *****")
      
      # Funktion zur Imputation fehlender Werte
      impute_missing_values <- function(data, ref_data) {
        for (col in colnames(data)) {
          if (any(is.na(data[[col]]))) {
            if (is.numeric(ref_data[[col]])) {
              data[[col]][is.na(data[[col]])] <- median(ref_data[[col]], na.rm = TRUE) # Median für numerische Werte
            } else if (is.factor(ref_data[[col]])) {
              mode_value <- names(which.max(table(ref_data[[col]], useNA = "no"))) # Modus für kategorische Werte
              data[[col]][is.na(data[[col]])] <- mode_value
            }
          }
        }
        return(data)
      }
      
      # Zeilenindizes mit fehlenden Werten in `var`
      missing_idx <- missing_indices[[var]]
      if (length(missing_idx) == 0) {
        print(paste("Keine fehlenden Werte in ", var, " → Keine Vorhersage notwendig."))
        return(NULL)
      }
      
      # Bestimme die Prädiktorvariablen (alle außer `var`wenn KEINE formula angegeben wird)
      variables <- colnames(data_temp)
      feature_cols <- setdiff(variables, var)
      
      # Unterscheidung: Ist eine `selected_formula` vorhanden?
      if (!isFALSE(selected_formula)) {
        print(paste("Formula-Modus aktiviert → Transformierte Design-Matrix (`mm_data`) wird verwendet."))
        
        # Extrahiere fehlende Zeilen aus der Design-Matrix
        backend_data <- mm_data[missing_idx, , drop = FALSE]
        
        # Prüfe, ob `backend_data` selbst fehlende Werte enthält
        if (any(is.na(backend_data))) {
          print(paste("Fehlende Werte in den Prädiktorvariablen gefunden → Imputation mit Median/Modus."))
          backend_data <- impute_missing_values(backend_data, data_temp)
        }
        
      } else {
        print(paste("Kein `selected_formula` → Nutze Originaldaten (`data_temp`)."))
        
        # Falls keine Formel, nutze die Originaldaten ohne `var`
        backend_cols <- union(feature_cols, var)
        backend_data <- data_temp[missing_idx, backend_cols, with = FALSE]
        
        # Falls der learner keine `NA`s unterstützt, müssen diese imputiert werden
        if (!supports_missing) {
          print(paste("Der Learner unterstützt keine `NA`s → Vorbereitung der `backend_data`."))
          backend_data <- impute_missing_values(backend_data, data_y_fill)
        }
      }
      
      ### Identifiziere NAs Ende ###
      
      ### *****Wähle passenden Task Type Start***** ###################################################################################################
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
        stop("Fehler: Zielvariable ist weder numerisch noch ein Faktor!")
      }
      ### Wähle passenden Task Type Ende ####
      
      ### *****Predict Start***** ###################################################################################################
      message(paste("***** Predict"))
      
      # wenn target factor variable ist --> dann stochastische Klassenzuordnung
      if (is.factor(data_temp[[target_col]])) {
        #learner$predict_type <- "prob" # Wahrscheinlichkeiten berechnen
        
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
        print(learner$model[[mod]]$param_set$values)
        
        pred_probs <- learner$predict(pred_task)$prob
        
        formatted_output <- capture.output(print(pred_probs))
        print(paste("pred probs: ", formatted_output))
        
        if (is.null(pred_probs)) {
          stop("Fehler bei der Berechnung der Vorhersagewahrscheinlichkeiten.")
        }
        
        if (isFALSE(seq) || i == nseq) {
          print(paste("stochastische Klassenzuordnung"))
          preds <- apply(pred_probs, 1, function(probs) {
            sample(levels(data_temp[[target_col]]), size = 1, prob = probs) # bei letzter Iteration: stochastische Klassenzuordnung
          })
        } else {
          # In allen anderen Iterationen wird die Klasse mit der höchsten Wahrscheinlichkeit gewählt
          preds <- apply(pred_probs, 1, which.max) #In allen anderen Iterationen wird die Klasse mit der höchsten Wahrscheinlichkeit (which.max) gewählt.
          preds <- levels(data_temp[[target_col]])[preds]
        }
        
      } else {
        # Für numerische Variablen: Normale Vorhersage
        preds <- learner$predict(pred_task)$response #Wenn die Zielvariable kein Faktor ist, wird die normale Vorhersage ($response) verwendet.
      }
      
      
      # Rücktransformation der Vorhersagen (falls notwendig)
      if (!is.null(lhs_transformation)) {
        if (lhs_transformation == "exp") {
          inverse_transformation <- function(x) log(x)
        } else if (lhs_transformation == "log") {
          inverse_transformation <- function(x) exp(x)
        } else if (lhs_transformation == "sqrt") {
          inverse_transformation <- function(x) x^2
        } else if (lhs_transformation == "inverse") {
          inverse_transformation <- function(x) 1 / x
        } else {
          stop("Unknown transformation: ", lhs_transformation)
        }
        preds <- inverse_transformation(preds)
      }
      
      # Function to determine the number of decimal places
      if (class(preds) == "numeric") {
        
        get_decimal_places <- function(x) {
          if (is.na(x)) return(0)
          if (x == floor(x)) return(0)
          nchar(sub(".*\\.", "", as.character(x)))
        }
        
        # nachkommastellen wie im "original" datensatz
        decimal_places <- max(sapply(na.omit(data[[var]]), get_decimal_places), na.rm = TRUE)
        preds <- round(preds, decimal_places)
      }
      
      #print(paste("***** Model Information *****"))
      #print(learner$model) 
      #cat(paste("prediction for variable:", var, "in iteration:", i, "PRED:", preds, "\n"))
      ### Predict Ende ###################################################################################################
      ### *****PMM Start***** ###################################################################################################
      message(paste("***** pmm for predictions"))
      if (pmm[[var]] && is.numeric(data_temp[[var]])) {
        print(paste("PMM wird auf", var, "angewendet."))
        
        # Echte beobachtete Werte aus den Originaldaten
        observed_values <- na.omit(data[[var]])
        
        # Für jede Vorhersage den nächsten echten Wert finden
        preds <- sapply(preds, function(x) {
          observed_values[which.min(abs(observed_values - x))]
        })
      }
      #cat(paste("prediction for variable:", var, "in iteration:", i, "PRED after PMM:", preds, "\n"))
      ### PMM Ende ###  
      
      ### *****Prediction History Start***** ###################################################################################################
      message(paste("***** Predict History"))
      if (pred_history == TRUE) {
        history[[paste0(var, "_iter", i)]] <- data.table(
          iteration = i,
          variable = var,
          index = missing_idx,
          predicted_values = preds
        )
      }
      #print(paste("prediction History:", history))
      ### Prediction History ENDE ###
      
      ### *****Ersetze fehlende mit vorhergesagten Werten Start***** ###################################################################################################
      message(paste("***** Ersetze Werte mit neuen Predictions"))
      if (length(missing_idx) > 0) {
        # Update values in `data`
        if (is.numeric(data_prev[[var]])) {  
          data[missing_idx, (var) := as.numeric(preds)]
        } else if (is.factor(data[[var]])) {
          data[missing_idx, (var) := factor(preds, levels = levels(data_prev[[var]]))]
        } else {
          stop(paste("Unknown data type for variable:", var))
        }
        data <- copy(data)
      }
      
      print(paste("test debug data:", head(data), "anzahl rows:", nrow(data)))
      ### Ersetze fehlende mit vorhergesagten Werten ENDE ###
      
      ### *****Import Variable Start***** ###################################################################################################
      message(paste("***** Import Variable (imp_var = TRUE)"))
      if (length(missing_idx) > 0) {
        if (imp_var) {
          imp_col <- paste0(var, "_imp")    # Name der _imp-Spalte
          
          # Prüfen, ob die Variable fehlende Werte anhand von `missing_idx` enthält
          if (!is.null(missing_idx) && length(missing_idx) > 0) {
            # Falls `_imp`-Spalte nicht existiert, erstelle sie als boolesche Variable
            if (!(imp_col %in% colnames(data_new))) {
              data_new[, (imp_col) := FALSE]
            }
            
            # `data_new[[var]]` bleibt synchron mit `data`
            data_new[, (var) := data[[var]]]
            
            # Sicherstellen, dass `preds` nicht NULL oder leer ist
            if (length(preds) == length(missing_idx) && !all(is.na(preds))) {
              # Setze die Imputation als TRUE für fehlende Werte
              set(data_new, i = missing_idx, j = imp_col, value = TRUE)
            } else {
              warning(paste("WARNUNG: `preds` ist leer oder hat nicht die gleiche Länge wie `missing_idx` für", var))
            }
          }
        }
      }
    }
    ### Import Variable ENDE ###
    
    ### *****Stop Kriterium Start***** ###################################################################################################
    if (sequential && i != 1) {
      # Prüfe, ob 'data' eine data.table ist
      is_dt <- inherits(data, "data.table")
      
      # Automatische Erkennung der Variablentypen
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      factor_cols <- names(data)[sapply(data, is.factor)]
      
      # Berechnung der numerischen Differenzen
      if (length(numeric_cols) > 0) {  
        epsilon <- 1e-8  # Vermeidung von Division durch Null  
        
        if (is_dt) {  
          # Standardabweichung jeder Spalte in data_prev berechnen  
          std_dev <- sapply(data_prev[, mget(numeric_cols)], sd, na.rm = TRUE)  
          
          # Normierte relative Änderung berechnen  
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
          # Anzahl der geänderten Werte berechnen
          cat_changes <- sum(data[, ..factor_cols] != data_prev[, ..factor_cols], na.rm = TRUE)
          
          # Gesamtanzahl der kategorialen Werte berechnen
          total_cat_values <- sum(!is.na(data_prev[, ..factor_cols]))
        } else {
          # Falls 'data' ein data.frame ist
          cat_changes <- sum(data[factor_cols] != data_prev[factor_cols], na.rm = TRUE)
          total_cat_values <- sum(!is.na(data_prev[factor_cols]))
        }
        
        # Normalisierte Änderungsrate berechnen
        cat_diff <- cat_changes / (total_cat_values + 1e-8)  # +epsilon zur Vermeidung von Division durch Null
      } else {
        cat_diff <- 0  # Falls keine kategorialen Spalten vorhanden sind, ist die Differenz 0  
      }
      
      # Gesamtänderung berechnen
      total_diff <- num_diff + cat_diff
      
      # Konvergenzprüfung
      if (total_diff < eps) {
        no_change_counter <- no_change_counter + 1
        if (no_change_counter >= 2) {
          message("Konvergenz erreicht nach zwei aufeinanderfolgenden Iterationen ohne Änderungen")
          print(paste("stoppe nach", i, "Iterationen"))
          
          if (is.factor(data_temp[[target_col]])) {
            #learner$predict_type <- "prob" # Wahrscheinlichkeiten berechnen
            
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
            print(paste("pred probs: ", formatted_output))
            
            if (is.null(pred_probs)) {
              stop("Fehler bei der Berechnung der Vorhersagewahrscheinlichkeiten.")
            }
            
            preds <- apply(pred_probs, 1, function(probs) {
              sample(levels(data_temp[[target_col]]), size = 1, prob = probs) # bei letzter Iteration: stochastische Klassenzuordnung
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
  if (tune) {
    print(paste("Anzahl der Fälle, in denen das getunte Modell besser war:", count_tuned_better))
    print(paste("Anzahl der Fälle, in denen das Default-Modell besser war:", count_default_better))
  }
  
  if (tune) {
    message("Tuning-Zusammenfassung:")
    for (var in names(hyperparameter_cache)) {
      if (!is.null(hyperparameter_cache[[var]])) {
        message(paste("Variable:", var, "- Parameter:", 
                      paste(names(hyperparameter_cache[[var]]), hyperparameter_cache[[var]], sep = "=", collapse = ", ")))
      }
    }
  }
  
  ### Stop Kriterium ENDE ###
  
  # Stelle sicher, dass `data` und `data_new` data.table-Objekte sind
  result <- as.data.table(if (imp_var) data_new else data)  # Standard: Nur `data` zurückgeben
  
  data_all_variables[, names(result) := result]
  
  if (pred_history) {
    pred_result <- rbindlist(history, fill = TRUE)
    return(list(data = data_all_variables, pred_history = pred_result))
  }
  
  return(data_all_variables)
  
}


