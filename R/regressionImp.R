#' Regression Imputation (via vimpute)
#'
#' Impute missing values based on a regression / classification model.
#' Internally delegates to `vimpute()` with method = "regularized" (glmnet)
#' or "robust" (lmrob/glmrob).
#'
#' @param formula model formula to impute one or several variables
#' @param data A data.frame containing the data
#' @param family ignored in this wrapper version (only "AUTO"-like behaviour via vimpute)
#' @param robust logical; if TRUE use method = "robust", otherwise "regularized"
#' @param imp_var logical; if TRUE create TRUE/FALSE variables indicating imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @param mod_cat ignored in this wrapper version (classification handled by vimpute)
#'
#' @return the imputed data set (same class as `data`)
#' @export

regressionImp <- function(formula, data,
                          family = "AUTO",
                          robust = FALSE,
                          imp_var = TRUE,
                          imp_suffix = "imp",
                          mod_cat = FALSE) {
  
  check_data(data)
  data_out <- as.data.frame(data)
  
  # Warn user about arguments that are no longer honored directly
  if (!identical(family, "AUTO") || !isFALSE(mod_cat)) {
    warning("`family` and `mod_cat` are ignored in this wrapper; ",
            "regressionImp() now delegates to vimpute(), ",
            "which chooses regression vs. classification based on the type of the LHS variable.")
  }
  
  formchar <- as.character(formula)
  lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])   # left-hand side variables
  rhs <- formchar[3]
  rhs2 <- gsub(" ", "", strsplit(rhs, "\\+")[[1]])          # right-hand side variables
  
  for (lhsV in lhs) {
    
    lhs_vector <- data_out[[lhsV]]
    
    # 1) No Missings in Target 
    if (!any(is.na(lhs_vector))) {
      message(sprintf("No missings in %s.", lhsV))
      
      if (imp_var) {
        imp_col <- paste0(lhsV, "_", imp_suffix)
        if (imp_col %in% colnames(data_out)) {
          data_out[[imp_col]] <- as.logical(data_out[[imp_col]])
          warning(
            "The following TRUE/FALSE imputation status variables will be updated: ",
            imp_col
          )
        } else {
          data_out[[imp_col]] <- is.na(lhs_vector)
        }
      }
      next
    }
    
    # 2) Missings -> vimpute() 
    considered <- unique(c(lhsV, rhs2))
    
    # Method: robust vs regularized (glmnet)
    method_name <- if (robust) "robust" else "regularized"
    method <- setNames(as.list(rep(method_name, length(considered))), considered)
    
    # PMM off -> behaviour like original regressionImp 
    pmm <- setNames(as.list(rep(FALSE, length(considered))), considered)
    
    subdata <- data_out[, considered, drop = FALSE]
    
    out <- vimpute(
      data                = subdata,
      considered_variables = considered,
      method              = method,
      pmm                 = pmm,
      sequential          = FALSE,
      nseq                = 1,
      imp_var             = TRUE,         # internaly: _imp-columns
      pred_history        = FALSE,
      tune                = FALSE,
      verbose             = FALSE
    )
    
    data_out[[lhsV]] <- out[[lhsV]]
    
    if (imp_var) {
      vimpute_imp_col <- paste0(lhsV, "_imp")
      target_imp_col  <- paste0(lhsV, "_", imp_suffix)
      
      if (vimpute_imp_col %in% colnames(out)) {
        data_out[[target_imp_col]] <- as.logical(out[[vimpute_imp_col]])
      } else if (!(target_imp_col %in% colnames(data_out))) {
        data_out[[target_imp_col]] <- is.na(lhs_vector)
      }
    }
  }
  
  data_out
}

# OLD FUNCTION
#
# regressionImp <- function(formula, data, family = "AUTO", robust = FALSE, imp_var = TRUE, imp_suffix = "imp", mod_cat = FALSE) {
#   check_data(data)
#   data <- as.data.frame(data)
#   formchar <- as.character(formula)
#   lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
#   rhs <- formchar[3]
#   rhs2 <- gsub(" ", "", strsplit(rhs, "\\+")[[1]])
#   #Missings in RHS variables
#   TFna2 <- apply(subset(data, select = rhs2), 1, function(x) !any(is.na(x)))
#   for(lhsV in lhs) {
#     form <- as.formula(paste(lhsV, "~", rhs))
#     lhs_vector <- data[[lhsV]]
#     #Check if there are missings in this LHS variable
#     if (!any(is.na(lhs_vector))) {
#       cat(paste0("No missings in ", lhsV, ".\n"))
#     } else {
#       if (!inherits(family, "function") & !inherits(family, "family")){
#         if (family == "AUTO") {
#           TFna <- TFna2 & !is.na(lhs_vector)
#           if (is.numeric(lhs_vector)) {
#             nLev <- 0
#             if (robust) {
#               fn <- lmrob
#             } else {
#               fn <- lm
#             }
#             mod <- fn(form, data = data[TFna, ])
#           } else if (inherits(lhs_vector, "factor") || inherits(lhs_vector, "character")) {
#             if (inherits(lhs_vector, "character")) {
#               dataset[[lhsV]] <- as.factor(dataset[[lhsV]])
#               lhs_vector <- data[[lhsV]]
#             }
#             nLev <- length(levels(lhs_vector))
#             if (nLev == 2) {
#               fam <- binomial
#               if (robust) {
#                 fn <- glmrob
#               } else {
#                 fn <- glm
#               }
#               mod <- fn(form, data = data[TFna, ], family = fam)
#             } else {
#               ## TODO: what to do if this clause is executed and !all(!TFna3) ?
#               co <- capture.output(mod <- multinom(form, data[TFna, ]))
#             }
#           }
#           if (imp_var) {
#             if (imp_var %in% colnames(data)) {
#               data[, paste(lhsV, "_", imp_suffix, sep = "")] <- as.logical(data[, paste(lhsV, "_", imp_suffix, sep = "")])
#               warning(paste("The following TRUE/FALSE imputation status variables will be updated:",
#                       paste(lhsV, "_", imp_suffix, sep = "")))
#             } else {
#               data$NEWIMPTFVARIABLE <- is.na(lhs_vector)
#               colnames(data)[ncol(data)] <- paste(lhsV, "_", imp_suffix, sep = "")
#             }
#           }
#           TFna1 <- is.na(lhs_vector)
#           TFna3 <- TFna1 & TFna2
#           if (all(!TFna3)) {
#             #Check if there are missings in this LHS variable where theR RHS variables are "not missing"
#             cat(paste0("No missings in ", lhsV, " with valid values in the predictor variables.\n"))
#           } else {
#             tmp <- data[TFna3, ]
#             tmp[, lhsV] <- 1
#             if (nLev > 2) {
#               if (mod_cat) {
#                 pre <- predict(mod, newdata = tmp)
#               }else{
#                 pre <- predict(mod, newdata = tmp, type = "probs")
#                 pre <- levels(data[, lhsV])[apply(pre, 1, function(x) sample(1:length(x), 1, prob = x))]
#               }
#             } else if (nLev == 2) {
#               if (mod_cat) {
#                 pre <- predict(mod, newdata = tmp, type = "response")
#                 pre <- levels(lhs_vector)[as.numeric(pre>.5)+1]
#               } else {
#                 pre <- predict(mod, newdata = tmp, type = "response")
#                 pre <- levels(lhs_vector)[sapply(pre, function(x) sample(1:2, 1, prob = c(1 - x, x)))]
#               }
#             } else {
#               pre <- predict(mod, newdata = tmp)
#             }
#             if (sum(TFna1) > sum(TFna3))
#               cat(paste("There still missing values in variable", lhsV, ". Probably due to missing values in the regressors.\n"))
#             data[TFna3, lhsV] <- pre
#           }
#         }
#       }else{
#         TFna1 <- is.na(lhs_vector)
#         TFna3 <- TFna1 & TFna2
#         tmp <- data[TFna3, ]
#         tmp[, lhsV] <- 1
#         if(robust)
#           mod <- glmrob(form, data = data, family = family)
#         else
#           mod <- glm(form, data = data, family = family)
#         pre <- predict(mod, newdata = tmp, type = "response")
#         data[TFna3, lhsV] <- pre
#       }
#     }
#   }
#   invisible(data)
# }
