#' Xgboost Imputation
#'
#' Impute missing values based on a random forest model using [ranger::ranger()]
#' @param formula model formula for the imputation
#' @param data A `data.frame` containing the data
#' @param imp_var `TRUE`/`FALSE` if a `TRUE`/`FALSE` variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @param ... Arguments passed to [ranger::ranger()]
#' @param verbose Show the number of observations used for training
#'   and evaluating the RF-Model. This parameter is also passed down to
#'   [ranger::ranger()] to show computation status.
#' @param median Use the median (rather than the arithmetic mean) to average
#'   the values of individual trees for a more robust estimate.
#' @return the imputed data set.
#' @family imputation methods
#' @examples 
#' data(sleep)
#' xgboostImpute(Dream~BodyWgt+BrainWgt,data=sleep)
#' xgboostImpute(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#' @export
xgboostImpute <- function(formula, data, imp_var = TRUE,
                         imp_suffix = "imp", ..., verbose = FALSE,
                         nrounds=2, objective=NULL,
                         median = FALSE) {
  check_data(data)
  formchar <- as.character(formula)
  lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
  rhs <- formchar[3]
  rhs2 <- gsub(" ", "", strsplit(rhs, "\\+")[[1]])
  #Missings in RHS variables
  rhs_na <- apply(subset(data, select = rhs2), 1, function(x) any(is.na(x)))
  #objective should be a vector of lenght equal to the lhs variables
  if(!is.null(objective)){
    stopfifnot(length(objective)!=length(lhs))
  }
  for (lhsV in lhs) {
    form <- as.formula(paste(lhsV, "~", rhs))
    lhs_vector <- data[[lhsV]]
    if (!any(is.na(lhs_vector))) {
      cat(paste0("No missings in ", lhsV, ".\n"))
    } else {
      lhs_na <- is.na(lhs_vector)
      if (verbose)
        message("Training   model for ", lhsV, " on ", sum(!rhs_na & !lhs_na), " observations")
      dattmp <- subset(data, !rhs_na & !lhs_na)
      labtmp <- dattmp[[lhsV]]
      if(inherits(labtmp,"factor")){
        labtmp <- as.integer(labtmp)
        if(length(unique(labtmp))==2){
          objective <- "binary:logistic"
        }else if(length(unique(labtmp))>2){
          objective <- "mult:softmax"
        }
      }else if(inherits(labtmp,"numeric")){
        if(length(unique(labtmp))==2){
          warning("binary factor detected but not probably stored as factor.")
          objective <- "binary:logistic"
        }else{
          objective <- "reg:squarederror"
        }
      }else if(inherits(labtmp,"integer")){
        if(length(unique(labtmp))==2){
          warning("binary factor detected but not probably stored as factor.")
          objective <- "binary:logistic"
        }else{
          objective <- "count:poisson"## Todo: this might not be wise as default
        }
      }
        
      
      mm <- model.matrix(form,dattmp)
      mod <- xgboost::xgboost(data=mm, label = labtmp,
                       nrounds=nrounds, objective=objective)
      if (verbose)
        message("Evaluating model for ", lhsV, " on ", sum(!rhs_na & lhs_na), " observations")
      if (median & inherits(lhs_vector, "numeric")) {
        predictions <- apply(
          predict(mod, model.matrix(form,subset(data, !rhs_na & lhs_na)), predict.all = TRUE)$predictions,
          1, median)
      } else {
        predictions <- predict(mod, subset(data, !rhs_na & lhs_na))$predictions
      }
      data[!rhs_na & lhs_na, lhsV] <- predictions
    }
    
    if (imp_var) {
      if (imp_var %in% colnames(data)) {
        data[, paste(lhsV, "_", imp_suffix, sep = "")] <- as.logical(data[, paste(lhsV, "_", imp_suffix, sep = "")])
        warning(paste("The following TRUE/FALSE imputation status variables will be updated:",
                      paste(lhsV, "_", imp_suffix, sep = "")))
      } else {
        data$NEWIMPTFVARIABLE <- is.na(lhs_vector)
        colnames(data)[ncol(data)] <- paste(lhsV, "_", imp_suffix, sep = "")
      }
    }
  }
  data
}