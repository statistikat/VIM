#' Xgboost Imputation
#'
#' Impute missing values based on a random forest model using [xgboost::xgboost()]
#' @param formula model formula for the imputation
#' @param data A `data.frame` containing the data
#' @param imp_var `TRUE`/`FALSE` if a `TRUE`/`FALSE` variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @param ... Arguments passed to [xgboost::xgboost()]
#' @param verbose Show the number of observations used for training
#'   and evaluating the RF-Model. This parameter is also passed down to
#'   [xgboost::xgboost()] to show computation status.
#' @return the imputed data set.
#' @family imputation methods
#' @examples 
#' data(sleep)
#' xgboostImpute(Dream~BodyWgt+BrainWgt,data=sleep)
#' xgboostImpute(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#' xgboostImpute(Dream+NonD+Gest~BodyWgt+BrainWgt,data=sleep)
#' 
#' sleepx <- sleep
#' sleepx$Pred <- as.factor(LETTERS[sleepx$Pred])
#' sleepx$Pred[1] <- NA
#' xgboostImpute(Pred~BodyWgt+BrainWgt,data=sleepx)
#' @export
xgboostImpute <- function(formula, data, imp_var = TRUE,
                         imp_suffix = "imp", verbose = FALSE,
                         nrounds=100, objective=NULL,
                         ...){
  check_data(data)
  formchar <- as.character(formula)
  lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
  rhs <- formchar[3]
  rhs2 <- gsub(" ", "", strsplit(rhs, "\\+")[[1]])
  #Missings in RHS variables
  rhs_na <- apply(subset(data, select = rhs2), 1, function(x) any(is.na(x)))
  #objective should be a vector of lenght equal to the lhs variables
  if(!is.null(objective)){
    stopifnot(length(objective)!=length(lhs))
  }
  for (lhsV in lhs) {
    form <- as.formula(paste(lhsV, "~", rhs,"-1"))
    # formula without left side for prediction
    formPred <- as.formula(paste( "~", rhs,"-1"))
    lhs_vector <- data[[lhsV]]
    num_class <- NULL
    if (!any(is.na(lhs_vector))) {
      cat(paste0("No missings in ", lhsV, ".\n"))
    } else {
      lhs_na <- is.na(lhs_vector)
      if (verbose)
        message("Training model for ", lhsV, " on ", sum(!rhs_na & !lhs_na), " observations")
      dattmp <- subset(data, !rhs_na & !lhs_na)
      labtmp <- dattmp[[lhsV]]
      currentClass <- NULL
      if(inherits(labtmp,"factor")){
        currentClass <- "factor"
        labtmp <- as.integer(labtmp)-1
        if(length(unique(labtmp))==2){
          objective <- "binary:logistic"
        }else if(length(unique(labtmp))>2){
          objective <- "multi:softmax"
          num_class <- max(labtmp)+1
        }
        
      }else if(inherits(labtmp,"numeric")){
        currentClass <- "numeric"
        if(length(unique(labtmp))==2){
          warning("binary factor detected but not probably stored as factor.")
          objective <- "binary:logistic"
        }else{
          objective <- "reg:squarederror"
        }
      }else if(inherits(labtmp,"integer")){
        currentClass <- "integer"
        if(length(unique(labtmp))==2){
          warning("binary factor detected but not probably stored as factor.")
          objective <- "binary:logistic"
        }else{
          objective <- "count:poisson"## Todo: this might not be wise as default
        }
      }
        
      
      mm <- model.matrix(form,dattmp)
      if(!is.null(num_class)){
        mod <- xgboost::xgboost(data = mm, label = labtmp,
                                nrounds=nrounds, objective=objective, num_class = num_class, verbose = FALSE,...)
      }else{
        mod <- xgboost::xgboost(data = mm, label = labtmp,
                                nrounds=nrounds, objective=objective, verbose = FALSE,...)
      }
      
      if (verbose)
        message("Evaluating model for ", lhsV, " on ", sum(!rhs_na & lhs_na), " observations")
      predictions <- 
        predict(mod, model.matrix(formPred,subset(data, !rhs_na & lhs_na)))
      if(currentClass=="factor"){
        data[!rhs_na & lhs_na, lhsV] <- levels(dattmp[,lhsV])[predictions+1]
      }else{
        data[!rhs_na & lhs_na, lhsV] <- predictions  
      }
      
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
