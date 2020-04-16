#' Random Forest Imputation
#'
#' Impute missing values based on a random forest model.
#' @param formula model formula to impute one variable
#' @param data A data.frame or survey object containing the data
#' @param imp_var `TRUE`/`FALSE` if a `TRUE`/`FALSE` variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @examples 
#' data(sleep)
#' randomForestImp_work(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#' @export
randomForestImp_work <- function(formula, data, imp_var = TRUE,
                                 imp_suffix = "imp") {
  formchar <- as.character(formula)
  lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
  rhs <- formchar[3]
  rhs2 <- gsub(" ", "", strsplit(rhs, "\\+")[[1]])
  #Missings in RHS variables
  rhs_na <- apply(subset(data, select = rhs2), 1, function(x) any(is.na(x)))
  for (lhsV in lhs) {
    form <- as.formula(paste(lhsV, "~", rhs))
    lhs_vector <- data[[lhsV]]
    lhs_na <- is.na(lhs_vector)
    mod <- ranger::ranger(form, subset(data, !rhs_na & !lhs_na))
    predictions <- predict(mod, subset(data, !rhs_na & lhs_na))$predictions
    data[!rhs_na & lhs_na, lhsV] <- predictions
    
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