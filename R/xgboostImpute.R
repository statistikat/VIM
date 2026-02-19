#' Xgboost Imputation
#'
#' Impute missing values based on a random forest model using [xgboost::xgboost()]
#' @param formula model formula for the imputation
#' @param data A `data.frame` containing the data
#' @param imp_var `TRUE`/`FALSE` if a `TRUE`/`FALSE` variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @param verbose Show the number of observations used for training
#'   and evaluating the RF-Model. This parameter is also passed down to
#'   [xgboost::xgboost()] to show computation status.
#' @param ... Arguments passed to [xgboost::xgboost()]
#' @param nrounds max number of boosting iterations,
#'   argument passed to [xgboost::xgboost()]
#' @param objective objective for xgboost,
#'   argument passed to [xgboost::xgboost()]
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
  if (!is.null(objective)) {
    stopifnot(length(objective) == length(lhs))
  }
  dots <- list(...)
  if (nrounds != 100 || !is.null(objective) || length(dots) > 0) {
    warning("`nrounds`, `objective`, and additional xgboost arguments are ignored; xgboostImpute() delegates to vimpute().")
  }

  data_out <- data
  for (lhsV in lhs) {
    lhs_vector <- data_out[[lhsV]]
    if (!any(is.na(lhs_vector))) {
      cat(paste0("No missings in ", lhsV, ".\n"))
      if (imp_var) {
        imp_col <- paste0(lhsV, "_", imp_suffix)
        if (imp_col %in% colnames(data_out)) {
          data_out[[imp_col]] <- as.logical(data_out[[imp_col]])
          warning(paste("The following TRUE/FALSE imputation status variables will be updated:", imp_col))
        } else {
          data_out[[imp_col]] <- is.na(lhs_vector)
        }
      }
      next
    }

    considered <- unique(c(lhsV, rhs2))
    method <- setNames(as.list(rep("xgboost", length(considered))), considered)
    pmm <- setNames(as.list(rep(FALSE, length(considered))), considered)

    if (verbose) {
      rhs_na <- apply(subset(data_out, select = rhs2), 1, function(x) any(is.na(x)))
      lhs_na <- is.na(lhs_vector)
      message("Training model for ", lhsV, " on ", sum(!rhs_na & !lhs_na), " observations")
      message("Evaluating model for ", lhsV, " on ", sum(!rhs_na & lhs_na), " observations")
    }

    out <- vimpute(
      data = data_out[, considered, drop = FALSE],
      considered_variables = considered,
      method = method,
      pmm = pmm,
      sequential = FALSE,
      nseq = 1,
      imp_var = imp_var,
      pred_history = FALSE,
      tune = FALSE,
      verbose = verbose
    )

    data_out[[lhsV]] <- out[[lhsV]]
    if (imp_var) {
      vimpute_imp_col <- paste0(lhsV, "_imp")
      target_imp_col <- paste0(lhsV, "_", imp_suffix)
      if (vimpute_imp_col %in% colnames(out)) {
        data_out[[target_imp_col]] <- as.logical(out[[vimpute_imp_col]])
      } else if (!(target_imp_col %in% colnames(data_out))) {
        data_out[[target_imp_col]] <- is.na(lhs_vector)
      }
    }
  }
  data_out
}
