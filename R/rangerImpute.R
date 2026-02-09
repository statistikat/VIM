#' Random Forest Imputation
#'
#' Impute missing values based on random-forest models via [vimpute()].
#' @param formula model formula for the imputation
#' @param data A `data.frame` containing the data
#' @param imp_var `TRUE`/`FALSE` if a `TRUE`/`FALSE` variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @param ... Additional arguments. Currently ignored because
#'   `rangerImpute()` delegates to [vimpute()].
#' @param verbose Show the number of observations used for training
#'   and evaluating the RF-Model.
#' @param median `TRUE`/`FALSE`. If `TRUE`, ranger regression predictions are
#'   aggregated tree-wise using the median (via [vimpute()]).
#' @return the imputed data set.
#' @family imputation methods
#' @examples 
#' data(sleep)
#' rangerImpute(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#' @export
rangerImpute <- function(formula, data, imp_var = TRUE,
                         imp_suffix = "imp", ..., verbose = FALSE,
                         median = FALSE) {
  check_data(data)
  formchar <- as.character(formula)
  lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
  rhs <- formchar[3]
  rhs2 <- gsub(" ", "", strsplit(rhs, "\\+")[[1]])
  rhs2 <- rhs2[rhs2 != "1"]
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Additional ranger arguments are ignored; only `median` is passed to vimpute().")
  }

  data_out <- data
  data_out_df <- as.data.frame(data_out)

  for (lhsV in lhs) {
    lhs_vector <- data_out[[lhsV]]
    imp_col <- paste0(lhsV, "_", imp_suffix)

    if (!any(is.na(lhs_vector))) {
      cat(paste0("No missings in ", lhsV, ".\n"))
      if (imp_var) {
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
    method <- setNames(as.list(rep("ranger", length(considered))), considered)
    pmm <- setNames(as.list(rep(FALSE, length(considered))), considered)

    if (verbose) {
      rhs_na <- if (length(rhs2) > 0) {
        apply(data_out_df[, rhs2, drop = FALSE], 1, function(x) any(is.na(x)))
      } else {
        rep(FALSE, nrow(data_out_df))
      }
      lhs_na <- is.na(lhs_vector)
      message("Training model for ", lhsV, " on ", sum(!rhs_na & !lhs_na), " observations")
      message("Evaluating model for ", lhsV, " on ", sum(!rhs_na & lhs_na), " observations")
    }

    out <- vimpute(
      data = data_out_df[, considered, drop = FALSE],
      considered_variables = considered,
      method = method,
      pmm = pmm,
      sequential = FALSE,
      nseq = 1,
      imp_var = imp_var,
      pred_history = FALSE,
      tune = FALSE,
      verbose = verbose,
      ranger_median = median
    )

    data_out[[lhsV]] <- out[[lhsV]]
    data_out_df[[lhsV]] <- out[[lhsV]]

    if (imp_var) {
      vimpute_imp_col <- paste0(lhsV, "_imp")
      if (imp_col %in% colnames(data_out)) {
        data_out[[imp_col]] <- as.logical(data_out[[imp_col]])
        warning(paste("The following TRUE/FALSE imputation status variables will be updated:", imp_col))
      }
      if (vimpute_imp_col %in% colnames(out)) {
        data_out[[imp_col]] <- as.logical(out[[vimpute_imp_col]])
      } else if (!(imp_col %in% colnames(data_out))) {
        data_out[[imp_col]] <- is.na(lhs_vector)
      }
    }
  }

  data_out
}
