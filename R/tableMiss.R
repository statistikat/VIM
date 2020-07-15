colored_column <- function(x, colname, delimiter = "_imp") {
  values <- x[[colname]]
  imp <- x[[paste0(colname, delimiter)]]
  values[imp] <- paste("<b><font color = 'orange'>", values[imp], "</font></b>")
  values[is.na(values)] <- paste(
    "<font color = 'red'>", values[is.na(values)], "</font>"
  )
  values
}

#' create table with highlighted missings/imputations
#'
#' Create a `reactable` table that highlights missing values and imputed values
#' with the same colors as [histMiss()]
#'
#' @inheritParams histMiss
#' @examples
#' data(tao)
#' x_IMPUTED <- kNN(tao[, c("Air.Temp", "Humidity")])
#' tableMiss(x_IMPUTED[105:114, ])
#' x_IMPUTED[106, 2] <- NA
#' x_IMPUTED[105, 1] <- NA
#' x_IMPUTED[107, "Humidity_imp"] <- TRUE
#' tableMiss(x_IMPUTED[105:114, ])
tableMiss <- function(x, delimiter = "_imp") {
  names <- names(x)
  imputed_cols <- names[grepl("imp", names)]
  imputed_cols <- substr(imputed_cols, 1, nchar(imputed_cols) - 4)
  for (col in imputed_cols)
    x[col] <- colored_column(x, col, delimiter)
  reactable::reactable(
    x[, !grepl("imp", names)],
    defaultColDef = colDef(html = TRUE), highlight = TRUE
  )
}
