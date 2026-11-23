primitive.impute <- function(x){
  x.na <- is.na(x)
  if(all(!x.na)|all(x.na)){
    return(x)
  }
  # if(all(x.na)){
  #   warning("no donors present in subsample")
  #   return(x)
  # }
  n.imp <- sum(x.na)
  if(length(x[!x.na])>1){
    x[x.na] <- sample(x[!x.na],n.imp,replace=TRUE)
  }else{
    x[x.na] <- x[!x.na]
  }
  return(x)
}

#' Random donor imputation by exact matching
#'
#' Fill missing values by sampling from groups of rows with the same values in
#' the matching columns. Broaden the groups when a donor is unavailable.
#'
#' @aliases matchImpute
#' @param data A data frame, data table, or matrix.
#' @param variable Unique column names or positive column positions to impute.
#'   Defaults to all columns outside `match_var`. An empty selection skips
#'   imputation and indicator creation.
#' @param match_var Unique matching column names in order of decreasing
#'   priority. Use `character(0)` or `NULL` to start with all rows in one group.
#' @param imp_var Logical. If `TRUE`, write a logical indicator for each target
#'   column. The indicator is `TRUE` where the target was originally missing.
#' @param imp_suffix A single character suffix for the indicator names. Each
#'   name consists of the target column name or position, an underscore, and
#'   this suffix. If a name already exists, choose another suffix or set
#'   `imp_var = FALSE`.
#' @return The imputed data as a data table for data table input and as a data
#'   frame for data frame or matrix input. Row names are carried over using
#'   the standard data frame rules. The `count_missings` attribute is a matrix
#'   with columns `STEP` and `NA_PRESENT`. Step 1 records the total number of
#'   missing cells across the target columns before imputation. Each subsequent
#'   step records that count after one pass.
#' @details
#' 1. Group the rows by all columns in `match_var`. Rows with `NA` in a matching
#'    column can share a group if their other matching values agree.
#' 2. For each target column within each group, use the non-missing entries in
#'    that column as donors. Draw one value uniformly with replacement for
#'    each missing cell. If there is only one donor, copy its value to every
#'    missing cell in the group. If there are no donors, leave those cells for
#'    the next pass. Each target column is imputed separately, so values filled
#'    in the same row can come from different donors.
#' 3. If any target cells are still missing, drop the last remaining matching
#'    column and repeat. For example, `c("region", "age_group")` matches on both
#'    columns first, then on `region` alone, then pools all rows if needed. Each
#'    pass fills only cells that are still missing. Values filled in previous
#'    passes enter the donor pool for later passes.
#'
#' Matching columns with many distinct values can produce small groups and lead
#' to broader fallback groups. A target column that is entirely missing causes
#' an error because it has no donor even when all rows are pooled. Use
#' [set.seed()] before calling `matchImpute()` to reproduce the random draws.
#' @author Johannes Gussenbauer, Alexander Kowarik
#' @seealso [VIM::hotdeck()]
#' @keywords manip
#' @family imputation methods
#' @examples
#' 
#' data(sleep,package="VIM")
#' imp_data <- matchImpute(sleep,variable=c("NonD","Dream","Sleep","Span","Gest"),
#'   match_var=c("Exp","Danger"))
#' 
#' data(testdata,package="VIM")
#' imp_testdata1 <- matchImpute(testdata$wna,match_var=c("c1","c2","b1","b2"))
#'
#' dt <- data.table::data.table(testdata$wna)
#' imp_testdata2 <- matchImpute(dt,match_var=c("c1","c2","b1","b2"))

# working function
#' @export
matchImpute <- function(data,variable=colnames(data)[!colnames(data)%in%match_var],match_var, imp_var=TRUE,
    imp_suffix="imp"){
  check_data(data)
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' must be a data frame, data table, or matrix", call. = FALSE)
  }
  if (missing(match_var)) {
    stop("'match_var' must be supplied", call. = FALSE)
  }
  if (!is.logical(imp_var) || length(imp_var) != 1L || is.na(imp_var)) {
    stop("'imp_var' must be a single TRUE or FALSE value", call. = FALSE)
  }

  is_df <- !is.data.table(data)
  if (is_df) {
    if (is.matrix(data)) {
      data <- as.data.frame(data)
    }
    input_row_names <- .row_names_info(data, 0L)
    data <- as.data.table(data)
  } else {
    data <- data.table::copy(data)
  }
  column_names <- names(data)
  duplicate_names <- column_names[duplicated(column_names)]
  validate_names <- function(columns, argument) {
    if (!is.character(columns) || !is.null(dim(columns)) || anyNA(columns) ||
        any(!nzchar(columns)) || anyDuplicated(columns)) {
      stop("'", argument, "' must contain unique, non-empty column names", call. = FALSE)
    }
    unknown <- setdiff(columns, column_names)
    if (length(unknown)) {
      stop("Unknown columns in '", argument, "' (", paste(unknown, collapse = ", "), ")",
           call. = FALSE)
    }
    ambiguous <- intersect(columns, duplicate_names)
    if (length(ambiguous)) {
      stop("Ambiguous columns in '", argument, "' (", paste(ambiguous, collapse = ", "), ")",
           call. = FALSE)
    }
  }
  if (!is.null(match_var)) {
    validate_names(match_var, "match_var")
  }
  if (is.numeric(variable)) {
    if (!is.null(dim(variable)) || anyNA(variable) || anyDuplicated(variable) ||
        any(variable < 1L | variable > ncol(data) | variable != floor(variable))) {
      stop("'variable' must contain unique column positions within 'data'", call. = FALSE)
    }
  } else {
    validate_names(variable, "variable")
  }

  add_indicators <- imp_var && length(variable) > 0L
  if (add_indicators) {
    if (!is.character(imp_suffix) || length(imp_suffix) != 1L || is.na(imp_suffix)) {
      stop("'imp_suffix' must be a single non-missing character value", call. = FALSE)
    }
    indicator_names <- paste(variable, imp_suffix, sep = "_")
    collisions <- intersect(indicator_names, column_names)
    if (length(collisions)) {
      stop("Imputation indicator columns already exist (", paste(collisions, collapse = ", "),
           "). Choose a different 'imp_suffix' or set 'imp_var = FALSE'.", call. = FALSE)
    }
  }

  tfna <- data[, vapply(.SD, function(x) all(is.na(x)), logical(1L)), .SDcols = variable]
  if (nrow(data) > 0L && any(tfna)) {
    stop(paste0(variable[tfna], collapse = ", "), " ",
         if (sum(tfna) > 1L) "are" else "is", " completely missing", call. = FALSE)
  }
  count_missing <- function(x) sum(vapply(x, function(col) sum(is.na(col)), numeric(1L)))
  na_present <- data[, count_missing(.SD), .SDcols = variable]
  if (add_indicators) {
    data[, (indicator_names) := lapply(.SD, is.na), .SDcols = variable]
  }

  count_missings <- matrix(c(1,na_present),nrow=1,ncol=2)
  colnames(count_missings) <- c("STEP","NA_PRESENT")
  
  i <- length(match_var)
  j <- 1
  while(na_present>0){
    
    if(i>0){
      groups <- as.list(data)[match_var[seq_len(i)]]
      data[, (variable) := lapply(.SD, primitive.impute), by = eval(groups), .SDcols = variable]
    }else{
      data[, (variable) := lapply(.SD, primitive.impute), .SDcols = variable]
    }
    na_present <- data[, count_missing(.SD), .SDcols = variable]
    i <- i-1
    
    j <- j+1
    count_missings <- rbind(count_missings,c(j,na_present))
  }
  attr(data,"count_missings") <- count_missings
  if (is_df) {
    data <- as.data.frame(data)
    attr(data, "row.names") <- input_row_names
  }
  return(data)
  
}
