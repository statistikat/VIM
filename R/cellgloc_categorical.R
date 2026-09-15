#' Iteration constants of the categorical EM step
#'
#' \code{.gloc_cat_sweeps}: mean-field sweeps for a row with two or more
#' missing categorical cells. \code{.gloc_cat_max_combos}: a row with more level
#' combinations than this is not expanded (see \code{.gloc_cat_candidates}).
#' \code{.gloc_cat_floor}: lower bound on a prior probability before logs, so a
#' level the multinomial fit rules out can still win on the continuous cells.
#' @keywords internal
.gloc_cat_sweeps <- 5L
#' @rdname dot-gloc_cat_sweeps
#' @keywords internal
.gloc_cat_max_combos <- 256L
#' @rdname dot-gloc_cat_sweeps
#' @keywords internal
.gloc_cat_floor <- 1e-10

#' Split a data frame into continuous and categorical columns
#'
#' The rule \code{imputeCellGLoc} has always used: factor, character and
#' logical columns are categorical, everything else continuous.
#' @param data a data frame.
#' @return \code{list(cont, cat)} of column names.
#' @keywords internal
.gloc_split_vars <- function(data) {
  is_cat <- vapply(data, function(x) is.factor(x) || is.character(x) ||
                     is.logical(x), logical(1))
  list(cont = names(data)[!is_cat], cat = names(data)[is_cat])
}

#' Categorical columns as factors, their missing mask and their levels
#'
#' @param data a data frame.
#' @param cat_vars names of the categorical columns.
#' @return \code{list(F, Mc, levels)}: \code{F} holds the columns as factors of
#'   their observed values (NA kept), \code{Mc} is the \eqn{n x k} missing mask,
#'   \code{levels} the observed levels per column.
#' @keywords internal
.gloc_cat_prepare <- function(data, cat_vars) {
  n <- nrow(data)
  F <- data[, cat_vars, drop = FALSE]
  lev <- stats::setNames(vector("list", length(cat_vars)), cat_vars)
  for (v in cat_vars) {
    x <- F[[v]]
    F[[v]] <- if (is.factor(x)) droplevels(x) else factor(x)
    lev[[v]] <- levels(F[[v]])
  }
  Mc <- matrix(FALSE, n, length(cat_vars), dimnames = list(NULL, cat_vars))
  for (v in cat_vars) Mc[, v] <- is.na(F[[v]])
  list(F = F, Mc = Mc, levels = lev)
}

#' Design rows for categorical values with fixed levels
#'
#' Builds the design the way \code{.gloc_design} does, but with every column's
#' levels fixed to \code{levels} and unused levels kept. Candidate rows that
#' fill a missing cell with each level therefore get the columns of the
#' complete-data design. On complete data the result is identical to
#' \code{.gloc_design}.
#' @param Fr data frame of the categorical columns, without NA.
#' @param design one-sided formula.
#' @param levels named list of levels, from \code{.gloc_cat_prepare}.
#' @return a design matrix.
#' @keywords internal
.gloc_design_rows <- function(Fr, design, levels) {
  if (is.null(design)) design <- ~ 1
  if (!length(levels) || identical(all.vars(design), character(0)))
    return(matrix(1, nrow = nrow(Fr), ncol = 1,
                  dimnames = list(NULL, "(Intercept)")))
  df <- Fr
  for (v in names(levels))
    df[[v]] <- factor(as.character(df[[v]]), levels = levels[[v]],
                      ordered = is.ordered(Fr[[v]]))
  mf <- stats::model.frame(design, data = df, na.action = stats::na.pass,
                           drop.unused.levels = FALSE)
  stats::model.matrix(design, mf)
}

#' Give an imputed categorical column back the class of the original
#'
#' @param f factor of values (observed and imputed).
#' @param orig the original column.
#' @return a factor with \code{orig}'s levels and orderedness, a logical, or a
#'   character vector, following \code{orig}.
#' @keywords internal
.gloc_cat_restore <- function(f, orig) {
  if (is.factor(orig))
    return(factor(as.character(f), levels = levels(orig), ordered = is.ordered(orig)))
  if (is.logical(orig)) return(as.logical(as.character(f)))
  as.character(f)
}
