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

#' Weighted multinomial logistic regression on the other categorical columns
#'
#' @param y factor response.
#' @param Xdf data frame of factor predictors.
#' @param w nonnegative case weights.
#' @return an \code{nnet::multinom} fit. \code{multinom} starts from zero
#'   weights (\code{rang = 0}), so it draws no random numbers.
#' @keywords internal
.gloc_multinom <- function(y, Xdf, w) {
  dd <- cbind(data.frame(.y = y), Xdf)
  dd$.w <- w
  # A formula built with "." and then "- .w" leaves .w in the terms object's
  # predvars (a documented R quirk: the "." expansion collects every data
  # column into "variables" before the "-" removes it from term.labels), so
  # predict() on newdata without a .w column then fails with "object '.w' not
  # found". Naming the predictors explicitly avoids the quirk.
  form <- stats::reformulate(names(Xdf), ".y")
  suppressMessages(nnet::multinom(form, data = dd, weights = .w,
                                  trace = FALSE, maxit = 200, MaxNWts = 100000))
}

#' Fit the prior model of every categorical column
#'
#' Each categorical column gets a multinomial logistic regression on the other
#' categorical columns, with main effects and case weights \code{wp}. Pseudo-rows
#' of rows with a missing cell carry their posterior weights, which makes this
#' the M-step of the categorical part. A column that is the only categorical
#' one, or whose weighted response has one level, uses its weighted marginal
#' frequencies. So does a column whose fit fails, with one warning.
#' @param Fp data frame of factor columns without NA (the pseudo-row table).
#' @param wp case weights, one per row of \code{Fp}.
#' @param levels named list of levels.
#' @param fit the fitting function; exposed so the fallback is testable.
#' @return a named list of prior models; see \code{.gloc_cat_prior}.
#' @keywords internal
.gloc_cat_fit_priors <- function(Fp, wp, levels, fit = .gloc_multinom) {
  vars <- names(levels)
  out <- stats::setNames(vector("list", length(vars)), vars)
  failed <- character(0)
  for (v in vars) {
    marg <- vapply(levels[[v]], function(l) sum(wp[Fp[[v]] == l]), numeric(1))
    marg <- marg / sum(marg)
    preds <- setdiff(vars, v)
    y <- droplevels(Fp[[v]])
    m <- NULL
    if (length(preds) && nlevels(y) >= 2L) {
      m <- tryCatch(fit(y, Fp[, preds, drop = FALSE], wp), error = function(e) NULL)
      if (!is.null(m) && !all(is.finite(stats::fitted(m)))) m <- NULL
      if (is.null(m)) failed <- c(failed, v)
    }
    out[[v]] <- if (is.null(m))
      list(type = "marginal", probs = unname(marg), levels = levels[[v]])
    else
      list(type = "multinom", model = m, preds = preds, levels = levels[[v]],
           fitted_levels = levels(y), probs = unname(marg))
  }
  if (length(failed))
    warning(sprintf(paste("cellGLoc: nnet::multinom() failed for categorical",
                          "variable(s) %s; their level probabilities use the",
                          "weighted marginal frequencies instead."),
                    paste(failed, collapse = ", ")), call. = FALSE)
  out
}

#' Prior level probabilities for new rows
#'
#' @param pr one element of \code{.gloc_cat_fit_priors}.
#' @param Fnew data frame of factor columns; the response column is ignored.
#' @return an \eqn{m x L} matrix in level order, floored at
#'   \code{.gloc_cat_floor} and renormalised by row.
#' @keywords internal
.gloc_cat_prior <- function(pr, Fnew) {
  m <- nrow(Fnew); L <- length(pr$levels)
  if (identical(pr$type, "marginal")) {
    P <- matrix(pr$probs, m, L, byrow = TRUE)
  } else {
    pp <- tryCatch(stats::predict(pr$model, newdata = Fnew[, pr$preds, drop = FALSE],
                                  type = "probs"),
                   error = function(e) NULL)   # e.g. a predictor level the fit never saw
    if (is.null(pp)) {
      P <- matrix(pr$probs, m, L, byrow = TRUE)
    } else {
      if (length(pr$fitted_levels) == 2L) pp <- cbind(1 - pp, pp)
      pp <- matrix(pp, nrow = m)
      P <- matrix(0, m, L)
      P[, match(pr$fitted_levels, pr$levels)] <- pp
    }
  }
  if (!is.null(pr$map)) {                      # priors fitted on another level set
    Pm <- matrix(0, m, length(pr$new_levels))
    ok <- !is.na(pr$map)
    Pm[, pr$map[ok]] <- P[, ok, drop = FALSE]
    P <- Pm
  }
  P <- pmax(P, .gloc_cat_floor)
  P / rowSums(P)
}
