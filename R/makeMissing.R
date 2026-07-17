#' Generate MCAR/MAR/MNAR missingness in complete data
#'
#' Amputation generator for simulation studies (the variable-wise counterpart
#' of \code{mice::ampute()}): takes complete data, sets a controlled share of
#' cells to \code{NA} under a chosen missingness mechanism, and returns the
#' amputed data together with an \code{attr(., "where")} indicator matrix that
#' plugs directly into \code{\link{evaluation}()}, \code{\link{nrmse}()} and
#' \code{\link{pfc}()}:
#'
#' \preformatted{
#'   amp <- makeMissing(dat, prop = 0.2, mechanism = "MAR")
#'   imp <- vimpute(amp)
#'   evaluation(dat, imp, m = attr(amp, "where"))
#' }
#'
#' Mechanisms (applied per target variable, each receiving exactly
#' \code{round(prop * nrow(data))} missing cells):
#' \describe{
#'   \item{\code{"MCAR"}}{cells are drawn uniformly at random.}
#'   \item{\code{"MAR"}}{the probability of a cell going missing grows with a
#'     weighted score of the \emph{other} (observed) variables: rows are
#'     drawn with probabilities \code{plogis(z)} where \code{z} is the
#'     standardized weighted sum of the driver columns. Default drivers: all
#'     numeric columns except the target, with equal weights; use
#'     \code{weights} to select drivers and directions (negative weights make
#'     low driver values predictive of missingness).}
#'   \item{\code{"MNAR"}}{as \code{"MAR"}, but the score is the target's
#'     \emph{own} standardized values, so high values are more often missing
#'     (a \code{weights} entry named after the target flips or scales the
#'     direction). Numeric targets only.}
#' }
#'
#' With several target variables the scores are computed on the complete
#' input, so each target's mechanism conditions on the pre-amputation values
#' of its drivers.
#'
#' @param data A complete data.frame or data.table (no missing values).
#' @param prop Proportion of cells set to missing per target variable,
#'   strictly between 0 and 1.
#' @param mechanism One of \code{"MCAR"} (default), \code{"MAR"},
#'   \code{"MNAR"}.
#' @param vars Character vector of target variables to receive missing
#'   values. Default: all columns.
#' @param weights Optional named numeric vector. For \code{"MAR"}: the driver
#'   columns and their weights (numeric columns, excluding the target). For
#'   \code{"MNAR"}: an entry named after a target scales/flips its own-value
#'   score. Ignored for \code{"MCAR"}.
#' @param seed Optional single number: applied via \code{set.seed()} before
#'   drawing, for reproducible amputation.
#' @return The amputed data, classed like the input (data.frame in,
#'   data.frame out; data.table in, data.table out), with attributes
#'   \code{"where"} (logical indicator matrix of the introduced missings),
#'   \code{"mechanism"} and \code{"prop"}.
#' @author Matthias Templ
#' @seealso [evaluation()], [vimpute()], [kNN()]
#' @export
#' @examples
#' data(sleep)
#' complete_rows <- na.omit(sleep[, c("BodyWgt", "BrainWgt", "Sleep", "Span")])
#' amp <- makeMissing(complete_rows, prop = 0.2, mechanism = "MAR",
#'                    vars = "Sleep", seed = 1)
#' colSums(is.na(amp))
#' imp <- kNN(amp, imp_var = FALSE)
#' evaluation(complete_rows, imp, m = attr(amp, "where"))
makeMissing <- function(data, prop = 0.1,
                        mechanism = c("MCAR", "MAR", "MNAR"),
                        vars = NULL, weights = NULL, seed = NULL) {
  mechanism <- match.arg(mechanism)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or data.table.")
  }
  if (anyNA(data)) {
    stop("'data' must be complete: makeMissing() generates missingness in ",
         "complete data (existing NAs would make the mechanism ambiguous).")
  }
  if (!is.numeric(prop) || length(prop) != 1L || is.na(prop) ||
      prop <= 0 || prop >= 1) {
    stop("'prop' must be a single number strictly between 0 and 1.")
  }
  if (is.null(vars)) {
    vars <- names(data)
  }
  unknown <- setdiff(vars, names(data))
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown variable name(s) in 'vars': %s.",
                 paste(unknown, collapse = ", ")))
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights) || is.null(names(weights)) ||
        any(!nzchar(names(weights)))) {
      stop("'weights' must be a named numeric vector.")
    }
    unknown_w <- setdiff(names(weights), names(data))
    if (length(unknown_w) > 0L) {
      stop(sprintf("Unknown variable name(s) in 'weights': %s.",
                   paste(unknown_w, collapse = ", ")))
    }
  }
  if (mechanism == "MNAR") {
    non_num <- vars[!vapply(data[, vars, drop = FALSE], is.numeric, logical(1))]
    if (length(non_num) > 0L) {
      stop(sprintf(
        "MNAR requires numeric target variables (own-value mechanism); not numeric: %s.",
        paste(non_num, collapse = ", ")))
    }
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
      stop("'seed' must be a single finite number (or NULL).")
    }
    set.seed(seed)
  }

  n <- nrow(data)
  k <- round(prop * n)
  if (k < 1L) {
    stop(sprintf("'prop' = %g introduces no missing cells for %d rows.", prop, n))
  }

  # standardized score column (mean 0, sd 1; constant columns score 0)
  z_score <- function(x) {
    x <- as.numeric(x)
    s <- stats::sd(x)
    if (!is.finite(s) || s == 0) return(rep(0, length(x)))
    (x - mean(x)) / s
  }

  # row scores for one target: NULL means uniform (MCAR)
  score_for <- function(v) {
    if (mechanism == "MCAR") {
      return(NULL)
    }
    if (mechanism == "MNAR") {
      w <- if (!is.null(weights) && v %in% names(weights)) weights[[v]] else 1
      return(w * z_score(data[[v]]))
    }
    # MAR: weighted sum over driver columns (numeric, not the target)
    if (!is.null(weights)) {
      if (v %in% names(weights)) {
        stop(sprintf(
          "MAR drivers must be other variables ('%s' cannot drive its own missingness; use mechanism = \"MNAR\").",
          v))
      }
      drivers <- names(weights)
      non_num <- drivers[!vapply(data[, drivers, drop = FALSE], is.numeric, logical(1))]
      if (length(non_num) > 0L) {
        stop(sprintf("MAR driver(s) must be numeric: %s.",
                     paste(non_num, collapse = ", ")))
      }
      w <- weights
    } else {
      drivers <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], v)
      if (length(drivers) == 0L) {
        stop(sprintf(
          "MAR needs at least one numeric driver column besides '%s'; supply 'weights' or add numeric variables.",
          v))
      }
      w <- setNames(rep(1, length(drivers)), drivers)
    }
    rowSums(vapply(drivers, function(d) w[[d]] * z_score(data[[d]]),
                   numeric(n)))
  }

  where <- matrix(FALSE, nrow = n, ncol = ncol(data),
                  dimnames = list(NULL, names(data)))
  for (v in vars) {
    score <- score_for(v)
    idx <- if (is.null(score)) {
      sample.int(n, k)
    } else {
      sample.int(n, k, prob = stats::plogis(z_score(score)))
    }
    where[idx, v] <- TRUE
  }

  input_is_dt <- data.table::is.data.table(data)
  out <- if (input_is_dt) data.table::copy(data) else data
  for (v in vars) {
    idx <- which(where[, v])
    if (input_is_dt) {
      data.table::set(out, i = idx, j = v, value = NA)
    } else {
      out[idx, v] <- NA
    }
  }

  attr(out, "where") <- where
  attr(out, "mechanism") <- mechanism
  attr(out, "prop") <- prop
  out
}
