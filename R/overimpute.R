#' Overimputation: calibration diagnostic for an imputation model
#'
#' Answers "is my imputation model well calibrated?" without ground truth
#' (the analogue of \code{Amelia::overimpute()}, model-agnostic): the
#' \emph{observed} cells of one variable are treated as missing -- fold by
#' fold, so every observed cell is overimputed exactly once -- and imputed
#' with \code{draws} multiple imputations via [vimpute()]. For each cell the
#' observed value is compared with the mean and a \code{level} interval of
#' its draws; a well-calibrated model covers roughly \code{level} of the
#' observed values. Any genuinely missing cells stay missing and are imputed
#' as part of the model, exactly as they would be in a real run.
#'
#' The `...` arguments are passed on to [vimpute()], so the diagnostic runs
#' for any method, spec, or grammar configuration -- e.g.
#' \code{overimpute(dat, "y", method = "robust")} or
#' \code{overimpute(dat, "y", spec = list(y = vs_ranger(num.trees = 300)))}.
#' Note the cost: \code{folds x draws} imputation runs; lower `folds` and
#' `draws` (the defaults are modest) or use a fast method for large data.
#' For a stochastic spread of the draws keep an uncertainty source switched
#' on (the default `uncert = "pmm"`, or `boot`/`uncert`); with purely
#' deterministic settings all draws coincide and the intervals collapse.
#'
#' @param data Dataset with (possibly) missing values.
#' @param variable Single column name: the numeric variable whose observed
#'   cells are overimputed.
#' @param ... Passed on to [vimpute()] (e.g. `method`, `spec`, `uncert`,
#'   `sequential`). `m` is controlled via `draws` and cannot be supplied.
#' @param draws Number of multiple-imputation draws per cell (>= 2).
#' @param folds Number of folds the observed cells are split into (each fold
#'   is set missing and overimputed in one [vimpute()] run; capped at the
#'   number of observed cells).
#' @param level Coverage level of the reported interval (default 0.9).
#' @param seed Optional single number: applied via \code{set.seed()} before
#'   the fold split and the imputation runs.
#' @return A data.frame of class \code{vimpute_overimpute} with one row per
#'   observed cell: \code{row} (row index in \code{data}), \code{observed},
#'   \code{mean}, \code{lower}, \code{upper} (quantiles of the draws at
#'   \code{level}), and \code{covered}. Attributes: \code{variable},
#'   \code{level}, \code{draws}, \code{folds}. \code{print()} reports the
#'   empirical coverage; \code{plot()} draws the observed-vs-imputed
#'   calibration plot with intervals and the 45-degree line.
#' @author Matthias Templ
#' @seealso [vimpute()], [makeMissing()], [evaluation()]
#' @export
#' @examples
#' \dontrun{
#' data(sleep)
#' ov <- overimpute(sleep, "Sleep", method = "robust", sequential = FALSE,
#'                  draws = 5, folds = 5, seed = 1)
#' print(ov)   # empirical coverage of the 90% intervals
#' plot(ov)
#' }
overimpute <- function(data, variable, ..., draws = 5, folds = 5,
                       level = 0.9, seed = NULL) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or data.table.")
  }
  if (!is.character(variable) || length(variable) != 1L ||
      !variable %in% names(data)) {
    stop(sprintf("'variable' must be a single column of 'data'%s.",
                 if (is.character(variable) && length(variable) == 1L) {
                   sprintf(" (not found: %s)", variable)
                 } else ""))
  }
  if (!is.numeric(data[[variable]])) {
    stop(sprintf("'%s' is not numeric; overimputation is defined for numeric variables.",
                 variable))
  }
  if (!is.numeric(draws) || length(draws) != 1L || is.na(draws) || draws < 2 ||
      draws != as.integer(draws)) {
    stop("'draws' must be a single integer >= 2 (multiple-imputation draws per cell).")
  }
  dots <- list(...)
  if ("m" %in% names(dots)) {
    stop("'m' cannot be supplied: the number of imputations is controlled by 'draws'.")
  }
  if (!is.numeric(folds) || length(folds) != 1L || is.na(folds) || folds < 1 ||
      folds != as.integer(folds)) {
    stop("'folds' must be a single positive integer.")
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      level <= 0 || level >= 1) {
    stop("'level' must be strictly between 0 and 1.")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
      stop("'seed' must be a single finite number (or NULL).")
    }
    set.seed(seed)
  }

  obs_idx <- which(!is.na(data[[variable]]))
  if (length(obs_idx) < 2L) {
    stop(sprintf("Too few observed cells in '%s' to overimpute.", variable))
  }
  folds <- min(as.integer(folds), length(obs_idx))
  fold_id <- sample(rep_len(seq_len(folds), length(obs_idx)))

  draw_mat <- matrix(NA_real_, nrow = length(obs_idx), ncol = as.integer(draws))
  input_is_dt <- data.table::is.data.table(data)

  for (f in seq_len(folds)) {
    in_fold <- fold_id == f
    cells <- obs_idx[in_fold]

    dat_f <- if (input_is_dt) data.table::copy(data) else data
    if (input_is_dt) {
      data.table::set(dat_f, i = cells, j = variable, value = NA)
    } else {
      dat_f[cells, variable] <- NA
    }

    mi <- do.call(vimpute, c(
      list(data = dat_f),
      dots,
      list(m = as.integer(draws), imp_var = FALSE, verbose = FALSE)
    ))

    imp_df <- mi$imp[[variable]]
    where_rows <- which(mi$where[, variable])
    pos <- match(cells, where_rows)
    draw_mat[in_fold, ] <- as.matrix(imp_df)[pos, , drop = FALSE]
  }

  alpha <- (1 - level) / 2
  observed <- data[[variable]][obs_idx]
  means  <- rowMeans(draw_mat, na.rm = TRUE)
  lower  <- apply(draw_mat, 1, stats::quantile, probs = alpha, na.rm = TRUE)
  upper  <- apply(draw_mat, 1, stats::quantile, probs = 1 - alpha, na.rm = TRUE)

  out <- data.frame(
    row      = obs_idx,
    observed = observed,
    mean     = means,
    lower    = lower,
    upper    = upper,
    covered  = observed >= lower & observed <= upper
  )
  rownames(out) <- NULL
  structure(out,
            variable = variable, level = level,
            draws = as.integer(draws), folds = folds,
            class = c("vimpute_overimpute", "data.frame"))
}

#' @param x A \code{vimpute_overimpute} object
#' @rdname overimpute
#' @method print vimpute_overimpute
#' @export
print.vimpute_overimpute <- function(x, ...) {
  cat(sprintf("Overimputation diagnostic for '%s'\n", attr(x, "variable")))
  cat(sprintf("  %d observed cells, %d draws each (%d folds)\n",
              nrow(x), attr(x, "draws"), attr(x, "folds")))
  cat(sprintf("  Empirical coverage of the %d%% intervals: %.1f%%\n",
              round(100 * attr(x, "level")), 100 * mean(x$covered)))
  cat(sprintf("  Mean absolute error (observed vs imputed mean): %.4g\n",
              mean(abs(x$observed - x$mean))))
  invisible(x)
}

#' @rdname overimpute
#' @method plot vimpute_overimpute
#' @importFrom graphics segments abline legend
#' @export
plot.vimpute_overimpute <- function(x, ...) {
  col_cov <- "#0072B2"
  col_mis <- "#D55E00"
  cols <- ifelse(x$covered, col_cov, col_mis)
  ylim <- range(c(x$lower, x$upper, x$observed), na.rm = TRUE)
  plot(x$observed, x$mean, ylim = ylim, col = cols, pch = 19,
       xlab = sprintf("observed %s", attr(x, "variable")),
       ylab = sprintf("imputed mean and %d%% interval", round(100 * attr(x, "level"))),
       main = sprintf("Overimputation: %s (coverage %.0f%%)",
                      attr(x, "variable"), 100 * mean(x$covered)),
       ...)
  segments(x$observed, x$lower, x$observed, x$upper, col = cols)
  abline(0, 1, lty = 2)
  legend("topleft", bty = "n", pch = 19, col = c(col_cov, col_mis),
         legend = c("interval covers observed", "interval misses observed"))
  invisible(x)
}
