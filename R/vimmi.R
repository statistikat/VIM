#' @title VIM Multiple Imputations (vimmi)
#' @author Matthias Templ
#' @description S3 class for storing multiple imputations from \code{\link{vimpute}}.
#'   Stores the original data once and only the imputed values per variable per
#'   imputation, for memory efficiency.
#'
#' @details
#' A \code{vimmi} object is returned by \code{\link{vimpute}} when \code{m > 1}.
#' It contains:
#' \describe{
#'   \item{\code{data}}{The original data.frame with NAs intact (stored once).}
#'   \item{\code{imp}}{Named list: for each variable with missings, a data.frame
#'     with \code{nmis} rows and \code{m} columns of imputed values.}
#'   \item{\code{where}}{Logical matrix indicating which cells were imputed.}
#'   \item{\code{m}}{Integer: number of imputations.}
#'   \item{\code{nmis}}{Named integer vector of missing counts per variable.}
#'   \item{\code{method}}{Named list of imputation methods used per variable.}
#'   \item{\code{boot}}{Logical: was bootstrap resampling used?}
#'   \item{\code{uncert}}{Character: uncertainty method used.}
#'   \item{\code{call}}{The original function call.}
#'   \item{\code{tuning_log}}{Tuning report of the (single) tuning run, or NULL.}
#'   \item{\code{chain}}{Per-iteration chain statistics (\code{mean}/\code{var}
#'     arrays \code{[variable, iteration, imputation]}) behind
#'     \code{\link{plot.vimmi}} trace plots.}
#'   \item{\code{seed}}{The seed applied at entry, or NULL.}
#' }
#'
#' Use \code{\link{complete.vimmi}} to extract completed datasets,
#' \code{\link{with.vimmi}} to fit models across imputations (returns a
#' mice-compatible \code{mira}), \code{\link{vim_as_mids}} to convert to a
#' mice \code{mids} object for pooling with \code{mice::pool()}, and
#' \code{\link{plot.vimmi}} for convergence trace plots.
#'
#' @seealso \code{\link{vimpute}}, \code{\link{complete.vimmi}},
#'   \code{\link{with.vimmi}}, \code{\link{vim_as_mids}},
#'   \code{\link{plot.vimmi}}
#' @name vimmi
#' @family imputation methods
#' @examples
#' \dontrun{
#' # Multiple imputation with bootstrap and normal error uncertainty
#' result <- vimpute(sleep, method = "ranger", m = 5,
#'                   boot = TRUE, uncert = "normalerror")
#' print(result)
#' summary(result)
#'
#' # Extract completed datasets
#' d1 <- complete(result, 1)
#' all_d <- complete(result, "all")
#'
#' # Fit models and pool
#' fits <- with(result, lm(Sleep ~ Dream + Span))
#' # mice::pool(fits)  # requires mice
#' }
NULL

#' Extract completed datasets
#'
#' Generic function to extract completed datasets from a multiply imputed object.
#' @param data A multiply imputed object
#' @param ... Additional arguments
#' @export
complete <- function(data, ...) {
  UseMethod("complete")
}

#' Constructor for vimmi objects
#'
#' @param data Original data.frame/data.table with NAs intact
#' @param imp Named list: per variable with missings, a data.frame with
#'   nmis rows and m columns of imputed values
#' @param where Logical matrix indicating which cells were imputed
#' @param m Number of imputations
#' @param nmis Named integer vector of missing counts per variable
#' @param method Named list of methods used per variable
#' @param boot Logical: was bootstrap used?
#' @param uncert Character: uncertainty method used
#' @param call The original function call
#' @param tuning_log Optional tuning report from the (single) tuning run,
#'   shared by all m imputations; NULL when tuning was not requested
#' @param chain Optional per-iteration chain statistics:
#'   \code{list(mean = , var = )} of arrays with dimensions
#'   \code{[variable, iteration, imputation]} holding the mean and variance of
#'   the imputed values of each numeric variable after each sequential
#'   iteration (the basis of \code{\link{plot.vimmi}} trace plots)
#' @param seed The seed applied at entry of the \code{vimpute()} call
#'   (NULL when none was set)
#' @return A \code{vimmi} object
#' @keywords internal
new_vimmi <- function(data, imp, where, m, nmis, method, boot, uncert, call,
                      tuning_log = NULL, chain = NULL, seed = NULL) {
  structure(
    list(
      data   = data,
      imp    = imp,
      where  = where,
      m      = as.integer(m),
      nmis   = nmis,
      method = method,
      boot   = boot,
      uncert = uncert,
      call   = call,
      tuning_log = tuning_log,
      chain  = chain,
      seed   = seed
    ),
    class = "vimmi"
  )
}

#' Extract completed datasets from a vimmi object
#'
#' Reconstructs one or more completed datasets by filling in imputed values
#' from the specified imputation(s).
#'
#' @param data A \code{vimmi} object (produced by \code{\link{vimpute}} with \code{m > 1})
#' @param action Specifies which completed dataset(s) to return:
#'   \itemize{
#'     \item Integer (1..m): return a single completed data.frame for that imputation
#'     \item \code{"all"}: return a named list of all m completed data.frames
#'     \item \code{"long"}: return a single data.frame in long format with
#'       \code{.imp} (imputation number) and \code{.id} (row number) columns
#'   }
#' @param ... Currently unused
#' @return A data.frame, list of data.frames, or long-format data.frame
#' @export
#' @rdname complete.vimmi
#' @examples
#' \dontrun{
#' result <- vimpute(sleep, method = "ranger", m = 5, boot = TRUE, uncert = "normalerror")
#' d1 <- complete(result, 1)        # first completed dataset
#' all_d <- complete(result, "all")  # list of 5 datasets
#' long_d <- complete(result, "long") # long format with .imp column
#' }
complete.vimmi <- function(data, action = 1, ...) {
  x <- data  # S3 generic passes object as first arg named 'data'

  reconstruct_one <- function(mi) {
    completed <- as.data.frame(x$data)
    for (varname in names(x$imp)) {
      imp_df <- x$imp[[varname]]
      if (is.null(imp_df) || ncol(imp_df) < mi) next
      miss_rows <- which(x$where[, varname])
      completed[miss_rows, varname] <- imp_df[[mi]]
    }
    completed
  }

  if (is.numeric(action) && length(action) == 1L) {
    if (!is.finite(action) || action != as.integer(action)) {
      stop("action must be a whole number between 1 and m.")
    }
    if (action < 1 || action > x$m) {
      stop(sprintf("action must be between 1 and %d (m).", x$m))
    }
    return(reconstruct_one(action))
  }

  if (identical(action, "all")) {
    result <- lapply(seq_len(x$m), reconstruct_one)
    names(result) <- paste0("imp", seq_len(x$m))
    return(result)
  }

  if (identical(action, "long")) {
    frames <- lapply(seq_len(x$m), function(mi) {
      d <- reconstruct_one(mi)
      d$.imp <- mi
      d$.id <- seq_len(nrow(d))
      d
    })
    return(do.call(rbind, frames))
  }

  stop("action must be an integer (1..m), 'all', or 'long'.")
}

#' Evaluate an expression across all imputations
#'
#' Applies an expression (typically a model fit) to each completed dataset
#' in a \code{vimmi} object. The return is a mice-compatible \code{mira}
#' object (elements \code{call}, \code{call1}, \code{nmis}, \code{analyses}),
#' so the standard mice pipeline runs unchanged: \code{mice::pool()},
#' \code{summary(mice::pool(fits))}, \code{mice::getfit(fits)}. The mice
#' package is not needed to create the object, only to pool it; for
#' \code{mitools::MIcombine()} pass the fit list \code{fits$analyses}.
#'
#' @param data A \code{vimmi} object
#' @param expr An expression to evaluate, e.g. \code{lm(y ~ x)}
#' @param ... Currently unused
#' @return An object of class \code{mira}: the \code{m} results are in
#'   \code{$analyses} (extract with \code{mice::getfit()})
#' @export
#' @rdname with.vimmi
#' @examples
#' \dontrun{
#' result <- vimpute(sleep, method = "ranger", m = 5, boot = TRUE, uncert = "normalerror")
#' fits <- with(result, lm(Sleep ~ Dream + Span))
#' # Pool with mice:
#' # mice::pool(fits)
#' }
with.vimmi <- function(data, expr, ...) {
  x <- data
  call_expr <- substitute(expr)
  caller_env <- parent.frame()
  fits <- lapply(seq_len(x$m), function(mi) {
    d <- complete.vimmi(x, action = mi)
    eval(call_expr, envir = d, enclos = caller_env)
  })
  # mice-compatible container (mirrors mice::with.mids): pool()/getfit()/
  # summary() work directly; the raw fit list stays reachable as $analyses
  structure(
    list(call = match.call(), call1 = x$call, nmis = x$nmis, analyses = fits),
    class = "mira"
  )
}

#' @param x A \code{vimmi} object
#' @param ... Currently unused
#' @method print vimmi
#' @export
#' @rdname vimmi
print.vimmi <- function(x, ...) {
  cat("Multiply imputed dataset (vimmi)\n")
  cat(sprintf("  Observations: %d\n", nrow(x$data)))
  cat(sprintf("  Variables:    %d\n", ncol(x$data)))
  cat(sprintf("  Imputations:  m = %d\n", x$m))
  cat(sprintf("  Bootstrap:    %s\n", if (x$boot) "yes" else "no"))
  cat(sprintf("  Uncertainty:  %s\n", x$uncert))
  n_imputed <- sum(x$nmis)
  cat(sprintf("  Missing cells: %d across %d variables\n",
              n_imputed, sum(x$nmis > 0)))
  if (sum(x$nmis > 0) > 0) {
    vars_with_na <- x$nmis[x$nmis > 0]
    for (nm in names(vars_with_na)) {
      cat(sprintf("    %s: %d NAs (%s)\n", nm, vars_with_na[[nm]],
                  if (!is.null(x$method[[nm]])) x$method[[nm]] else "?"))
    }
  }
  invisible(x)
}

#' @param object A \code{vimmi} object
#' @method summary vimmi
#' @export
#' @rdname vimmi
summary.vimmi <- function(object, ...) {
  cat("Summary of vimmi object\n")
  cat(sprintf("  m = %d imputations\n", object$m))
  cat(sprintf("  boot = %s, uncert = '%s'\n", object$boot, object$uncert))
  cat("\nPer-variable summary:\n")
  for (varname in names(object$imp)) {
    imp_df <- object$imp[[varname]]
    if (is.null(imp_df)) next
    cat(sprintf("\n  %s (%d missing, method: %s):\n",
                varname, object$nmis[[varname]],
                if (!is.null(object$method[[varname]])) object$method[[varname]] else "?"))
    # Only show numeric summaries for numeric imputed values
    if (all(sapply(imp_df, is.numeric))) {
      imp_mat <- as.matrix(imp_df)
      cat(sprintf("    Mean across imputations:   %.4f\n", mean(imp_mat)))
      cat(sprintf("    SD across imputations:     %.4f\n", sd(as.numeric(imp_mat))))
      cat(sprintf("    Range: [%.4f, %.4f]\n", min(imp_mat), max(imp_mat)))
      imp_means <- colMeans(imp_mat)
      cat(sprintf("    Between-imp variance of means: %.6f\n", var(imp_means)))
    } else {
      cat("    (non-numeric imputed values)\n")
    }
  }
  invisible(object)
}

#' Convert a vimmi object to a mice mids object
#'
#' Converts a \code{vimmi} object to long format and uses
#' \code{mice::as.mids()} to create a proper \code{mids} object.
#' This enables use of \code{mice::pool()}, \code{mice::with.mids()},
#' and other mice infrastructure.
#'
#' \code{vim_as_mids()} is the documented name. \code{as.mids.vimmi()} is the
#' same function under its historical name (kept for backward compatibility):
#' despite the dotted suffix it is a plain function, not an S3 method --
#' \code{mice::as.mids()} is not a generic, so it never dispatches on
#' \code{vimmi} objects.
#'
#' @param x A \code{vimmi} object
#' @param ... Currently unused
#' @return A \code{mids} object (from the mice package)
#' @export
#' @rdname vim_as_mids
#' @examples
#' \dontrun{
#' result <- vimpute(sleep, method = "ranger", m = 5,
#'                   boot = TRUE, uncert = "normalerror")
#' mids_obj <- vim_as_mids(result)
#' # Now use mice infrastructure:
#' # fits <- with(mids_obj, lm(Sleep ~ Dream + Span))
#' # mice::pool(fits)
#' }
vim_as_mids <- function(x, ...) {
  if (!inherits(x, "vimmi")) {
    stop("'x' must be a vimmi object (vimpute() with m > 1).")
  }
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' is required for the mids conversion. Please install it.")
  }

  # Build long format: row 0 = original data, rows 1..m = completed datasets
  original <- as.data.frame(x$data)
  original$.imp <- 0L
  original$.id <- seq_len(nrow(original))

  completed_list <- lapply(seq_len(x$m), function(mi) {
    d <- complete.vimmi(x, action = mi)
    d$.imp <- mi
    d$.id <- seq_len(nrow(d))
    d
  })

  long_df <- do.call(rbind, c(list(original), completed_list))
  mice::as.mids(long_df, .imp = ".imp", .id = ".id")
}

#' @export
#' @rdname vim_as_mids
as.mids.vimmi <- vim_as_mids

#' Convergence trace plots for a vimmi object
#'
#' Draws mice-style convergence diagnostics for the sequential (FCS)
#' imputation behind a \code{vimmi} object: for every numeric variable with
#' imputed cells, the mean and the standard deviation of its imputed values
#' are plotted against the iteration number, one line per imputation. Chains
#' that mix well fluctuate around a common level without trends; systematic
#' drift suggests increasing \code{nseq}.
#'
#' @param x A \code{vimmi} object created by \code{vimpute(m > 1)} in
#'   VIM >= 7.3.0 (older objects carry no chain statistics)
#' @param y Type of diagnostic; currently only \code{"chains"}
#' @param ... Passed on to \code{\link[graphics]{matplot}}
#' @return \code{x}, invisibly
#' @export
#' @method plot vimmi
#' @importFrom graphics matplot
#' @seealso \code{\link{vimmi}}, \code{\link{vimpute}}
#' @examples
#' \dontrun{
#' result <- vimpute(sleep, method = "ranger", m = 5, seed = 1)
#' plot(result)
#' }
plot.vimmi <- function(x, y = c("chains"), ...) {
  y <- match.arg(y)
  if (is.null(x$chain) || is.null(x$chain$mean) ||
      length(dim(x$chain$mean)) != 3L) {
    stop("This vimmi object stores no chain statistics ",
         "(created with VIM < 7.3.0?). Re-run vimpute() to obtain them.")
  }
  vars <- dimnames(x$chain$mean)[[1]]
  traced <- vars[apply(x$chain$mean, 1, function(r) any(is.finite(r)))]
  if (length(traced) == 0L) {
    stop("No numeric imputed variables to trace.")
  }
  iters <- seq_len(dim(x$chain$mean)[2])
  plot_type <- if (length(iters) > 1L) "l" else "p"

  op <- par(mfrow = c(length(traced), 2), mar = c(4, 4, 2, 1))
  on.exit(par(op))
  for (v in traced) {
    matplot(iters, x$chain$mean[v, , ], type = plot_type, lty = 1, pch = 1,
            xlab = "iteration", ylab = "mean of imputed", main = v, ...)
    matplot(iters, sqrt(x$chain$var[v, , ]), type = plot_type, lty = 1, pch = 1,
            xlab = "iteration", ylab = "sd of imputed", main = v, ...)
  }
  invisible(x)
}
