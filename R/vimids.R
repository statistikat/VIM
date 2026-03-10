#' @title Multiply Imputed Dataset (VIM)
#' @description S3 class for storing multiple imputations from \code{\link{vimpute}}.
#'   Inspired by mice's \code{mids} class: stores the original data once and only
#'   the imputed values per variable per imputation, for memory efficiency.
#' @name vimids
#' @family imputation methods
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

#' Constructor for vimids objects
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
#' @return A \code{vimids} object
#' @keywords internal
new_vimids <- function(data, imp, where, m, nmis, method, boot, uncert, call) {
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
      call   = call
    ),
    class = "vimids"
  )
}

#' Extract completed datasets from a vimids object
#'
#' Reconstructs one or more completed datasets by filling in imputed values
#' from the specified imputation(s).
#'
#' @param data A \code{vimids} object (produced by \code{\link{vimpute}} with \code{m > 1})
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
#' @rdname complete.vimids
#' @examples
#' \dontrun{
#' result <- vimpute(sleep, method = "ranger", m = 5, boot = TRUE, uncert = "normalerror")
#' d1 <- complete(result, 1)        # first completed dataset
#' all_d <- complete(result, "all")  # list of 5 datasets
#' long_d <- complete(result, "long") # long format with .imp column
#' }
complete.vimids <- function(data, action = 1, ...) {
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
#' in a \code{vimids} object. The results can be pooled using
#' \code{mice::pool()} or \code{mitools::MIcombine()}.
#'
#' @param data A \code{vimids} object
#' @param expr An expression to evaluate, e.g. \code{lm(y ~ x)}
#' @param ... Currently unused
#' @return A list of length \code{m} containing the result of evaluating
#'   \code{expr} on each completed dataset
#' @export
#' @rdname with.vimids
#' @examples
#' \dontrun{
#' result <- vimpute(sleep, method = "ranger", m = 5, boot = TRUE, uncert = "normalerror")
#' fits <- with(result, lm(Sleep ~ Dream + Span))
#' # Pool with mice:
#' # mice::pool(fits)
#' }
with.vimids <- function(data, expr, ...) {
  x <- data
  call_expr <- substitute(expr)
  lapply(seq_len(x$m), function(mi) {
    d <- complete.vimids(x, action = mi)
    eval(call_expr, envir = d, enclos = parent.frame(2L))
  })
}

#' @method print vimids
#' @export
#' @rdname vimids
print.vimids <- function(x, ...) {
  cat("Multiply imputed dataset (vimids)\n")
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

#' @method summary vimids
#' @export
#' @rdname vimids
summary.vimids <- function(object, ...) {
  cat("Summary of vimids object\n")
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

#' Convert a vimids object to a mice mids object
#'
#' Converts a \code{vimids} object to long format and uses
#' \code{mice::as.mids()} to create a proper \code{mids} object.
#' This enables use of \code{mice::pool()}, \code{mice::with.mids()},
#' and other mice infrastructure.
#'
#' @param x A \code{vimids} object
#' @param ... Currently unused
#' @return A \code{mids} object (from the mice package)
#' @export
#' @rdname as.mids.vimids
as.mids.vimids <- function(x, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' is required for as.mids(). Please install it.")
  }

  # Build long format: row 0 = original data, rows 1..m = completed datasets
  original <- as.data.frame(x$data)
  original$.imp <- 0L
  original$.id <- seq_len(nrow(original))

  completed_list <- lapply(seq_len(x$m), function(mi) {
    d <- complete.vimids(x, action = mi)
    d$.imp <- mi
    d$.id <- seq_len(nrow(d))
    d
  })

  long_df <- do.call(rbind, c(list(original), completed_list))
  mice::as.mids(long_df, .imp = ".imp", .id = ".id")
}
