#' Computes the extended Gower distance of two data sets
#'
#' The function gowerD is used by kNN to compute the distances for numerical,
#' factor ordered and semi-continous variables.
#'
#' @param data.x data frame
#' @param data.y data frame
#' @param weights finite nonnegative numeric weights, one per input column,
#' with a positive finite total over the selected distance variables
#' @param numerical names of numerical variables
#' @param factors names of factor variables
#' @param orders names of ordered variables
#' @param mixed names of mixed variables
#' @param levOrders vector of positive integer level counts, one for each
#' variable in `orders`. Ordered factors must have identical levels in both
#' inputs, including unused levels. Numeric ordinal variables must contain
#' integer ranks from 1 to the corresponding level count, or NA.
#' @param mixed.constant 	vector with length equal to the number of semi-continuous variables specifying the point of the semi-continuous distribution with non-zero probability
#' @param returnIndex logical if TRUE return the index of the minimum distance
#' @param nMin positive integer number of values with smallest distance to be returned, no greater than the number of rows in data.x
#' @param returnMin logical if the computed distances for the indices should be returned
#' @param methodStand character either "range" or "iqr", iqr is more robust for outliers
#' @details Numerical and semi-continuous (mixed) variables are standardised by
#' their range (or IQR, see `methodStand`) before the distance is computed; for
#' mixed variables the point mass (`mixed.constant`) is excluded from the range
#' estimate and scaled along with the data.
#'
#' Missing values in a distance variable are replaced by a sentinel (the pooled
#' column maximum plus one) after standardisation. Two records both missing the
#' same variable therefore have distance 0 on it, and a missing-vs-observed pair
#' contributes up to about 2 (rather than a variable's nominal maximum of 1).
#' This sentinel convention departs from Gower's (1971) omit-and-renormalise
#' rule and biases nearest-neighbour selection towards records that share the
#' same missingness pattern.
#'
#' Ordinal distances use the full declared scale, including unused levels.
#' A single-level ordinal variable contributes zero between observed values;
#' missing values still follow the sentinel convention described above.
#'
#' The distance computation is parallelised with OpenMP where R was built with
#' OpenMP support. The number of threads is `getOption("VIM.ncores")`; if the
#' option is unset, at most 2 threads are used while the package is checked
#' (`R CMD check` sets `_R_CHECK_LIMIT_CORES_`; CRAN policy allows at most two
#' cores) and OpenMP's default -- typically all cores, or `OMP_NUM_THREADS` --
#' otherwise. Results do not depend on the number of threads.
#'
#' returnIndex=FALSE: a numerical matrix n x m with the computed distances
#' returnIndex=TRUE: a named list with "ind" containing the requested indices and "mins" the computed distances
#' @examples
#' data(sleep)
#' # all variables used as numerical
#' gowerD(sleep)
#'
#' # split in numerical an
#' gowerD(sleep, numerical = c("BodyWgt", "BrainWgt", "NonD", "Dream", "Sleep", "Span", "Gest"),
#'   orders = c("Pred","Exp","Danger"), levOrders = c(5,5,5))
#'
#' # as before but only returning the index of the closest observation
#' gowerD(sleep, numerical = c("BodyWgt", "BrainWgt", "NonD", "Dream", "Sleep", "Span", "Gest"),
#'   orders = c("Pred","Exp","Danger"), levOrders = c(5,5,5), returnIndex = TRUE)
#' @export



gowerD <- function(data.x, data.y = data.x,
                   weights = rep(1, ncol(data.x)),
                   numerical = colnames(data.x),
                   factors = vector(),
                   orders = vector(),
                   mixed = vector(),
                   levOrders  = vector(),
                   mixed.constant = rep(0, length(mixed)),
                   returnIndex=FALSE,
                   nMin=1L,
                   returnMin=FALSE,
                   methodStand="range") {
  stopifnot(length(methodStand)==1&&methodStand%in%c("range", "iqr"))
  if (!is.data.frame(data.x) || !is.data.frame(data.y) ||
      ncol(data.x) != ncol(data.y) || anyDuplicated(names(data.x)) ||
      anyDuplicated(names(data.y)) || !setequal(names(data.x), names(data.y)))
    stop("'data.x' and 'data.y' must be data frames with matching unique column names")
  if (!is.numeric(weights) || length(weights) != ncol(data.x) ||
      anyNA(weights) || any(!is.finite(weights)) || any(weights < 0) ||
      !is.finite(sum(weights)) || sum(weights) <= 0)
    stop("'weights' must contain one finite nonnegative value per input column with a positive finite total")
  resolve_columns <- function(cols) {
    if (!length(cols)) return(character())
    if (is.numeric(cols)) {
      if (anyNA(cols) || any(!is.finite(cols)) ||
          any(cols != floor(cols) | cols < 1 | cols > ncol(data.x)))
        stop("Distance variable indices must identify existing columns")
      return(names(data.x)[cols])
    }
    if (!is.character(cols) || anyNA(cols) || any(!cols %in% names(data.x)))
      stop("Distance variables must identify existing columns")
    cols
  }
  numerical <- resolve_columns(numerical)
  factors <- resolve_columns(factors)
  orders <- resolve_columns(orders)
  mixed <- resolve_columns(mixed)
  distance_columns <- c(numerical, factors, orders, mixed)
  if (!length(distance_columns) || anyDuplicated(distance_columns))
    stop("Distance variables must be nonempty and belong to exactly one type")
  if (length(mixed)) {
    if (!is.numeric(mixed.constant) || length(mixed.constant) != length(mixed) ||
        anyNA(mixed.constant) || any(!is.finite(mixed.constant)))
      stop("'mixed.constant' must contain one finite value per mixed variable")
  } else {
    # Historically callers could pass an unused scalar constant.
    mixed.constant <- numeric()
  }
  if (returnIndex)
    validate_neighbor_count(nMin, nrow(data.x), "nMin")
  if ((!is.numeric(levOrders) && length(levOrders) > 0L) ||
      length(levOrders) != length(orders) ||
      anyNA(levOrders) || any(!is.finite(levOrders)) ||
      any(levOrders < 1 | levOrders != floor(levOrders))) {
    stop("'levOrders' must contain one positive integer level count per ordinal variable")
  }
  for (i in seq_along(orders)) {
    x <- data.x[[orders[i]]]
    y <- data.y[[orders[i]]]
    if (is.factor(x) || is.factor(y)) {
      if (!is.ordered(x) || !is.ordered(y) ||
          !identical(levels(x), levels(y))) {
        stop("Ordinal variable '", orders[i],
             "' must have identical ordered factor levels in both inputs")
      }
      if (nlevels(x) != levOrders[i]) {
        stop("'levOrders' does not match the declared levels of ordinal variable '", orders[i], "'")
      }
    } else {
      if (!is.numeric(x) || !is.numeric(y)) {
        stop("Ordinal variable '", orders[i], "' must be an ordered factor or numeric ranks")
      }
      ranks <- c(x, y)
      ranks <- ranks[!is.na(ranks)]
      if (any(!is.finite(ranks)) || any(ranks != floor(ranks) | ranks < 1 | ranks > levOrders[i])) {
        stop("Ordinal variable '", orders[i], "' must contain integer ranks between 1 and ", levOrders[i])
      }
    }
  }
  counts <- lengths(list(numerical, factors, orders, mixed))
  prepared <- prepare_gower_columns(data.x, data.y, distance_columns, counts,
                                    mixed.constant, methodStand)
  weightind <- match(distance_columns, names(data.x))
  nthreads <- vim_ncores()
  if (returnIndex) {
    out <- gowerDind(prepared$x, prepared$y, weights[weightind], counts,
                    as.numeric(levOrders), prepared$mixed.constant,
                    nMin, as.integer(returnMin), nthreads = nthreads)
    # The single-recipient result historically omits an absent mins entry.
    if (nrow(data.y) == 1L && is.null(out$min)) return(list(ind = out$ind))
    list(ind = out$ind, mins = out$min)
  } else {
    gowerd(prepared$x, prepared$y, weights[weightind], counts,
           as.numeric(levOrders), prepared$mixed.constant, nthreads = nthreads)$delta
  }
}

prepare_gower_columns <- function(data.x, data.y, columns, counts, mixed.constant,
                                  methodStand) {
  x_matrix <- matrix(0, nrow(data.x), length(columns))
  y_matrix <- matrix(0, nrow(data.y), length(columns))
  numeric_end <- counts[1L]
  factor_end <- numeric_end + counts[2L]
  ordered_end <- factor_end + counts[3L]

  for (i in seq_along(columns)) {
    x <- data.x[[columns[i]]]
    y <- data.y[[columns[i]]]
    nominal <- i > numeric_end && i <= factor_end
    if (nominal && (is.character(x) || is.character(y) ||
                    is.factor(x) || is.factor(y))) {
      # Both sides must use the same codes for nominal labels.
      x <- as.character(x)
      y <- as.character(y)
      labels <- unique(c(x[!is.na(x)], y[!is.na(y)]))
      x <- match(x, labels)
      y <- match(y, labels)
    } else {
      if ("character" %in% class(x)) x <- as.factor(x)
      if ("character" %in% class(y)) y <- as.factor(y)
      x <- as.numeric(x)
      y <- as.numeric(y)
    }

    is_mixed <- i > ordered_end
    if (i <= numeric_end || is_mixed) {
      pooled <- c(x, y)
      if (is_mixed) {
        # Exclude the point mass when estimating the scale.
        mixed_index <- i - ordered_end
        point_mass <- mixed.constant[mixed_index]
        pooled <- pooled[!is.na(pooled) & pooled != point_mass]
      }
      scale <- 1
      if (any(!is.na(pooled))) {
        bounds <- if (methodStand == "range") range(pooled, na.rm = TRUE) else
          quantile(pooled, probs = c(.25, .75), na.rm = TRUE, names = FALSE)
        span <- bounds[2L] - bounds[1L]
        if (!is.na(span) && span != 0) scale <- span
      }
      x <- x / scale
      y <- y / scale
      if (is_mixed) mixed.constant[mixed_index] <- point_mass / scale
    }

    missing_x <- is.na(x)
    missing_y <- is.na(y)
    if (any(missing_x) || any(missing_y)) {
      # Use the pooled maximum after scaling. An entirely missing column
      # uses 1, even when one side has no rows.
      sentinel <- if (all(missing_x) && all(missing_y)) 1 else
        max(x, y, na.rm = TRUE) + 1
      x[missing_x] <- sentinel
      y[missing_y] <- sentinel
    }
    x_matrix[, i] <- x
    y_matrix[, i] <- y
  }
  list(x = x_matrix, y = y_matrix, mixed.constant = mixed.constant)
}

validate_neighbor_count <- function(n, available = Inf, name = "k") {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n < 1 || n != floor(n) || n > .Machine$integer.max)
    stop("'", name, "' must be a single positive integer")
  if (n > available)
    stop("'", name, "' exceeds the number of eligible donors (", available, ")")
}
