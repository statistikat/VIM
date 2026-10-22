#' Computes the extended Gower distance of two data sets
#'
#' The function gowerD is used by kNN to compute the distances for numerical,
#' factor ordered and semi-continous variables.
#'
#' @param data.x data frame
#' @param data.y data frame
#' @param weights numeric vector providing weights for the observations in x
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
#' @param nMin integer number of values with smallest distance to be returned
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
  maxplus1 <- function(x){
    if(all(is.na(x)))
      return(1)
    else
      return(max(x,na.rm=TRUE)+1)
  }
  min0 <- function(x,na.rm){
    if(all(is.na(x)))
      return(0)
    else
      return(min(x,na.rm=na.rm))
  }
  max1 <- function(x,na.rm){
    if(all(is.na(x)))
      return(1)
    else
      return(max(x,na.rm=na.rm))
  }
  as.numericX <- function(x,...){
    if("character"%in%class(x)){
      x <- as.factor(x)
    }
    return(as.numeric(x,...))
  }
  #weights <- rep(1,ncol(data.x))
  for(i in 1:ncol(data.x)){
    data.x[,i] <- as.numericX(data.x[,i])
    data.y[,i] <- as.numericX(data.y[,i])
  }
  weightind <- order(match(colnames(data.x),c(numerical,factors,orders,mixed)))
  data.x <- data.x[,c(numerical,factors,orders,mixed),drop=FALSE]
  data.y <- data.y[,c(numerical,factors,orders,mixed),drop=FALSE]
  if(length(numerical)>0){
    ##Datensatz durch Range dividieren
    if(methodStand == "range"){
      rmin <- apply(rbind(data.x[,numerical,drop=FALSE],data.y[,numerical,drop=FALSE]),
                        2,min0,na.rm=TRUE)
      rmax <- apply(rbind(data.x[,numerical,drop=FALSE],
                        data.y[,numerical,drop=FALSE]),2,max1,na.rm=TRUE)
    }else if(methodStand == "iqr"){
      rmin <- apply(rbind(data.x[,numerical,drop=FALSE],data.y[,numerical,drop=FALSE]),2,quantile,na.rm=TRUE,
                    probs=.25)
      rmax <- apply(rbind(data.x[,numerical,drop=FALSE],data.y[,numerical,drop=FALSE]),2,quantile,na.rm=TRUE,
                    probs=.75)
    }
    r <- rmax-rmin
    r[is.na(r)] <- 1
    r[r==0] <- 1
    for(i in seq_along(numerical)){
      data.x[,numerical[i]] <- data.x[,numerical[i]]/r[i]
      data.y[,numerical[i]] <- data.y[,numerical[i]]/r[i]
    }
  }
  # Semi-continuous (mixed) columns need the same range/IQR scaling as numeric
  # columns; otherwise their raw magnitude dominates the Gower distance (the C++
  # kernel uses raw |x-y| for two non-spike values, exactly like the numeric
  # branch that IS scaled). The point mass (mixed.constant) is excluded from the
  # scale estimate and scaled along, so the kernel's spike comparison is kept.
  if(length(mixed)>0){
    pooled_mixed <- rbind(data.x[,mixed,drop=FALSE], data.y[,mixed,drop=FALSE])
    for(i in seq_along(mixed)){
      col <- pooled_mixed[,i]
      nonspike <- col[!is.na(col) & col != mixed.constant[i]]
      if(length(nonspike) > 0){
        if(methodStand == "range"){
          rr <- max(nonspike) - min(nonspike)
        }else{
          rr <- as.numeric(diff(quantile(nonspike, probs = c(.25, .75), na.rm = TRUE)))
        }
        if(is.na(rr) || rr == 0) rr <- 1
        data.x[,mixed[i]] <- data.x[,mixed[i]]/rr
        data.y[,mixed[i]] <- data.y[,mixed[i]]/rr
        mixed.constant[i] <- mixed.constant[i]/rr
      }
    }
  }
  justone <- FALSE
  if(nrow(data.y)==1){
    data.y <- rbind(data.y,data.y)
    justone <- TRUE
  }
  # Maximum + 1 for missing values
  # TODO: find better way to handle missing in distance variables
  maxplus1X <- apply(rbind(data.x,data.y),2,maxplus1)
  for(i in 1:ncol(data.x)){
    data.x[is.na(data.x[,i]),i] <- maxplus1X[i]
    data.y[is.na(data.y[,i]),i] <- maxplus1X[i]
  }
  levOrders <- as.numeric(levOrders)
  if(returnIndex){
    out <- gowerDind( as.matrix(data.x), as.matrix(data.y),weights[weightind],
        nthreads = vim_ncores(),
        c(length(numerical),length(factors),length(orders),length(mixed)),
        levOrders,mixed.constant,nMin,as.integer(returnMin))
    out <- list(ind=out$ind,mins=out$min)
    if(justone){
      out$ind <-  out$ind[,1,drop=FALSE]
      out$mins <-  out$mins[,1,drop=FALSE]
    }

  }else{
    out <- gowerd(as.matrix(data.x), as.matrix(data.y),weights[weightind],
        nthreads = vim_ncores(),
        c(length(numerical),length(factors),length(orders),length(mixed)),
        levOrders,mixed.constant)
    if(justone)
      out <-  out$delta[,1,drop=FALSE]
    else
      out <- out$delta
  }
  return(out)
}
