# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

## widgets

## pack
#pack <- function(object) {
#    tkpack(object$frame, expand=TRUE, fill="x", padx=3, pady=3, side="left")
#}


# ---------------------------------------


## get state for menu items and dialog elements


# ---------------------------------------

check_data <- function(data) {
  if (inherits(data, "survey.design"))
    stop("support for survey.design objects was removed in VIM",
         " 6.0.0. Use data.frame or data.table instead")
}

## utilities for data
getEscapeChars <- function() {
  c("", "_", "__", "-", " ", "/", "\\", ",")
}

# get vector indicating missings
isNA <- function(x, selection = c("any","all")) {
  selection <- match.arg(selection)
  if(is.null(dim(x))) is.na(x)
  else if(ncol(x) == 1) as.vector(is.na(x))
  else apply(x, 1, function(x) eval(call(selection, is.na(x))))
}

# returns a vector indicating the imputed missings of the current varibale, a vector indicating if the current variable is imputed
# and a vector indicating if there are imputed missings in the other variables
isImp <- function(x, pos, delimiter, imp_var, selection = c("none","any","all")) {
  selection <- match.arg(selection)
  # character vector for possible prefixes for the delimiter
  escape <- getEscapeChars()
  if(is.null(dim(x)) || is.null(dim(imp_var))) list(misspos = imp_var, impp = TRUE, missh = rep(FALSE, NROW(x)))
  else {
    # does the current Variable have imputed missings
    # search escape-vector for possible prefixes
    for(i in 1:length(escape)) {
      indexp <- colnames(imp_var) %in% paste(colnames(x)[pos],delimiter,sep=escape[i])
      # end loop if a match is found
      if(any(indexp))	break
    }
    if(any(indexp)) {
      misspos <- imp_var[,indexp]
      impp <- TRUE
      imp_var <- imp_var[,!indexp, drop = FALSE]
    } else {
      misspos <- rep(FALSE, nrow(x))
      impp <- FALSE
    }
    
    # are there other Variables with missing-indices in the dataset
    # search escape-vector for possible prefixes
    for(i in 1:length(escape)) {
      indexh <- (paste(colnames(x),delimiter,sep=escape[i])) %in% colnames(imp_var)
      # end loop if a match is found
      if(any(indexh)) {
        escape <- escape[i]
        break
      }
    }
    if(any(indexh)) {
      index <- which(indexh)
      tmp <- matrix(nrow = nrow(x), ncol = length(index))
      
      for (i in 1:length(index)) {
        tmp[,i] <- imp_var[,paste(colnames(x)[index[i]],delimiter,sep=escape)]					
      }
      
      if(length(index) > 1 && selection != "none") {
        missh <- apply(tmp, 1, function(tmp) eval(call(selection, tmp)))
      } else {
        missh <- tmp
        colnames(missh) <- colnames(x[,indexh])
      }
    } else {
      missh <- rep(FALSE, nrow(x))
    }
    
    list(misspos = misspos, impp = impp ,missh = missh)
  }
}

#' Count number of infinite or missing values
#' 
#' Count the number of infinite or missing values in a vector.
#' 
#' @aliases countInf countNA
#' @param x a vector.
#' @return  `countInf` returns the number of infinite values in `x`.
#' `countNA` returns the number of missing values in `x`.
#' @author Andreas Alfons
#' @keywords utilities
#' @examples
#' data(sleep, package="VIM")
#' countInf(log(sleep$Dream))
#' countNA(sleep$Dream)
#' @export countInf
# count infinite values
countInf <- function(x) sum(is.infinite(x))#length(which(is.infinite(x)))
#' @export countNA
# count missings
countNA <- function(x) sum(is.na(x))#length(which(is.na(x)))

# count imputed missings
countImp <- function(x, delimiter, imp_var) {
  # character vector for possible prefixes for the delimiter
  escape <- getEscapeChars()
  # search escape-vector for possible prefixes
  for(i in 1:length(escape)) {
    indexh <- (paste(colnames(x),delimiter,sep=escape[i])) %in% colnames(imp_var)
    # end loop if a match is found
    if(any(indexh)) {
      escape <- escape[i]
      break
    }
  }
  tmp <-integer(ncol(x))
  names(tmp) <- colnames(x)
  for ( i in 1:ncol(x)) {
    tmp[i] <- ifelse(indexh[i],length(which(imp_var[,paste(colnames(x)[i],delimiter,sep=escape)])),0)
  }
  tmp
}

# default column names
defaultNames <- function(p) paste("Var", 1:p, sep="")

# is vector categorical or continuous?
is.categorical <- function(x) {
  is.factor(x) || is.character(x) || is.integer(x) || is.logical(x)
}
is.continuous <- function(x) is.numeric(x) && !is.integer(x)

# ---------------------------------------

## utilities for plots

# get plot annotation for variables
getLabel <- function(v) {
  sc <- switch(getVm("scaling"), none="", classical="scaled", 
      MCD="robustly scaled", robust="robustly scaled")
  if(nchar(sc)) paste(v, " (", sc, ")", sep="")
  else v
}

# print out which variables are highlighted
highlightInfo <- function(highlight, selection = c("any","all"), imputed = FALSE) {
  if(!imputed) label <- "missings"
  else label <- "imputed missings"
  
  if(length(highlight) == 0) cat(paste("No ", label, " highlighted.\n",sep=""))
  else if(length(highlight) == 1) {
    cat(paste("Highlighted ", label, " in variable ", highlight, ".\n", sep="'"))
  } else {
    selection <- match.arg(selection)
    hlout <- paste(highlight, collapse="', '")
    cat(paste("Highlighted ", label," in ", selection, 
            " of the variables '", hlout, "'.\n", sep=""))
  }
  invisible()
}

# initialize plot region
initializePlot <- function(xlim, ylim) {
  plot(xlim, ylim, type="n", axes=FALSE, ann=FALSE, xaxs="i", yaxs="i")
}

# locator with error catching
locatorVIM <- function(error = FALSE) {
  pt <- try(locator(1), silent=TRUE)
  if(class(pt) == "try-error" && !error) pt <- NULL
  pt
}

# pretty rotated x-axis labels
#prettyLabels <- function(labels, at, space.vert, cex.axis = NULL) {
#    if(is.null(cex.axis)) cex.axis <- par("cex.axis")
#    # space.vert ... vertical space for axis labels
#    labels.width <- strwidth(labels, units="inches", cex=cex.axis, srt=90)
#    labels.width.ok <- labels.width < space.vert
#    # 'strheight' does not seem to work properly 
#    # with user coordinates and rotated strings
#    labels.height <- strheight(labels, units="inches", cex=cex.axis, srt=90)
#    labels.height <- labels.height/(par("pin")[1]) * diff(par("usr")[1:2])
#    labels.ok <- logical(length(labels))
#    ind <- which(labels.width.ok)
#    labels.ok[ind[1]] <- TRUE
#    r.last <- at[ind[1]] + labels.height[ind[1]]/2
#    for(i in ind[-1]) {
#        if((at[i] - labels.height[i]/2) > r.last) {
#            labels.ok[i] <- TRUE
#            r.last <- at[i] + labels.height[i]/2
#        }
#    }
#    labels.ok
#}
prettyLabels <- function(labels, at, space.vert, 
    cex.axis = NULL, rotate = NULL, xlim = NULL) {
  # space.vert ... vertical space for axis labels
  n <- length(labels)
  if(is.null(cex.axis)) cex.axis <- par("cex.axis")
  # at does not have to be sorted
  ord <- order(at)
  rnk <- rank(at)  # to retrieve correct ok values
  labels <- labels[ord]
  at <- at[ord]
  if(is.null(rotate)) rotate <- rep.int(TRUE, n)
  else rotate <- rotate[ord]
  # width and height of labels
  labels.width <- strwidth(labels, units="inches", cex=cex.axis)
  labels.height <- strheight(labels, units="inches", cex=cex.axis)
  # check vertical space
  labels.vert <- numeric(n)
  labels.vert[rotate] <- labels.width[rotate]
  labels.vert[!rotate] <- labels.height[!rotate]
  labels.vert.ok <- labels.vert < space.vert
  # check horizontal space
  # 'strheight' does not seem to work properly 
  # with user coordinates and rotated strings
  xsize <- par("pin")[1]
  xrange <- diff(par("usr")[1:2])
  labels.horiz <- numeric(n)
  labels.horiz[rotate] <- labels.height[rotate]/xsize * xrange
  labels.horiz[!rotate] <- labels.width[!rotate]/xsize * xrange
  labels.ok <- logical(n)
  ind <- which(labels.vert.ok)
#    labels.ok[ind[1]] <- TRUE
#    r.last <- at[ind[1]] + labels.horiz[ind[1]]/2
#    for(i in ind[-1]) {
#        if((at[i] - labels.horiz[i]/2) > r.last) {
#            labels.ok[i] <- TRUE
#            r.last <- at[i] + labels.horiz[i]/2
#        }
#    }
  if(is.null(xlim)) xlim <- c(-Inf, Inf)
  r.last <- xlim[1]
  for(i in ind) {
    l <- labels.horiz[i]/2
    if((at[i] - l) > r.last && at[i] + l < xlim[2]) {
      labels.ok[i] <- TRUE
      r.last <- at[i] + l
    }
  }
  labels.ok[rnk]
}

# test means for boxplot with missings
testMeans <- function(x, pos = 1, selection = c("any","all")) {
  selection <- match.arg(selection)
  ind <- isNA(x[, -pos], selection)
  x1 <- x[ind, pos]
  x2 <- x[!ind, pos]
  if(length(which(!is.na(x1))) > 1 && length(which(!is.na(x2)))) {
    list(ind=ind, p.v=t.test(x1, x2)$p.v)
  } else list(ind=ind, p.v=NA)
}
