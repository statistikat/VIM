# ---------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         and Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------



#' Mosaic plot with information about missing/imputed values
#' 
#' Create a mosaic plot with information about missing/imputed values.
#' 
#' Mosaic plots are graphical representations of multi-way contingency tables.
#' The frequencies of the different cells are visualized by area-proportional
#' rectangles (tiles).  Additional tiles are be used to display the frequencies
#' of missing/imputed values.  Furthermore, missing/imputed values in a certain
#' variable or combination of variables can be highlighted in order to explore
#' their structure.
#' 
#' @param x a matrix or \code{data.frame}.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, \code{x} needs to have
#' \code{\link{colnames}}). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see \code{col}).
#' @param highlight a vector giving the variables to be used for highlighting.
#' If \code{NULL} (the default), all variables are used for highlighting.
#' @param selection the selection method for highlighting missing/imputed
#' values in multiple highlight variables.  Possible values are \code{"any"}
#' (highlighting of missing/imputed values in \emph{any} of the highlight
#' variables) and \code{"all"} (highlighting of missing/imputed values in
#' \emph{all} of the highlight variables).
#' @param plotvars a vector giving the categorical variables to be plotted.  If
#' \code{NULL} (the default), all variables are plotted.
#' @param col a vector of length three giving the colors to be used for
#' observed, missing and imputed data. If only one color is supplied, the tiles
#' corresponding to observed data are transparent and the supplied color is
#' used for highlighting.
#' @param labels a list of arguments for the labeling function
#' \code{\link[vcd]{labeling_border}}.
#' @param miss.labels either a logical indicating whether labels should be
#' plotted for observed and missing/imputed (highlighted) data, or a character
#' vector giving the labels.
#' @param \dots additional arguments to be passed to \code{\link[vcd]{mosaic}}.
#' @return An object of class \code{"structable"} is returned invisibly.
#' @note This function uses the highly flexible \code{strucplot} framework of
#' package \code{vcd}.
#' @author Andreas Alfons, modifications by Bernd Prantner
#' @seealso \code{\link{spineMiss}}, \code{\link[vcd]{mosaic}}
#' @references Meyer, D., Zeileis, A. and Hornik, K. (2006) The
#' \code{strucplot} framework: Visualizing multi-way contingency tables with
#' \pkg{vcd}. \emph{Journal of Statistical Software}, \bold{17 (3)}, 1--48.
#' 
#' M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete data using
#' visualization tools.  \emph{Journal of Advances in Data Analysis and
#' Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(sleep, package = "VIM")
#' ## for missing values
#' mosaicMiss(sleep, highlight = 4, 
#'     plotvars = 8:10, miss.labels = FALSE)
#' 
#' ## for imputed values
#' mosaicMiss(kNN(sleep), highlight = 4, 
#'     plotvars = 8:10, delimiter = "_imp", miss.labels = FALSE)
#' 
#' @export mosaicMiss
#' @S3method mosaicMiss data.frame
#' @S3method mosaicMiss survey.design
#' @S3method mosaicMiss default
mosaicMiss <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                       plotvars = NULL, col = c("skyblue","red","orange"), 
                       labels = NULL, miss.labels = TRUE, ...) {
  UseMethod("mosaicMiss", x)
}

mosaicMiss.data.frame <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                                  plotvars = NULL, col = c("skyblue","red","orange"), 
                                  labels = NULL, miss.labels = TRUE, ...) {
  mosaicMiss_work(x, delimiter, highlight, selection, plotvars, col, labels, miss.labels, ...)
}

mosaicMiss.survey.design <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                                     plotvars = NULL, col = c("skyblue","red","orange"), 
                                     labels = NULL, miss.labels = TRUE, ...) {
  mosaicMiss_work(x$variables, delimiter, highlight, selection, plotvars, col, labels, miss.labels, ...)
}

mosaicMiss.default <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                               plotvars = NULL, col = c("skyblue","red","orange"), 
                               labels = NULL, miss.labels = TRUE, ...) {
  mosaicMiss_work(as.data.frame(x), delimiter, highlight, selection, plotvars, col, labels, miss.labels, ...)
}

mosaicMiss_work <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
        plotvars = NULL, col = c("skyblue","red","orange"), 
        labels = NULL, miss.labels = TRUE, ...) {
    # initializations and error messages
    if(!(inherits(x, c("data.frame","matrix")))) {
        stop("'x' must be a data.frame or matrix")
    }
	imputed <- FALSE # indicates if there are Variables with missing-index
	## delimiter ##
	if(!is.null(delimiter)) {
		tmp <- grep(delimiter, colnames(x)) # Position of the missing-index
		if(length(tmp) > 0) {
			imp_var <- x[, tmp, drop=FALSE]
			x <- x[, -tmp, drop=FALSE]
			
			if(ncol(x) == 0) stop("Only the missing-index is given")
			if(is.matrix(imp_var) && range(imp_var) == c(0,1)) imp_var <- apply(imp_var,2,as.logical)
			
			if(is.null(dim(imp_var))) {
				if(!is.logical(imp_var)) stop("The missing-index of imputed Variables must be of the type logical")
			} else {
				if(!any(as.logical(lapply(imp_var,is.logical)))) stop("The missing-index of imputed Variables must be of the type logical")	
			}
			imputed <- TRUE
		} else {
			warning("'delimiter' is given, but no missing-index-Variable is found", call. = FALSE)
		}
	}
    px <- ncol(x)
    if(is.null(colnames(x))) colnames(x) <- defaultNames(px)
    if(length(highlight) > px) stop("'highlight' is too long")
    if(length(plotvars) > px) stop("'plotvars' is too long")
    z <- if(is.null(plotvars)) x else x[, plotvars, drop=FALSE]
    if(ncol(z) < 1) stop("no data to be plotted")
    selection <- match.arg(selection)
    if(length(col) == 0) col <- c("skyblue","red","orange")
    else if(length(col) == 1) col <- c("transparent", rep(col,2))
	else if(length(col) == 2) col <- c(col[1], rep(col[2],2))
    else col <- rep(col, length.out=3)
	if(!imputed) col <- col[1:2]
	else col <- col[c(1,3)]
    # define vector indicating NAs
	if(is.null(highlight)) {
		if(!imputed) NAvec <- isNA(x, selection)
		else NAvec <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
	}
	else {
		if(!imputed) NAvec <- isNA(x[, highlight], selection)
		else NAvec <- isImp(x[, highlight, drop=FALSE], pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
	}
    if(is.logical(miss.labels)) {
        ml <- isTRUE(miss.labels)
		if(!imputed) sml <- c("observed", "missing")
		else sml <- c("observed", "imputed")
    } else {
        sml <- rep(ml, length.out=2)
        ml <- TRUE
    }
    NAvec <- factor(ifelse(!NAvec, sml[1], sml[2]), levels=sml, ordered=TRUE)
    z <- cbind(z, .highlight=NAvec)
    # contingency table
    tab <- table(z, useNA="ifany")
    if(is.null(labels)) labels <- list()
    else if(!is.list(labels)) stop("'labels' must be a list")
    if(is.null(labels$labels)) labels$labels <- c(.highlight=ml)
    else {
        tmp <- labels$labels
        tmp[".highlight"] <- ml
        labels$labels <- tmp
    }
    if(is.null(labels$varnames)) labels$varnames <- c(.highlight=FALSE)
    else {
        tmp <- labels$varnames
        tmp[".highlight"] <- FALSE
        labels$varnames <- tmp
    }
    if(is.null(labels$set_labels)) labels$set_labels <- list(.highlight=sml)
    else {
        tmp <- labels$set_labels
        tmp$.highlight <- sml
        labels$set_labels <- tmp
    }
    localMosaic <- function(..., col, labels, labeling, 
            highlighting, highlighting_fill) {
        mosaic(..., labeling=do.call(labeling_border, labels), 
            highlighting=".highlight", highlighting_fill=col)
    }
    localMosaic(tab, col=col, labels=labels, ...)
}
