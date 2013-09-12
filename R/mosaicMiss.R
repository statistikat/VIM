# ---------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         and Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------

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
