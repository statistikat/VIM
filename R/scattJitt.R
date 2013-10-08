# ----------------------------------------------------------
# Authors: Matthias Templ, Bernd Prantner and Andreas Alfons
#          Vienna University of Technology
# ----------------------------------------------------------



#' Bivariate jitter plot
#' 
#' Create a bivariate jitter plot.
#' 
#' The amount of observed and missing/imputed values is visualized by jittered
#' points.  Thereby the plot region is divided into up to four regions
#' according to the existence of missing/imputed values in one or both
#' variables.  In addition, the amount of observed and missing/imputed values
#' can be represented by a number.
#' 
#' @param x a \code{data.frame} or \code{matrix} with two columns.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, \code{x} needs to have
#' \code{\link{colnames}}). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see \code{col}).
#' @param col a vector of length five giving the colors to be used in the plot.
#' The first color will be used for complete observations, the second/fourth
#' color for missing/imputed values in only one variable, and the third/fifth
#' color for missing/imputed values in both variables.  If only one color is
#' supplied, it is used for all.  Else if two colors are supplied, the second
#' one is recycled.
#' @param alpha a numeric value between 0 and 1 giving the level of
#' transparency of the colors, or \code{NULL}.  This can be used to prevent
#' overplotting.
#' @param cex the character expansion factor for the plot characters.
#' @param col.line the color for the lines dividing the plot region.
#' @param lty the line type for the lines dividing the plot region (see
#' \code{\link[graphics]{par}}).
#' @param lwd the line width for the lines dividing the plot region.
#' @param numbers a logical indicating whether the frequencies of observed and
#' missing/imputed values should be displayed (see \sQuote{Details}).
#' @param cex.numbers the character expansion factor to be used for the
#' frequencies of the observed and missing/imputed values.
#' @param main,sub main and sub title.
#' @param xlab,ylab axis labels.
#' @param axes a logical indicating whether both axes should be drawn on the
#' plot.  Use graphical parameter \code{"xaxt"} or \code{"yaxt"} to suppress
#' just one of the axes.
#' @param frame.plot a logical indicating whether a box should be drawn around
#' the plot.
#' @param labels a vector of length three giving the axis labels for the
#' regions for observed, missing and imputed values (see \sQuote{Details}).
#' @param \dots further graphical parameters to be passed down (see
#' \code{\link[graphics]{par}}).
#' @note Some of the argument names and positions have changed with version 1.3
#' due to extended functionality and for more consistency with other plot
#' functions in \code{VIM}.  For back compatibility, the argument
#' \code{cex.text} can still be supplied to \code{\dots{}} and is handled
#' correctly.  Nevertheless, it is deprecated and no longer documented.  Use
#' \code{cex.numbers} instead.
#' @author Matthias Templ, modifications by Andreas Alfons and Bernd Prantner
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  \emph{Journal of Advances in Data Analysis
#' and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(tao, package = "VIM")
#' ## for missing values
#' scattJitt(tao[, c("Air.Temp", "Humidity")])
#' 
#' ## for imputed values
#' scattJitt(kNN(tao[, c("Air.Temp", "Humidity")]), delimiter = "_imp")
#' 
#' @export scattJitt
scattJitt <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"),
		alpha = NULL, cex = par("cex"), col.line = "lightgrey", 
        lty = "dashed", lwd = par("lwd"), 
        numbers = TRUE, cex.numbers = par("cex"), 
        main = NULL, sub = NULL, xlab = NULL, 
        ylab = NULL, axes = TRUE, frame.plot = axes, 
        labels = c("observed","missing","imputed"), ...) {
    # back compatibility
    dots <- list(...)
    nmdots <- names(dots)
    if(missing(cex.numbers) && "cex.text" %in% nmdots) {
        cex.numbers <- dots$cex.text
    }
    # error messages
    if(!(inherits(x,c("data.frame","matrix")))) {
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
	if(ncol(x) != 2) stop("'x' must be 2-dimensional")
    if(length(col) == 0) col <- c("skyblue","red","red4","orange","orange4")
    else if(length(col) == 1) col <- rep.int(col, 5)
    else if(length(col) == 2 || length(col) == 4) col <- c(col, rep(col[2],3))
	else if(length(col) != 5) col <- c(col[1],rep(col[2:3],2))
    if(length(labels)  == 0) {
		if(!imputed) labels <- c("observed","missing")
		else labels <- c("observed","imputed")
	}
    else if(length(labels) == 1) stop("'labels' must be a vector of length 2 or 3")
    else if(length(labels) > 2) {
		if(!imputed) labels <- labels[1:2]
		else labels <- labels[c(1,3)]
	}
    # semitransparent colors
    if(!is.null(alpha)) col <- alphablend(col, alpha)  
    # get number of complete observations and missings
    nobs <- nrow(na.omit(x))
    if(!imputed) {
		nmissx <- countNA(x[,1])
    	nmissy <- countNA(x[,2])
    	nmissall <- length(which(isNA(x, "all")))
	} else {
		nmiss <- countImp(x, delimiter, imp_var)
		nmissx <- nmiss[1]
		nmissy <- nmiss[2]
		nmissall <- length(which(isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = "all")[["missh"]]))
	}
    z <- cbind(
        x=jitter(rep(c(-1,1,-1,1), c(nobs,nmissx,nmissy,nmissall)), amount=0.7), 
        y=jitter(rep(c(-1,-1,1,1), c(nobs,nmissx,nmissy,nmissall)), amount=0.7))
    if(is.null(colnames(x))) {
        if(is.null(xlab)) xlab <- ""
        if(is.null(ylab)) ylab <- ""
    } else colnames(z) <- colnames(x)
    plot.xaxis <- if(is.null(dots$xaxt)) axes else dots$xaxt != "n" && axes
    plot.yaxis <- if(is.null(dots$yaxt)) axes else dots$yaxt != "n" && axes
    if(nmissx && nmissy) {  # missings in both variables
        if(!imputed) col <- col[1:3]
		else col <- col[c(1,4,5)]
		col <- rep(col,c(nobs, nmissx+nmissy, nmissall))
        localPlot <- function(..., cex.text, type, 
                xlim, ylim, log, axes, frame.plot, 
                panel.first, panel.last, asp) {
            plot(..., xlim=c(-1.7,1.7), ylim=c(-1.7,1.7), axes=FALSE)
        }
        localPlot(z, col=col, cex=cex, main=main, 
            sub=sub, xlab=xlab, ylab=ylab, ...)
        abline(h=0, col=col.line, lty=lty, lwd=lwd)
        abline(v=0, col=col.line, lty=lty, lwd=lwd)
        if(numbers) {  # display numbers
            text(x=rep(-0.1,2), y=c(-0.15,0.15), labels=c(nobs,nmissy), 
                adj=c(1,0.5), cex=cex.numbers)
            text(x=rep(0.1,2), y=c(-0.15,0.15), labels=c(nmissx,nmissall), 
                adj=c(0,0.5), cex=cex.numbers)
        }
        # arguments for axis
        x.at <- c(-0.9,0.9)
        x.labels <- labels
        y.at <- c(-0.9,0.9)
        y.labels=labels
    } else if(nmissx && !nmissy) {  # missings only in x-variable
        if(!imputed) col <- col[1:2]
		else col <- col[c(1,4)]
		col <- rep(col,c(nobs, nmissx))
        localPlot <- function(..., cex.text, type, 
                xlim, ylim, log, axes, frame.plot, 
                panel.first, panel.last, asp) {
            plot(..., xlim=c(-1.7,1.7), ylim=c(-1.7,-0.3), axes=FALSE)
        }
        localPlot(z, col=col, cex=cex, main=main, 
            sub=sub, xlab=xlab, ylab=ylab, ...)
        abline(v=0, col=col.line, lty=lty, lwd=lwd)
        if(numbers) {  # display numbers
            text(x=-0.1, y=-1, labels=nobs, adj=c(1,0.5), cex=cex.numbers)
            text(x=0.1, y=-1, labels=nmissx, adj=c(0,0.5), cex=cex.numbers)
        }
        # arguments for axis
        x.at <- c(-0.9,0.9)
        x.labels <- labels
        y.at <- -1
        y.labels=labels[1]
    } else if(!nmissx && nmissy) {  # missings only in y-variable
		if(!imputed) col <- col[1:2]
		else col <- col[c(1,4)]
		col <- rep(col,c(nobs, nmissy))
        localPlot <- function(..., cex.text, type, 
                xlim, ylim, log, axes, frame.plot, 
                panel.first, panel.last, asp) {
            plot(..., xlim=c(-1.7,-0.3), ylim=c(-1.7,1.7), axes=FALSE)
        }
        localPlot(z, col=col, cex=cex, main=main, 
            sub=sub, xlab=xlab, ylab=ylab, ...)
        abline(h=0, col=col.line, lty=lty, lwd=lwd)
        if(numbers) {  # display numbers
            text(x=-1, y=-0.15, labels=nobs, adj=c(0.5,0.5), cex=cex.numbers)
            text(x=-1, y=0.15, labels=nmissy, adj=c(0.5,0.5), cex=cex.numbers)
        }
        # arguments for axis
        x.at <- -1
        x.labels <- labels[1]
        y.at <- c(-0.9,0.9)
        y.labels=labels
    } else {  # no missings
    	col <- col[1]
        localPlot <- function(..., cex.text, type, 
            xlim, ylim, log, axes, frame.plot, 
            panel.first, panel.last, asp) {
            plot(..., xlim=c(-1.7,-0.3), ylim=c(-1.7,-0.3), axes=FALSE)
        }
        localPlot(z, col=col, cex=cex, main=main, 
            sub=sub, xlab=xlab, ylab=ylab, ...)
        # arguments for axis
        x.at <- -1
        x.labels <- labels[1]
        y.at <- -1
        y.labels=labels[1]
    }
    localAxis <- function(..., cex.text, type, xlim, 
            ylim, log, ann, panel.first, panel.last,
            asp, col, bg, pch, cex, lty, lwd) {
        axis(..., lty=0)
    }
    if(plot.xaxis) localAxis(side=1, at=x.at, labels=x.labels, ...)
    if(plot.yaxis) localAxis(side=2, at=y.at, labels=y.labels, ...)
    if(frame.plot) {
        localBox <- function(..., cex.text, type, xlim, 
                ylim, log, ann, panel.first, panel.last,
                asp, col, bg, pch, cex, lty, lwd) {
            box(...)
        }
        localBox()
    }
    invisible()
}
