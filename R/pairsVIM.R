# --------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         Vienna University of Techology
# --------------------------------------

# workhorse for scatterplot matrices


#' Scatterplot Matrices
#' 
#' Create a scatterplot matrix.
#' 
#' This function is the workhorse for \code{\link{marginmatrix}} and
#' \code{\link{scattmatrixMiss}}.
#' 
#' The graphical parameter \code{oma} will be set unless supplied as an
#' argument.
#' 
#' A panel function should not attempt to start a new plot, since the
#' coordinate system for each panel is set up by \code{pairsVIM}.
#' 
#' @param x a matrix or \code{data.frame}.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, \code{x} needs to have
#' \code{\link{colnames}}). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see \code{col}).
#' @param main,sub main and sub title.
#' @param panel a \code{function(x, y, \dots{})}, which is used to plot the
#' contents of each off-diagonal panel of the display.
#' @param \dots further arguments and graphical parameters to be passed down.
#' \code{par("oma")} will be set appropriately unless supplied (see
#' \code{\link[graphics]{par}}).
#' @param lower,upper separate panel functions to be used below and above the
#' diagonal, respectively.
#' @param diagonal optional \code{function(x, \dots{})} to be applied on the
#' diagonal panels.
#' @param labels either a logical indicating whether labels should be plotted
#' in the diagonal panels, or a character vector giving the labels.
#' @param pos.labels the vertical position of the labels in the diagonal
#' panels.
#' @param cex.labels the character expansion factor to be used for the labels.
#' @param font.labels the font to be used for the labels.
#' @param layout a character string giving the layout of the scatterplot
#' matrix.  Possible values are \code{"matrix"} (a matrix-like layout with the
#' first row on top) and \code{"graph"} (a graph-like layout with the first row
#' at the bottom).
#' @param gap a numeric value giving the distance between the panels in margin
#' lines.
#' @note The code is based on \code{\link[graphics]{pairs}}.  Starting with
#' version 1.4, infinite values are no longer removed before passing the
#' \code{x} and \code{y} vectors to the panel functions.
#' @author Andreas Alfons, modifications by Bernd Prantner
#' @seealso \code{\link{marginmatrix}}, \code{\link{scattmatrixMiss}}
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  \emph{Journal of Advances in Data Analysis
#' and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(sleep, package = "VIM")
#' x <- sleep[, -(8:10)]
#' x[,c(1,2,4,6,7)] <- log10(x[,c(1,2,4,6,7)])
#' pairsVIM(x)
#' 
#' @export pairsVIM
pairsVIM <- function(x, ..., delimiter = NULL, main = NULL, sub = NULL, panel = points, 
        lower = panel, upper = panel, diagonal = NULL, 
        labels = TRUE, pos.labels = NULL, cex.labels = NULL, 
        font.labels = par("font"), layout = c("matrix","graph"), 
        gap = 1) {
    
    # additional arguments
    dots <- list(...)
    nmdots <- names(dots)
    
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
    n <- nrow(x)
    p <- ncol(x)
    if(p < 2) stop("'x' must be at least 2-dimensional")
    # prepare data
    if(is.data.frame(x)) x <- data.matrix(x)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
    if(is.null(colnames(x))) colnames(x) <- defaultNames(p)
    
    # panel functions
    has.lower <- !is.null(lower)
    has.upper <- !is.null(upper)
    has.diag  <- !is.null(diagonal)
    panel <- match.fun(panel)
    if(has.lower && !missing(lower)) lower <- match.fun(lower)
    if(has.upper && !missing(upper)) upper <- match.fun(upper)
    if(has.diag && !missing(diagonal)) diagonal <- match.fun(diagonal)
    
    # use matrix or graph-like layout?
    layout <- match.arg(layout)
    row1attop <- layout == "matrix"
    if(!row1attop) {
        tmp <- has.lower
        has.lower <- has.upper
        has.upper <- tmp
        tmp <- lower
        lower <- upper
        upper <- tmp
    }
    
    # default labels for diagonal panels
    has.labs <- TRUE
    if(is.null(labels)) labels <- colnames(x)
    else if(is.logical(labels)) {
        if(!is.na(labels) && labels) labels <- colnames(x)
        else has.labs <- FALSE
    } else labels <- rep(labels, length.out=p)
    rf <- if(p == 2) 5/6 else 2/3
    if(is.null(cex.labels)) cex.labels <- 1/rf
    if(is.null(pos.labels)) pos.labels <- 0.5 + has.diag/3
    
    # local functions
    initializePlot <- function(..., main, sub, col, bg, pch, cex, lty, lwd) {
        plot.new()
        plot.window(...)
    }
    localLower <- function(..., log, main, sub) lower(...)
    localUpper <- function(..., log, main, sub) upper(...)
    localDiagonal <- function(..., log, main, sub) diagonal(...)
    localAxis <- function(..., log, col, bg, pch, cex, lty, lwd, xpd) {
        axis(..., xpd=NA)
    }
    localBox <- function(..., log, col, bg, pch, cex, lty, lwd) box(...)
    localTitle <- function(..., log, xlab, ylab, outer, 
        cex.main = par("cex.main"), cex.sub = par("cex.sub"), 
        col, bg, pch, cex, lty, lwd) {
        title(..., cex.main=cex.main/rf, cex.sub=cex.sub/rf, outer=TRUE)
    }
    localText <- function(..., log, col, bg, pch, cex, lty, lwd, font) {
        text(..., cex=cex.labels, font=font.labels)
    }
    localStrwidth <- function(..., log, col, bg, pch, cex, lty, lwd, font) {
        strwidth(..., cex=cex.labels, font=font.labels)
    }
    localStrheight <- function(..., log, col, bg, pch, cex, lty, lwd, font) {
        strheight(..., cex=cex.labels, font=font.labels)
    }
    
    # set outer margin
    oma <- if("oma" %in% nmdots) dots$oma else NULL
    if(is.null(oma)) {
        oma <- rep.int(4, 4)
        if(!is.null(main)) oma[3] <- 6
        if(!is.null(sub)) oma[1] <- 5
    }
    op <- par(mfrow=c(p, p), mar = rep.int(gap/2, 4), oma = oma)
    op$usr <- c(0,1,0,1)
    on.exit(par(op))
    
    # check for infinite values
    iInf <- is.infinite(x)
    for(i in 1:p) {
        if(any(iInf[, i])) {
            warning(gettextf("variable '%s' contains infinite values", 
                    colnames(x)[i]))
        }
    }
    # create plot
    for(i in if(row1attop) 1:p else p:1) {
        for(j in 1:p) {
#            ind <- !iInf[, j] & !iInf[, i]
#            xj <- x[ind, j]
#            xi <- x[ind, i]
#            rxj <- if(all(is.na(xj))) c(0,0) else range(xj, na.rm=TRUE)
#            rxi <- if(all(is.na(xi))) c(0,0) else range(xi, na.rm=TRUE)
            xj <- x[, j, drop = FALSE]
            xi <- x[, i, drop = FALSE]
            rxj <- if(any(is.finite(xj))) range(xj, finite=TRUE) else c(0,0)
            rxi <- if(any(is.finite(xi))) range(xi, finite=TRUE) else c(0,0)
            initializePlot(rxj, rxi, ...)
            if(i == j || (i < j && has.lower) || (i > j && has.upper)) {
                mfg <- par("mfg")
				if(imputed) xj <- cbind(xj, imp_var)
                if(i == j) {
                    if(has.diag) localDiagonal(xi, ...)
                    if(has.labs) {
                        par(xlog=FALSE, ylog=FALSE, usr=c(0,1,0,1))
                        if(is.null(cex.labels)) {
                        }
                        lab.width <- localStrwidth(labels[i], ...)
                        lab.height <- localStrheight(labels[i], ...)
                        if(lab.width < 1 && lab.height < 1) {
                            localText(0.5, pos.labels, labels[i], ...)
                        }
                    }
                } else if(i < j) localLower(xj, xi, ...)
                else localUpper(xj, xi, ...)
                if(any(par("mfg") != mfg)) {
                    stop("the panel function made a new plot")
                }
                if(i == 1  && (!(j %% 2) || !has.upper || !has.lower)) {
                    localAxis(1 + 2*row1attop, ...)
                }
                if(i == p && (j %% 2  || !has.upper || !has.lower)) {
                    localAxis(3 - 2*row1attop, ...)
                }
                if(j == 1  && (!(i %% 2) || !has.upper || !has.lower)) {
                    localAxis(2, ...)
                }
                if(j == p && (i %% 2 || !has.upper || !has.lower)) {
                    localAxis(4, ...)
                }
                localBox(...)
            } else par(new=FALSE)
        }
    }
    
    # main and sub title
    localTitle(main=main, sub=sub, ...)
    invisible()
}
