# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner and Matthias Templ
#          Vienna University of Technology
# ----------------------------------------------------------



#' Scatterplot with additional information in the margins
#' 
#' In addition to a standard scatterplot, information about missing/imputed
#' values is shown in the plot margins. Furthermore, imputed values are
#' highlighted in the scatterplot.
#' 
#' Boxplots for available and missing/imputed data, as well as univariate
#' scatterplots for missing/imputed values in one variable are shown in the
#' plot margins.
#' 
#' Imputed values in either of the variables are highlighted in the
#' scatterplot.
#' 
#' Furthermore, the frequencies of the missing/imputed values can be displayed
#' by a number (lower left of the plot). The number in the lower left corner is
#' the number of observations that are missing/imputed in both variables.
#' 
#' @param x a \code{matrix} or \code{data.frame} with two columns.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, \code{x} needs to have
#' \code{\link{colnames}}). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see \code{col}).
#' @param col a vector of length five giving the colors to be used in the plot.
#' The first color is used for the scatterplot and the boxplots for the
#' available data. In case of missing values, the second color is taken for the
#' univariate scatterplots and boxplots for missing values in one variable and
#' the third for the frequency of missing/imputed values in both variables (see
#' \sQuote{Details}). Otherwise, in case of imputed values, the fourth color is
#' used for the highlighting, the frequency, the univariate scatterplot and the
#' boxplots of mputed values in the first variable and the fifth color for the
#' same applied to the second variable. A black color is used for the
#' highlighting and the frequency of imputed values in both variables instead.
#' If only one color is supplied, it is used for the bivariate and univariate
#' scatterplots and the boxplots for missing/imputed values in one variable,
#' whereas the boxplots for the available data are transparent.  Else if two
#' colors are supplied, the second one is recycled.
#' @param alpha a numeric value between 0 and 1 giving the level of
#' transparency of the colors, or \code{NULL}.  This can be used to prevent
#' overplotting.
#' @param pch a vector of length two giving the plot symbols to be used for the
#' scatterplot and the univariate scatterplots.  If a single plot character is
#' supplied, it is used for the scatterplot and the default value will be used
#' for the univariate scatterplots (see \sQuote{Details}).
#' @param cex the character expansion factor to be used for the bivariate and
#' univariate scatterplots.
#' @param numbers a logical indicating whether the frequencies of
#' missing/imputed values should be displayed in the lower left of the plot
#' (see \sQuote{Details}).
#' @param cex.numbers the character expansion factor to be used for the
#' frequencies of the missing/imputed values.
#' @param zeros a logical vector of length two indicating whether the variables
#' are semi-continuous, i.e., contain a considerable amount of zeros.  If
#' \code{TRUE}, only the non-zero observations are used for drawing the
#' respective boxplot.  If a single logical is supplied, it is recycled.
#' @param xlim,ylim axis limits.
#' @param main,sub main and sub title.
#' @param xlab,ylab axis labels.
#' @param ann a logical indicating whether plot annotation (\code{main},
#' \code{sub}, \code{xlab}, \code{ylab}) should be displayed.
#' @param axes a logical indicating whether both axes should be drawn on the
#' plot.  Use graphical parameter \code{"xaxt"} or \code{"yaxt"} to suppress
#' only one of the axes.
#' @param frame.plot a logical indicating whether a box should be drawn around
#' the plot.
#' @param \dots further graphical parameters to be passed down (see
#' \code{\link[graphics]{par}}).
#' @note Some of the argument names and positions have changed with versions
#' 1.3 and 1.4 due to extended functionality and for more consistency with
#' other plot functions in \code{VIM}.  For back compatibility, the argument
#' \code{cex.text} can still be supplied to \code{\dots{}} and is handled
#' correctly.  Nevertheless, it is deprecated and no longer documented.  Use
#' \code{cex.numbers} instead.
#' @author Andreas Alfons, Matthias Templ, modifications by Bernd Prantner
#' @seealso \code{\link{scattMiss}}
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  \emph{Journal of Advances in Data Analysis
#' and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' 
#' data(tao, package = "VIM")
#' data(chorizonDL, package = "VIM")
#' ## for missing values
#' marginplot(tao[,c("Air.Temp", "Humidity")])
#' marginplot(log10(chorizonDL[,c("CaO", "Bi")]))
#' 
#' ## for imputed values
#' marginplot(kNN(tao[,c("Air.Temp", "Humidity")]), delimiter = "_imp")
#' marginplot(kNN(log10(chorizonDL[,c("CaO", "Bi")])), delimiter = "_imp")
#' 
#' 
#' @export marginplot
marginplot <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"), 
        alpha = NULL, pch = c(1,16), cex = par("cex"), 
        numbers = TRUE, cex.numbers = par("cex"), 
        zeros = FALSE, xlim = NULL, ylim = NULL, 
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
        ann = par("ann"), axes = TRUE, frame.plot = axes, ...) {
    # back compatibility
    dots <- list(...)
    if(missing(cex.numbers) && "cex.text" %in% names(dots)) {
        cex.numbers <- dots$cex.text
    }
    # error messages
    if(!(inherits(x, c("data.frame","matrix")))) {
        stop("x must be a data.frame or matrix")
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
    fillbox <- TRUE
    if(length(col) == 0) col <- c("skyblue","red","red4","orange","orange4")
    else if(length(col) == 1) {
        col <- rep.int(col, 5)
        fillbox <- FALSE
    } else if(length(col) == 2 || length(col) == 4) col <- c(col, rep(col[2],3))
    else if(length(col) != 5) col <- c(col[1], rep(col[2:3],2))
    if(length(pch) == 0) pch <- c(1,16)
    else if(length(pch) == 1) pch <- c(pch, 16)
    else if(length(pch) > 2) pch <- pch[1:2]
    if(!is.logical(zeros) || length(zeros) == 0) zeros <- FALSE
    zeros <- rep(sapply(zeros, isTRUE), length.out=2)
    # prepare data
    if(is.data.frame(x)) x <- data.matrix(x)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
    iInf <- apply(x, 1, function(x) any(is.infinite(x)))
    if(any(iInf)) {
        x <- x[!iInf, , drop=FALSE]
		if(imputed) imp_var <- imp_var[!iInf, , drop=FALSE]
        warning("'x' contains infinite values")
    }
    # default axis labels
    if(!is.null(colnames(x))) {
        if(is.null(xlab)) xlab <- colnames(x)[1]
        if(is.null(ylab)) ylab <- colnames(x)[2]
    }
    # semitransparent colors
    colalpha <- alphablend(col, alpha)
    # count missings
    n <- nrow(x)
    if(!imputed) nNA <- c(apply(x, 2, countNA), sum(isNA(x, "all")))
	else nNA <- c(countImp(x, delimiter, imp_var),sum(isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = "all")[["missh"]]))
	# default axis limits
    if(is.null(xlim)) {
        xlim <- if(nNA[1] == n) rep.int(0, 2) else range(x[,1], na.rm=TRUE)
    }
    if(is.null(ylim)) {
        ylim <- if(nNA[2] == n) rep.int(0, 2) else range(x[,2], na.rm=TRUE)
    }
    # initialize plot
    initializeWindow <- function(..., cex.text, 
            col, bg, pch, cex, lty, lwd) {
        plot.new()
        plot.window(...)
    }
    initializeWindow(xlim, ylim, ...)
    # define grid
    # order of graphical parameters matters
    op <- par(c("xlog", "ylog", "plt", "usr", "xaxp", "yaxp"))
    on.exit(par(op))
    pltx <- c(op$plt[2] - diff(op$plt[1:2])/c(1, 1.15/1.05, 1.15), op$plt[2])
    plty <- c(op$plt[4] - diff(op$plt[3:4])/c(1, 1.15/1.05, 1.15), op$plt[4])
    # extend usr coordinates
    gridx <- c(op$usr[1] - c(0.15, 0.05, 0)*diff(op$usr[1:2]), op$usr[2])
    gridy <- c(op$usr[3] - c(0.15, 0.05, 0)*diff(op$usr[3:4]), op$usr[4])
    op$usr <- c(gridx[c(1,4)], gridy[c(1,4)])
    # set plot region for points
    par(plt=c(pltx[3:4], plty[3:4]), usr=c(gridx[3:4], gridy[3:4]))
    # draw points
    localPoints <- function(..., cex.text, log, type, lty, lwd) {
        points(..., type="p")
    }
    localPoints(x[,1], x[,2], cex=cex, col=colalpha[1], pch=pch[1], ...)
    # univariate scatterplots of missings in other variable
    if(!imputed) miss <- is.na(x)
	else {
		tmp <- isImp(x, pos = 1, delimiter = delimiter, imp_var = imp_var, selection = "none")
		miss <- cbind(tmp[["misspos"]],tmp[["missh"]])
		# draw points for imputed values
		localPoints(x[miss[,2],1], x[miss[,2],2], cex=cex, col=colalpha[4], pch=pch[1], ...)
		localPoints(x[miss[,1],1], x[miss[,1],2], cex=cex, col=colalpha[5], pch=pch[1], ...)
		# draw points for imputed values in both variables
		both_imp <- which(apply(miss,1,all))
		col_both <- alphablend("black", alpha)
		localPoints(x[both_imp,1], x[both_imp,2], cex=cex, col=col_both, pch=pch[1], ...)
		
	}
	
	# set plot region for univariate plot of missings along x-axis
    par(xlog=op$xlog, ylog=FALSE, plt=c(pltx[3:4], plty[2:3]), 
        usr=c(gridx[3:4], 0:1))
    box(col="transparent")  # reset clipping region
	if(!imputed) {
		col_scattX <- col_scattY <- colalpha[2]
		col_boxX <- col_boxY <- col[2]
	} else {
		col_scattX <- colalpha[4]
		col_scattY <- colalpha[5]
		col_boxX <- col[4]
		col_boxY <- col[5]
	}
    localPoints(x[miss[,2], 1], rep(0.5, nNA[2]), 
        cex=cex, col=col_scattX , pch=pch[2], ...)
    # set plot region for univariate plot of missings along y-axes
    par(xlog=FALSE, ylog=op$ylog, plt=c(pltx[2:3], plty[3:4]), 
        usr=c(0:1, gridy[3:4]))
    box(col="transparent")  # reset clipping region
    localPoints(rep(0.5, nNA[1]), x[miss[,1], 2], 
        cex=cex, col=col_scattY , pch=pch[2], ...)
    # set plot region for boxplots along x-axis
    par(xlog=op$xlog, ylog=FALSE, plt=c(pltx[3:4], plty[1:2]), 
        usr=c(gridx[3:4], 0:1))
    box(col="transparent")  # reset clipping region
#      any(!is.na(x[!miss[,2],1]))
	if(any(!miss[!miss[,2],1])) {
        xbox <- x[!miss[,2],1]
        if(zeros[1]) xbox <- xbox[xbox != 0]
        boxplot(xbox, boxwex=0.4, col=if(fillbox) col[1], 
            horizontal=TRUE, add=TRUE, at=0.7, axes=FALSE)
    }
    if(any(!miss[miss[,2],1])) {
        xbox <- x[miss[,2],1]
        if(zeros[1]) xbox <- xbox[xbox != 0]
        boxplot(xbox, boxwex=0.4, col=col_boxX, 
            horizontal=TRUE, add=TRUE, at=0.3, axes=FALSE)
    }
    # set plot region for boxplots along y-axis
    par(xlog=FALSE, ylog=op$ylog, plt=c(pltx[1:2], plty[3:4]), 
        usr=c(0:1, gridy[3:4]))
    box(col="transparent")  # reset clipping region
    if(any(!miss[!miss[,1],2])) {
        xbox <- x[!miss[,1],2]
        if(zeros[2]) xbox <- xbox[xbox != 0]
        boxplot(xbox, boxwex=0.4, col=if(fillbox) col[1], 
            add=TRUE, at=0.7, axes=FALSE)
    }
    if(any(!miss[miss[,1],2])) {
        xbox <- x[miss[,1],2]
        if(zeros[2]) xbox <- xbox[xbox != 0]
        boxplot(xbox, boxwex=0.4, col=col_boxY, 
            add=TRUE, at=0.3, axes=FALSE)
    }
    # dot representing missings in both variables
    if(nNA[3]) {
		# set plot region
        par(xlog=FALSE, ylog=FALSE, plt=c(pltx[2:3], plty[2:3]), usr=c(0,1,0,1))
        box(col="transparent")  # reset clipping region
        localPoints(rep.int(0.5, nNA[3]), rep.int(0.5, nNA[3]), 
            cex=cex, col=ifelse(!imputed,colalpha[3],col_both) , pch=pch[2], ...)
    }
    # set plot region for grid lines and numbers
    par(xlog=FALSE, ylog=FALSE, plt=op$plt, usr=c(0,1.15,0,1.15))
    box(col="transparent")  # reset clipping region
    # grid lines
    abline(v=0.15, col="lightgrey")
    abline(v=0.1, col="lightgrey")
    abline(h=0.15, col="lightgrey")
    abline(h=0.1, col="lightgrey")
    # display numbers of missings
    if(isTRUE(numbers)) {
        nNA.width <- strwidth(nNA, cex=cex.numbers)
        nNA.height <- strheight(nNA, cex=cex.numbers)
        if(nNA.width[2] < 0.1 && nNA.height[2] < 0.05) {
            text(0.05, 0.125, labels=nNA[2], col=col_boxX, cex=cex.numbers)
        }
        if(nNA.width[1] < 0.05 && nNA.height[1] < 0.1) {
            text(0.125, 0.05, labels=nNA[1], col=col_boxY, cex=cex.numbers)
        }
        if(nNA.width[3] < 0.1 && nNA.height[3] < 0.1) {
            text(0.05, 0.05, labels=nNA[3], col=ifelse(!imputed,col[3],"black"), cex=cex.numbers)
        }
    }
    # axes and box
    par(op)  # reset plot region
    if(isTRUE(axes)) {
        localAxis <- function(..., cex.text, log, col, bg, pch, cex, lty, lwd) {
            axis(...)
        }
        localAxis(side=1, ...)
        localAxis(side=2, ...)
    }
    if(isTRUE(frame.plot)) {
        localBox <- function(..., cex.text, log, col, bg, pch, cex, lty, lwd) {
            box(...)
        }
        localBox()
    }
    if(isTRUE(ann)) {
        localTitle <- function(..., cex.text, 
                log, col, bg, pch, cex, lty, lwd) {
            title(...)
        }
        localTitle(main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
    }
    invisible()
}
