# ------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ------------------------------------------



#' Histogram with information about missing/imputed values
#' 
#' Histogram with highlighting of missing/imputed values in other variables by
#' splitting each bin into two parts.  Additionally, information about
#' missing/imputed values in the variable of interest is shown on the right
#' hand side.
#' 
#' If more than one variable is supplied, the bins for the variable of interest
#' will be split according to missingness/number of imputed missings in the
#' additional variables.
#' 
#' If `only.miss=TRUE`, the missing/imputed values in the variable of
#' interest are visualized by one bar on the right hand side.  If additional
#' variables are supplied, this bar is again split into two parts according to
#' missingness/number of imputed missings in the additional variables.
#' 
#' Otherwise, a small barplot consisting of two bars is drawn on the right hand
#' side.  The first bar corresponds to observed values in the variable of
#' interest and the second bar to missing/imputed values.  Since these two bars
#' are not on the same scale as the main barplot, a second y-axis is plotted on
#' the right (if `axes=TRUE`).  Each of the two bars are again split into
#' two parts according to missingness/number of imputed missings in the
#' additional variables.  Note that this display does not make sense if only
#' one variable is supplied, therefore `only.miss` is ignored in that
#' case.
#' 
#' If `interactive=TRUE`, clicking in the left margin of the plot results
#' in switching to the previous variable and clicking in the right margin
#' results in switching to the next variable.  Clicking anywhere else on the
#' graphics device quits the interactive session.  When switching to a
#' categorical variable, a barplot is produced rather than a histogram.
#' 
#' @param x a vector, matrix or `data.frame`.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, `x` needs to have
#' [colnames()]). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see `col`).
#' @param pos a numeric value giving the index of the variable of interest.
#' Additional variables in `x` are used for highlighting.
#' @param selection the selection method for highlighting missing/imputed
#' values in multiple additional variables.  Possible values are `"any"`
#' (highlighting of missing/imputed values in *any* of the additional
#' variables) and `"all"` (highlighting of missing/imputed values in
#' *all* of the additional variables).
#' @param breaks either a character string naming an algorithm to compute the
#' breakpoints (see [hist()]), or a numeric value giving the number
#' of cells.
#' @param right logical; if `TRUE`, the histogram cells are right-closed
#' (left-open) intervals.
#' @param col a vector of length six giving the colors to be used. If only one
#' color is supplied, the bars are transparent and the supplied color is used
#' for highlighting missing/imputed values.  Else if two colors are supplied,
#' they are recycled.
#' @param border the color to be used for the border of the cells.  Use
#' `border=NA` to omit borders.
#' @param main,sub main and sub title.
#' @param xlab,ylab axis labels.
#' @param axes a logical indicating whether axes should be drawn on the plot.
#' @param only.miss logical; if `TRUE`, the missing/imputed values in the
#' first variable are visualized by a single bar.  Otherwise, a small barplot
#' is drawn on the right hand side (see \sQuote{Details}).
#' @param miss.labels either a logical indicating whether label(s) should be
#' plotted below the bar(s) on the right hand side, or a character string or
#' vector giving the label(s) (see \sQuote{Details}).
#' @param interactive a logical indicating whether the variables can be
#' switched interactively (see \sQuote{Details}).
#' @param \dots further graphical parameters to be passed to
#' [graphics::title()] and [graphics::axis()].
#' @return a list with the following components:
#' - breaks the breakpoints.
#' - counts the number of observations in each cell.
#' - missings the number of highlighted observations in each cell.
#' - mids the cell midpoints.
#' @note Some of the argument names and positions have changed with version 1.3
#' due to extended functionality and for more consistency with other plot
#' functions in `VIM`.  For back compatibility, the arguments
#' `axisnames` and `names.miss` can still be supplied to
#' \code{\dots{}} and are handled correctly.  Nevertheless, they are deprecated
#' and no longer documented.  Use `miss.labels` instead.
#' @author Andreas Alfons, Bernd Prantner
#' @seealso [spineMiss()], [barMiss()]
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  *Journal of Advances in Data Analysis
#' and Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @family plotting functions
#' @examples
#' 
#' data(tao, package = "VIM")
#' ## for missing values
#' x <- tao[, c("Air.Temp", "Humidity")]
#' histMiss(x)
#' histMiss(x, only.miss = FALSE)
#' 
#' ## for imputed values
#' x_IMPUTED <- kNN(tao[, c("Air.Temp", "Humidity")])
#' histMiss(x_IMPUTED, delimiter = "_imp")
#' histMiss(x_IMPUTED, delimiter = "_imp", only.miss = FALSE)
#' 
#' @export
histMiss <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                     breaks = "Sturges", right = TRUE, 
                     col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                     border = NULL, main = NULL, sub = NULL, 
                     xlab = NULL, ylab = NULL, axes = TRUE, 
                     only.miss = TRUE, miss.labels = axes, 
                     interactive = TRUE, ...) {
  check_data(x)
  x <- as.data.frame(x)

	imputed <- FALSE # indicates if there are Variables with missing-index
	# initializations and error messages
	if(is.null(dim(x))) {  # vector
		# call barMiss if the plot variable is categorial
		if(is.categorical(x)) {
			barMiss(x, delimiter=delimiter, pos=pos, selection=selection, col=col, 
					border=border, main=main, sub=sub, xlab=xlab, ylab=ylab,
					axes=axes, only.miss=only.miss, miss.labels=miss.labels,
					interactive=interactive, ...)
			return(invisible(1))
		}
		n <- length(x)
		p <- 1
		if(n == 0) stop("'x' must have positive length")
	} else {  # matrix or data.frame
		if(!(inherits(x, c("data.frame","matrix")))) { 
			stop("'x' must be a data.frame or matrix")
		}
		
		# call barMiss if the plot variable is categorial
		if(is.categorical(x[, pos])) {
			barMiss(x, delimiter=delimiter, pos=pos, selection=selection, col=col, 
					border=border, main=main, sub=sub, xlab=xlab, ylab=ylab,
					axes=axes, only.miss=only.miss, miss.labels=miss.labels,
					interactive=interactive, ...)
			return(invisible(1))
		}
		
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
		if(n == 0) stop("'x' has no rows")
		else if(p == 0) stop("'x' has no columns")
		if(is.null(colnames(x))) colnames(x) <- defaultNames(p)
	}
	
	if(p == 1) {
		only.miss <- TRUE
		interactive <- FALSE
	} else {
		if((!is.numeric(pos)) || (length(pos) != 1) || (p < pos)) {
			stop("'pos' must be an integer specifying one column of 'x' and must be lesser than the number of colums of 'x'")
		}
		selection <- match.arg(selection)
	}
	if(!is.character(breaks) && 
			!(is.numeric(breaks) && length(breaks) == 1)) {
		stop("'breaks' must be a character string naming an algorithm ", 
				"or a single number giving the number of cells")
	}
	if(length(col) == 0) col <- c("skyblue","red","skyblue4","red4","orange","orange4")
	else if(length(col) == 1) col <- c(rep.int(c("transparent", col), 2),rep.int(col,2))
	else if(length(col) == 3 || length(col) == 5) col <- rep.int(col[1:2], 3)
	else if(length(col) != 6) col <- rep(col, length.out=6)
	localAxis <- function(..., names.arg, axisnames, cex.names, names.miss) {
		axis(...)
	}
	localTitle <- function(..., names.arg, axisnames, cex.names, names.miss) {
		title(...)
	}
	
	# back compatibility
	dots <- list(...)
	nmdots <- names(dots)
	if(missing(miss.labels)) {
		if("axisnames" %in% nmdots) {
			if(dots$axisnames) {
				if("names.miss" %in% nmdots) miss.labels <- dots$names.miss
				else miss.labels <- TRUE
			} else miss.labels <- FALSE
		} else if("names.miss" %in% nmdots) miss.labels <- dots$names.miss
	}
	
	# workhorse to create plot
	createPlot <- function(main=NULL, sub=NULL, xlab=NULL, ylab=NULL) {
		# prepare data
		if(is.null(dim(x))) xpos <- as.numeric(x)
		else if(p == 1) {
			xpos <- as.numeric(x[,1])
			if(is.null(xlab)) xlab <- colnames(x)  # default x-axis label
		} else {
			xpos <- as.numeric(x[, pos])  # plot variable
			xh <- x[, -pos, drop=FALSE]  # highlight variables
			if(is.null(xlab)) xlab <- colnames(x)[pos]  # default x-axis label
		}

		if(p == 2 && is.null(ylab)) {  # default y-axis label
			if(!imputed) ylab <- paste("missing/observed in", colnames(x)[-pos])
			else ylab <- paste("imputed/observed in", colnames(x)[-pos])
		}
				
		impp <- FALSE # indicates if the current variable has imputed missings
		# get missings/imputed missings and plot limits
		if(!imputed) { # histMiss
			misspos <- isNA(xpos)
		} else { # histImp
			tmp <- isImp(x, pos = pos, delimiter = delimiter, imp_var = imp_var, selection = selection)
			misspos <- tmp[["misspos"]]
			impp <- tmp[["impp"]]
			missh <- tmp[["missh"]]
		}
		missposf <- factor(ifelse(misspos, 1, 0), levels=0:1)
			
		if(p == 1) ct <- table(missposf)[2]  # number of missings
		else {
			if(!imputed) missh <- isNA(xh, selection) # histMiss
			
			misshf <- factor(ifelse(missh, 1, 0), levels=1:0)
			ct <- table(misshf, missposf)  # contingency table for missings
			ct[2,] <- ct[1,] + ct[2,]  # y-coordinates for rectangles
			if(only.miss) ct <- ct[,2]
		}
		
		# check for infinite values
		iInf <- is.infinite(xpos)  # indicates infinite values
		allNAInf <- all(misspos | iInf)
		if(any(iInf)) {
			if(is.null(dim(x))) cnw <- "'x'" 
			else cnw <- paste("variable '", colnames(x)[pos], "'", sep="")
			warning(cnw, " contains infinite values")
		}
		if(allNAInf) {
			r <- list(counts=0)
			br <- 0:1
			n <- 5
		} else {
			r <- hist(xpos, breaks=breaks, right=right, plot=FALSE)  # histogram
			br <- range(r$breaks)
			n <- length(r$counts)
			# dummy plot with original x-axis limits to retrieve tickmarks
			plot(br, 0:1, type="n", ann=FALSE, axes=FALSE)
			par(new=TRUE)
			xaxp <- par("xaxp")  # retrieve tickmarks
		}
		# extend x-axis limits
		h <- br[2] - br[1]
		if(only.miss) {
			xlim <- c(br[1], br[2]+(0.08+1/n)*h)
			ylim <- if(all(iInf)) c(0,1) else c(0, max(r$counts, ct))
		} else {
			xlim <- c(br[1], br[2]+0.155*h)
			ylim <- c(0, max(r$counts))
		}
		if(allNAInf) {
			plot(xlim, ylim, type="n", ann=FALSE, axes=FALSE)
			if(axes && only.miss) localAxis(side=2, ...)  # y-axis
		} else {
			plot(r, col=col[1], border=border, main="", sub="", xlim=xlim, 
					ylim=ylim, xlab="", ylab="", axes=FALSE)
			if(p > 1 && any(missh)) {  # add histogram for missings
				if(imputed) color <- col[5]
				else color <- col[2]
				rr <- hist(xpos[missh], breaks=r$breaks, 
						right=right, col=color, border=border, add=TRUE)
				if(imputed) {
				  indices <- which(is.na(x[,2]) & imp_var == TRUE)
				  rr1 <- hist(xpos[indices], breaks=r$breaks, 
				             right=right, col=col[2], border=border, add=TRUE)
				}
			}
			else if(p == 1 && impp == TRUE && any(misspos)) {
				rr <- hist(xpos[misspos], breaks=r$breaks, 
						right=right, col=col[5], border=border, add=TRUE)
				
			}
			else rr <- list(counts=rep.int(0, length(r$counts)))
			if(axes) {
				localAxis(side=1, at=axTicks(side=1, axp=xaxp), ...)  # x-axis
				localAxis(side=2, ...)  # y-axis
			}
		}
		localTitle(main, sub, xlab, ylab, ...)  # plot annotation
		abline(v=br[2]+0.04*h, col="lightgrey")
		
		# additional information about missings
		miss.axes <- TRUE
		if(is.logical(miss.labels)) {
			if(!is.na(miss.labels) && miss.labels) miss.labels <- NULL
			else miss.axes <- FALSE
		}
		if(miss.axes) {
			dots$side <- 1
			if(is.null(dots$line)) dots$line <- par("mgp")[3]
			dots$lty <- 0
			if(is.null(dots$las)) dots$las <- 3
		}
		if(only.miss) {  # one bar for missings in first variable
			xleft <- br[2] + 0.08*h
			xright <- xlim[2]
			if(p == 1) {
				rect(xleft, 0, xright, ct, col=col[3], border=border, xpd=TRUE)
			} else {
				if(!imputed) color <- col[4:3]
				else color <- col[c(6,3)]
				rect(rep(xleft, 2), c(0, ct[1]), rep(xright, 2), ct, 
						col=color, border=border, xpd=TRUE)
			}
			if(miss.axes) {
				dots$at <- xleft+(xright-xleft)/2
				if(is.null(miss.labels)) {
					if(!imputed) miss.labels <- "missing"
					else miss.labels <- "imputed"
				}
				else miss.labels <- rep(miss.labels, length.out=1)
				dots$labels <- miss.labels
			}
		} else {  # stacked barplot for observed/missing in first variable
			usr <- par("usr")
			on.exit(par(usr=usr))  # reset user coordinates on exit
			# set up new plot region
			par(new=TRUE)
			plot(xlim, c(0, max(ct[2,])), type="n", ann=FALSE, axes=FALSE)
			zero <- br[2]+0.08*h
			xleft <- zero + c(0,0,1.5,1.5)*0.03*h
			ybottom <- c(0,ct[1,1],0,ct[1,2])
			xright <- zero + c(1,1,2.5,2.5)*0.03*h
			ytop <- ct
			if(!imputed) color <- col[c(2,1,4,3)]
			else color <- col[c(5,1,6,3)]
			########################################################
			
			rect(xleft, ybottom, xright, ytop, 
			     col=color, border=border, xpd=TRUE)
			## still missings
			if(length(indices) > 0 & imputed) {
			  sum_miss <- length(indices)
			  xleft1 <- xleft[1]
			  ybottom1 <- ybottom[1]
			  xright1 <- xright[1]
			  ytop1 <- sum_miss
			  color1 <- col[2]
			  rect(xleft1,ybottom1,xright1,ytop1,col=color1,border=border,xpd=TRUE)
			}
			########################################################
			if(miss.axes) {
				dots$at <- zero + c(0.5,2)*0.03*h
				if(is.null(miss.labels)) {
					if(!imputed) miss.labels <- c("observed","missing")
					else miss.labels <- c("observed","imputed")
				} 
				else miss.labels <- rep(miss.labels, length.out=2)
				dots$labels <- miss.labels
			}
			if(axes) localAxis(side=4, ...)
		}
		if(miss.axes && dots$las %in% 2:3) {
			space.vert <- 
					(par("oma")[1]+par("mar")[1]-dots$line-par("mgp")[2])*par("csi")
			ok <- prettyLabels(dots$labels, dots$at, space.vert, dots$cex.axis)
			if(any(ok)) {
				dots$at <- dots$at[ok]
				dots$labels <- dots$labels[ok]
			} else miss.axes <- FALSE
		}
		if(miss.axes) do.call(localAxis, dots)
		
		if(allNAInf) return() 
		else return(list(breaks=r$breaks, counts=r$counts, 
							missings=rr$counts, mids=r$mids))
	}
	result <- createPlot(main, sub, xlab, ylab)

	# interactive features
	interactiveDevices <- c("X11cairo","quartz","windows")
	dev <- names(dev.cur())
	
	if(interactive && any(!is.na(charmatch(interactiveDevices, dev)))) {
		cat(paste("\nClick in in the left margin to switch to the previous",
						"variable or in the right margin to switch to the next",
						"variable.\n"))
		cat(paste("To regain use of the VIM GUI and the R console,",
						"click anywhere else in the graphics window.\n\n"))
		usr <- par("usr")
		pt <- locatorVIM()
		while(!is.null(pt) && (pt$x < usr[1] || pt$x > usr[2])) {
			if(pt$x < usr[1]) pos <- if(pos == 1) p else (pos - 1) %% p
			else pos <- if(pos == p-1) p else (pos + 1) %% p
			#result <- createPlot()
			result <- 
					if(is.categorical(x[, pos])) {
						barMiss(if(imputed) cbind(x,imp_var) else x, delimiter = delimiter, pos=pos, selection=selection, col=col, 
								border=border, axes=axes, only.miss=only.miss, 
								miss.labels=miss.labels, interactive=FALSE, ...) 
					} else createPlot()
			usr <- par("usr")
			pt <- locatorVIM()
		}
	}
	
	invisible(result)
}
