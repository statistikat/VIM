###########################################################
##Authors: Andreas Alfons, Bernd Prantner, Matthias Templ
##          and Daniel Schopfhauser
##          Vienna University of Technology
###########################################################



#' Aggregations for missing/imputed values
#' 
#' Calculate or plot the amount of missing/imputed values in each variable and
#' the amount of missing/imputed values in certain combinations of variables.
#' 
#' Often it is of interest how many missing/imputed values are contained in
#' each variable.  Even more interesting, there may be certain combinations of
#' variables with a high number of missing/imputed values.
#' 
#' If `combined` is `FALSE`, two separate plots are drawn for the
#' missing/imputed values in each variable and the combinations of
#' missing/imputed and non-missing values. The barplot on the left hand side
#' shows the amount of missing/imputed values in each variable.  In the
#' *aggregation plot* on the right hand side, all existing combinations of
#' missing/imputed and non-missing values in the observations are visualized.
#' Available, missing and imputed data are color coded as given by `col`.
#' Additionally, there are two possibilities to represent the frequencies of
#' occurrence of the different combinations.  The first option is to visualize
#' the proportions or frequencies by a small bar plot and/or numbers.  The
#' second option is to let the cell heights be given by the frequencies of the
#' corresponding combinations. Furthermore, variables may be sorted by the
#' number of missing/imputed values and combinations by the frequency of
#' occurrence to give more power to finding the structure of missing/imputed
#' values.
#' 
#' If `combined` is `TRUE`, a small version of the barplot showing
#' the amount of missing/imputed values in each variable is drawn on top of the
#' aggregation plot.
#' 
#' The graphical parameter `oma` will be set unless supplied as an
#' argument.
#' 
#' @aliases aggr plot.aggr print.aggr summary.aggr print.summary.aggr
#' @param x a vector, matrix or `data.frame`.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, `x` needs to have
#' [colnames()]). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see `col`).
#' 
#' @return for `aggr`, a list of class `"aggr"` containing the
#' following components:
#' - x the data used.
#' - combinations a character vector representing the combinations of
#' variables.
#' -  count the frequencies of these combinations.
#' -  percent the percentage of these combinations.
#' -  missings a `data.frame` containing the amount of
#' missing/imputed values in each variable.
#' -  tabcomb the indicator matrix for the combinations of variables.
#' @note Some of the argument names and positions have changed with version 1.3
#' due to extended functionality and for more consistency with other plot
#' functions in `VIM`.  For back compatibility, the arguments `labs`
#' and `names.arg` can still be supplied to \code{\dots{}} and are handled
#' correctly.  Nevertheless, they are deprecated and no longer documented.  Use
#' `ylabs` and `labels` instead.
#' @author Andreas Alfons, Matthias Templ, modifications for displaying imputed
#' values by Bernd Prantner
#' @seealso [print.aggr()], [summary.aggr()]
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  *Journal of Advances in Data Analysis
#' and Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(sleep, package="VIM")
#' ## for missing values
#' a <- aggr(sleep)
#' a
#' summary(a)
#' 
#' ## for imputed values
#' sleep_IMPUTED <- kNN(sleep)
#' a <- aggr(sleep_IMPUTED, delimiter="_imp")
#' a
#' summary(a)
#' 
#' @export 
aggr <- function(x, delimiter = NULL, plot = TRUE, ...) {
  UseMethod("aggr", x)
}

#' @rdname aggr
#' @export 
aggr.data.frame <- function(x, delimiter = NULL, plot = TRUE, ...) {
  aggr_work(x, delimiter, plot, ...)
}

#' @rdname aggr
#' @export 
aggr.survey.design <- function(x, delimiter = NULL, plot = TRUE, ...) {
  aggr_work(x$variables, delimiter, plot, ...)
}

#' @rdname aggr
#' @export 
aggr.default <- function(x, delimiter = NULL, plot = TRUE, ...) {
  aggr_work(as.data.frame(x), delimiter, plot, ...)
}

#' @rdname aggr
#' @export 
aggr_work <- function(x, delimiter = NULL, plot = TRUE, ...) {
	
	imputed <- FALSE # indicates if there are Variables with missing-index
	if(is.null(dim(x))) {
		n <- length(x)
		imp <- FALSE
		cn <- defaultNames(1)
		nNA <- countNA(x)
		tab <- table(as.numeric(is.na(x)))
		tabcomb <- as.matrix(as.integer(names(tab)))
	} else {
		## delimiter ##
		if(!is.null(delimiter)) {
			tmp <- grep(delimiter, colnames(x)) # Position of the missing-index
			if(length(tmp) > 0) {
				imp_var <- x[, tmp, drop=FALSE]
				x <- x[, -tmp, drop=FALSE]
				
				if(ncol(x) == 0) stop("Only the missing-index is given")
				if(is.matrix(imp_var) && range(imp_var) == c(0,1)) imp_var <- apply(imp_var,2,as.logical)
				
				if(is.null(dim(imp_var))) {
					if(!is.logical(imp_var)) stop("The missing-index of imputed variables must be of the type logical")
				} else {
					if(!any(as.logical(lapply(imp_var,is.logical)))) stop("The missing-index of imputed variables must be of the type logical")	
				}
				imputed <- TRUE
			} else {
				warning("'delimiter' is given, but no missing-index variable is found", call. = FALSE)
			}
		}
		n <- nrow(x)
		cn <- colnames(x)
		if(is.null(cn)) cn <- defaultNames(ncol(x))
		nNA <- apply(x, 2, countNA)
		imp <- rep(FALSE,ncol(x))
		# Combine imputed and Missing Values
		if(imputed) {
			nNA_imp <-countImp(x, delimiter, imp_var)
			imp <- nNA_imp > 0
			nNA <- nNA + nNA_imp
		}
		# Combine imputed and Missing Values
		tmp <- ifelse(is.na(x), 1, 0)  # 'ifelse' does not omit 'dim' attribute
		if(imputed) {
			tmp_imp <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = "none")[["missh"]]
			tmp_imp <- ifelse(tmp_imp, 2,0)
			tmp[,colnames(tmp_imp)] <- tmp_imp
		}	
		tab <- table(apply(tmp, 1, paste, collapse=":"))
		tabcomb <- sapply(names(tab), 
			function(x) as.integer(unlist(strsplit(x, ":", fixed=TRUE))), 
			USE.NAMES=FALSE)
		tabcomb <- if(is.null(dim(tabcomb))) as.matrix(tabcomb) else t(tabcomb)
	}
	miss <- data.frame(Variable=cn, Count=nNA, stringsAsFactors=FALSE)
	count <- as.integer(tab)  # frequency of combinations
	res <- list(x=x, combinations=names(tab), count=count, 
		percent=count*100/n, missings=miss, tabcomb=tabcomb, imputed = imp)
	class(res) <- "aggr"
	if(plot) {
		plot(res, ...)
		invisible(res)
	} else res
}

# plot method
# TODO: interactive sorting of variables or combinations
# FIXME: sortVars = TRUE bei nur missings
#' @rdname aggr
#' @method plot aggr
#' @param plot a logical indicating whether the results should be plotted (the
#' default is `TRUE`).
#' @param col a vector of length three giving the colors to be used for
#' observed, missing and imputed data. If only one color is supplied, it is
#' used for missing and imputed data and observed data is transparent. If only
#' two colors are supplied, the first one is used for observed data and the
#' second color is used for missing and imputed data.
#' @param bars a logical indicating whether a small barplot for the frequencies
#' of the different combinations should be drawn.
#' @param numbers a logical indicating whether the proportion or frequencies of
#' the different combinations should be represented by numbers.
#' @param prop a logical indicating whether the proportion of missing/imputed
#' values and combinations should be used rather than the total amount.
#' @param combined a logical indicating whether the two plots should be
#' combined.  If `FALSE`, a separate barplot on the left hand side shows
#' the amount of missing/imputed values in each variable.  If `TRUE`, a
#' small version of this barplot is drawn on top of the plot for the
#' combinations of missing/imputed and non-missing values.  See
#' \dQuote{Details} for more information.
#' @param varheight a logical indicating whether the cell heights are given by
#' the frequencies of occurrence of the corresponding combinations.
#' @param only.miss a logical indicating whether the small barplot for the
#' frequencies of the combinations should only be drawn for combinations
#' including missing/imputed values (if `bars` is `TRUE`).  This is
#' useful if most observations are complete, in which case the corresponding
#' bar would dominate the barplot such that the remaining bars are too
#' compressed.  The proportion or frequency of complete observations (as
#' determined by `prop`) is then represented by a number instead of a bar.
#' @param border the color to be used for the border of the bars and
#' rectangles.  Use `border=NA` to omit borders.
#' @param sortVars a logical indicating whether the variables should be sorted
#' by the number of missing/imputed values.
#' @param sortCombs a logical indicating whether the combinations should be
#' sorted by the frequency of occurrence.
#' @param ylabs if `combined` is `TRUE`, a character string giving
#' the y-axis label of the combined plot, otherwise a character vector of
#' length two giving the y-axis labels for the two plots.
#' @param axes a logical indicating whether axes should be drawn.
#' @param labels either a logical indicating whether labels should be plotted
#' on the x-axis, or a character vector giving the labels.
#' @param cex.lab the character expansion factor to be used for the axis
#' labels.
#' @param cex.axis the character expansion factor to be used for the axis
#' annotation.
#' @param cex.numbers the character expansion factor to be used for the
#' proportion or frequencies of the different combinations
#' @param gap if `combined` is `FALSE`, a numeric value giving the
#' distance between the two plots in margin lines.
#' @param \dots for `aggr` and `TKRaggr`, further arguments and
#' graphical parameters to be passed to [plot.aggr()].  For
#' `plot.aggr`, further graphical parameters to be passed down.
#' `par("oma")` will be set appropriately unless supplied (see
#' [graphics::par()]).
#' @export 
plot.aggr <- function(x, col = c("skyblue","red","orange"), bars = TRUE, 
		numbers = FALSE, prop = TRUE, combined = FALSE, varheight = FALSE, 
		only.miss = FALSE, border = par("fg"), sortVars = FALSE,
		sortCombs = TRUE, ylabs = NULL, axes = TRUE, labels = axes,
		cex.lab = 1.2, cex.axis = par("cex"), cex.numbers = par("cex"),
		gap = 4, 
		#interactive=TRUE, 
		...) {
	
	# back compatibility
	dots <- list(...)
	nmdots <- names(dots)
	if(missing(ylabs) && "labs" %in% nmdots) ylabs <- dots$labs
	if(missing(labels) && "names.arg" %in% nmdots) labels <- dots$names.arg
	# are there imputed variables in the dataset 
	imputed <- x$imputed
	# are there imputed and missing variables in the dataset
	miss_imp <- any(x$missings[,2] > 0 & !imputed)
	imputed <- any(x$imputed)
	
	# error messages and initializations
	if(length(col) == 0) col <- c("skyblue","red","orange")
	else if(length(col) == 1) col <- c("transparent", rep(col,2))
	else if(length(col) == 2) col <- rep(col,1:2)
	else if(length(col) != 3) col <-  rep(col, length.out=3)
    if(!is.logical(prop) || length(prop) == 0) prop <- TRUE
    if(combined) {
		prop <- rep.int(isTRUE(prop[1]), 2)
        if(varheight) bars <- FALSE
#		numbers <- FALSE
		if(is.null(ylabs)) ylabs <- "Combinations"
	} else {
		if(length(prop) == 1) prop <- rep.int(isTRUE(prop), 2)
		else prop <- c(isTRUE(prop[1]), isTRUE(prop[2]))
		if(varheight) {
			bars <- FALSE
			numbers <- FALSE
		}
		if(is.null(ylabs)) {
			if(!imputed) ylabs <- if(prop[1]) "Proportion of missings" else "Number of missings"
			else if (!miss_imp) ylabs <- if(prop[1]) "Proportion of imputed missings" else "Number of imputed missings"
			else ylabs <- if(prop[1]) "Proportion of missings or imputed missings" else "Number of missings or imputed missings"
			ylabs <- c(ylabs, "Combinations")
		} else if(length(ylabs) != 2) stop("'ylabs' must be a vector of length 2")
	}
	
	# dimensions of the data
	if(is.null(dim(x$x))) {
		nx <- length(x$x)
		px <- 1
		if(nx == 0) stop("'x' must have positive length")
	} else {
		nx <- nrow(x$x)
		px <- ncol(x$x)
		if(nx == 0) stop("'x' has no rows")
		else if(px == 0) stop("'x' has no columns")
	}
	
	# graphical parameters for resetting
	op <- par(no.readonly=TRUE)
	on.exit(par(op))
	
	# some parameters
	if(is.null(dots$oma)) {
		oma.left <- if(combined) 2.5 else 4.5
		oma <- c(5, oma.left, 1.5, 1)+0.1 
	} else oma <- dots$oma
	par(mar=rep(0, 4), oma=oma, xpd=NA)
	csi <- par("csi")  # margin line height in inches
	gap.inch <- gap * csi  # gap in inches
	gap.cm <- gap.inch * 2.54  # gap in centimeters
	p.bars <- 0.1
	p.numbers <- if(combined) 0.1 else 0.15
	p.gap <- 0.05  # gap before bars and numbers
	if(is.null(dots$las)) {
		las <- par("las")
		if(las == 0) las <- 3
		else if(las == 1) las <- 2
	} else las <- dots$las
	
	# combinations
	tc <- x$tabcomb
	count <- x$count
	nc <- nrow(tc)
	pc <- ncol(tc)
	
	# if desired, check if numbers can be plotted
	if(numbers) {
		num <- if(prop[2]) format(x$percent/100, digits=2) else count
		num.height <- strheight(num, units="inches", cex=cex.numbers)
		space.vert <- par("pin")[2]/nc
		if(all(num.height < space.vert)) {
			plot.width <- par("pin")[1] - gap.inch
			p.sum <- 2 + p.bars + p.numbers + 2*p.gap
			num.width <- strwidth(num, units="inches", cex=cex.numbers)
			space.horiz <- plot.width*p.numbers/p.sum + oma[4]*csi
			num.width.ok <- num.width < space.horiz
			if(!any(num.width.ok)) {
				numbers <- FALSE
				warning("not enough horizontal space to display frequencies")
			}
		} else {
			numbers <- FALSE
			warning("not enough vertical space to display ", 
				"frequencies (too many combinations)")
		}
	}
	if(combined) {
        if(numbers) {
            if(bars) {
                l.mat <- matrix(c(1, 0, 2, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 4), 3, 5)
                l.widths <- c(1, p.gap, p.bars, p.gap, p.numbers)
            } else {
                l.mat <- matrix(c(1, 0, 2, 0, 0, 0, 0, 0, 3), 3, 3)
                l.widths <- c(1, p.gap, p.numbers)
            }
        } else {
            if(bars) {
                l.mat <- matrix(c(1, 0, 2, 0, 0, 0, 0, 0, 3), 3, 3)
                l.widths <- c(1, p.gap, p.bars)
            } else {
                l.mat <- matrix(c(1, 0, 2), 3, 1)
                l.widths <- 1
            }
        }
        l.heights <- c(p.bars, p.gap, 1)
        layout(l.mat, widths=l.widths, heights=l.heights)
	} else {
		if(numbers) {
			if(bars) {
				l.mat <- t(c(1, 0, 2, 0, 3, 0, 4))
				l.widths <- c(1, lcm(gap.cm), 1, p.gap, p.bars, p.gap, p.numbers)
			} else {
				l.mat <- t(c(1, 0, 2, 0, 3))
				l.widths <- c(1, lcm(gap.cm), 1, p.gap, p.numbers)
			}
		} else {
			if(bars) {
				l.mat <- t(c(1, 0, 2, 0, 3))
				l.widths <- c(1, lcm(gap.cm), 1, p.gap, p.bars)
			} else {
				l.mat <- t(c(1, 0, 2))
				l.widths <- c(1, lcm(gap.cm), 1)
			}
		}
		layout(l.mat, widths=l.widths)
	}
	par(cex=op$cex, mex=op$mex)
	
	# check if x-axis should be plotted and get labels
	x.axis <- TRUE
	if(is.logical(labels)) {
		if(!is.na(labels) && labels) labels <- NULL
		else x.axis <- FALSE
	}
	if(is.null(labels)) {
		if(is.null(dim(x$x))) cn <- ""
		else {
			cn <- colnames(x$x)
			if(is.null(cn)) cn <- defaultNames(ncol(x$x))
		}
	}
	else cn <- rep(labels, length.out=px)
	
	# barplot
	miss <- x$missings[, 2]
	if(!combined && prop[1]) miss <- miss/nx
	if(!imputed) color <- rep(col[2],px)
	else color <- ifelse(x$imputed,col[3],col[2])
	if(sortVars) {
		ordVars <- order(miss, decreasing=TRUE)
		miss <- miss[ordVars]
		cn <- cn[ordVars]
		color <- color[ordVars]
	}
	if(all(miss == 0)) {
		ylim <- if(!combined && prop[1]) c(0,1) else c(0, nx)
	} else ylim <- c(0, max(miss))
	if(x.axis) {
		space.vert <- (oma[1]-1)*csi
	}
	if(combined) {
		xlim <- c(0, pc)
		barplot(miss, col=color, border=border, axes=FALSE, 
			xlim=0.1+1.2*xlim, ylim=ylim, xaxs="i", yaxs="i", xpd=NA)
	} else {
		if(las %in% 2:3) {
			mp <- barplot(miss, col=color, border=border, ylab=ylabs[1], 
				ylim=ylim, xpd=NA, axes=axes, axisnames=FALSE, cex.axis=cex.axis, 
				cex.lab=cex.lab, las=las)
			if(x.axis) {
				cn.ok <- prettyLabels(cn, as.vector(mp), space.vert, cex.axis)
				axis(side=1, at=mp[cn.ok], labels=cn[cn.ok], 
					lty=0, las=las, cex.axis=cex.axis)
			}
		} else {
			barplot(miss, col=color, names.arg=cn, border=border, 
				ylab=ylabs[1], ylim=ylim, xpd=NA, axes=axes, axisnames=x.axis, 
				cex.axis=cex.axis, cex.names=cex.axis, cex.lab=cex.lab, las=las)
		}
	}
	
	# combinations plot
	if(sortVars) tc <- tc[, ordVars, drop=FALSE]
	if(sortCombs) {
		ordCombs <- order(count, decreasing=TRUE)
		count <- count[ordCombs]
		tc <- tc[ordCombs, , drop=FALSE]
		if(numbers) num <- num[ordCombs]
	}
	# axis limits
	xlim <- c(0, pc)
#	ylim <- c(0, nc)
	# define rectangles
	xleft <- 0:(pc-1)
	xright <- 1:pc
	if(varheight) {
		cs <- cumsum(count)
		ybottom <- c(0, cs[-nc])
		ytop <- cs
		ylim <- c(0, cs[nc])
	} else {
		ybottom <- 0:(nc-1)
		ytop <- 1:nc
		ylim <- c(0, nc)
	}
	rects <- merge(data.frame(ybottom,ytop), data.frame(xleft,xright))
	# initialize plot
	initializePlot(xlim, ylim)
	# match the colors
	color <- matrix(nrow = nrow(tc), ncol = ncol(tc))
	color[which(tc == 0)] <- col[1]
	color[which(tc == 1)] <- col[2]
	color[which(tc == 2)] <- col[3]
	# plot rectangles
	rect(rects$xleft, rects$ybottom, rects$xright, rects$ytop, 
		col=color, border=border)
	title(ylab=ylabs[2-combined], line=1, cex.lab = cex.lab)
	if(x.axis) {
		at <- (xleft+xright)/2
		if(las %in% 2:3) {
			cn.ok <- prettyLabels(cn, at, space.vert, cex.axis)
			axis(side=1, at=at[cn.ok], labels=cn[cn.ok], 
				lty=0, las=las, cex.axis=cex.axis)
		} else {
			axis(side=1, at=at, labels=cn, lty=0, las=las, cex.axis=cex.axis)
		}
	}
	if(bars) {  # add barplot
		
#		cols <- ifelse(rowSums(tc) == 0, col[1], col[2])
#		cols[unique(which(tc == 2, arr.ind = TRUE)[,1])] <- col[3]
#		barplot(count, horiz=TRUE, col=cols, border=border, 
#			axes=FALSE, ylim=0.1+1.2*ylim, xaxs="i", yaxs="i", xpd=NA)
	
		which.complete <- rowSums(tc) == 0
		if(only.miss && any(which.complete)) {
			# set count of bar for complete observations to 0
			countPlot <- count
			countPlot[which.complete] <- 0
			# transparent color and border of bar for complete observations
			cols <- ifelse(which.complete, NA, col[2])
			cols[unique(which(tc == 2, arr.ind = TRUE)[,1])] <- col[3]
			border <- rep(border, length.out=length(count))
			border[which.complete] <- NA
			# plot bars
			barplot(countPlot, horiz=TRUE, col=cols, border=border, 
					axes=FALSE, ylim=0.1+1.2*ylim, xaxs="i", yaxs="i", xpd=NA)
			# when numbers are not plotted, plot amount of complete observations
			if(!numbers) {
				if(prop[2]) {
					num <- x$percent/100
					if(sortCombs) num <- num[ordCombs]
					num <- format(num[which.complete], digits=2)
				} else num <- count[which.complete]
				text(0, 0.1+1.2*(ybottom[which.complete]+ytop[which.complete])/2, 
						labels=num, adj=0, cex=cex.numbers)
			}
		} else {
			cols <- ifelse(which.complete, col[1], col[2])
			cols[unique(which(tc == 2, arr.ind = TRUE)[,1])] <- col[3]
			barplot(count, horiz=TRUE, col=cols, border=border, 
					axes=FALSE, ylim=0.1+1.2*ylim, xaxs="i", yaxs="i", xpd=NA)
		}
	}
	if(numbers) {  # plot number of combinations
		initializePlot(c(0, 1), ylim)
		num[!num.width.ok] <- ""
		text(0, (ybottom+ytop)/2, labels=num, adj=0, cex=cex.numbers)
	}
	
	# labels may not have been plotted, if we sorted the variables, we 
	# should print out their order, otherwise plot is not very useful
	if(sortVars) {
		if(!imputed) cat("\n Variables sorted by number of missings: \n")
		else if (!miss_imp) cat("\n Variables sorted by number of imputed missings: \n")
		else cat("\n Variables sorted by number of missings or imputed missings: \n")
		tmp <- data.frame(Variable=cn, Count=miss)
		print(tmp, row.names=FALSE)
	}
	invisible()
}

# print method


#' Print method for objects of class aggr
#' 
#' Print method for objects of class `"aggr"`.
#' 
#' 
#' @param digits the minimum number of significant digits to be used (see
#' [print.default()]).
#' @author Matthias Templ, modifications by Andreas Alfons and Bernd Prantner
#' @seealso [aggr()]
#' @keywords print
#' @examples
#' 
#' data(sleep, package = "VIM")
#' a <- aggr(sleep, plot=FALSE)
#' a
#' 
#' @rdname aggr
#' @method print aggr
#' @export
print.aggr <- function(x, ..., digits = NULL) {
	i <- x$missings[,2] > 0
	imputed <- x$imputed
	
	if(!any(imputed)) cat("\n Missings in variables:\n")
	else if(any(i & !imputed)) cat("\n Missings or imputed missings in variables:\n")
	else if(any(i))	cat("\n Imputed missings in variables:\n")
	else cat("No missings.\n")
	
	print(x$missings[i, ], digits=digits, row.names=FALSE)
}


#' Summary method for objects of class aggr
#' 
#' Summary method for objects of class `"aggr"`.
#' 
#' 
#' @param object an object of class `"aggr"`.
#' @return a list of class `"summary.aggr"` containing the following
#' components:
#' -  missings a `data.frame` containing the amount of missing or
#' imputed values in each variable.
#' -  combinations a `data.frame` containing a character vector
#' representing the combinations of variables along with their frequencies and
#' percentages.
#' @author Matthias Templ, modifications by Andreas Alfons
#' @seealso [print.summary.aggr()], [aggr()]
#' @keywords print
#' @examples
#' 
#' data(sleep, package = "VIM")
#' summary(aggr(sleep, plot=FALSE))
#' 
#' @rdname aggr
#' @export
#' @method summary aggr
summary.aggr <- function(object, ...) {
	res <- list(missings=object$missings, 
		combinations=data.frame(Combinations=object$combinations, 
			Count=object$count, Percent=object$percent), imputed = object$imputed)
	class(res) <- "summary.aggr"
	res
}



#' Print method for objects of class summary.aggr
#' 
#' Print method for objects of class `"summary.aggr"`.
#' 
#' 
#' @param x an object of class `"summary.aggr"`.
#' @param \dots Further arguments, currently ignored.
#' @author Andreas Alfons, modifications by Bernd Prantner
#' @seealso [summary.aggr()], [aggr()]
#' @keywords print
#' @examples
#' 
#' data(sleep, package = "VIM")
#' s <- summary(aggr(sleep, plot=FALSE))
#' s
#' 
#' @rdname aggr
#' @export
#' @method print summary.aggr
print.summary.aggr <- function(x, ...) {
	i <- x$missings[,2] > 0
	imputed <- x$imputed
	
	if(!any(imputed)) cat("\n Missings per variable: \n")
	else if(any(i & !imputed)) cat("\n Missings or imputed missings per variables:\n")
	else cat("\n Imputed missings per variables:\n")
	
	print(x$missings, row.names=FALSE)
	
	if(!any(imputed)) cat("\n Missings in combinations of variables: \n")
	else if(any(i & !imputed)) cat("\n Missings or imputed missings in combinations of variables:\n")
	else cat("\n Imputed missings in combinations of variables:\n")
	
	print(x$combinations, row.names=FALSE)
}
