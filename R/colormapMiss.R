# ---------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         and Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------

#' Colored map with information about missing/imputed values
#' 
#' Colored map in which the proportion or amount of missing/imputed values in
#' each region is coded according to a continuous or discrete color scheme.
#' The sequential color palette may thereby be computed in the \emph{HCL} or
#' the \emph{RGB} color space.
#' 
#' The proportion or amount of missing/imputed values in \code{x} of each
#' region is coded according to a continuous or discrete color scheme in the
#' color range defined by \code{col}.  In addition, the proportions or numbers
#' can be shown as labels in the regions.
#' 
#' If \code{interactive} is \code{TRUE}, clicking in a region displays more
#' detailed information about missing/imputed values on the console.  Clicking
#' outside the borders quits the interactive session.
#' 
#' @rdname colormapMiss
#' @aliases colormapMiss colormapMissLegend
#' @param x a numeric vector.
#' @param region a vector or factor of the same length as \code{x} giving the
#' regions.
#' @param map an object of any class that contains polygons and provides its
#' own plot method (e.g., \code{"SpatialPolygons"} from package \code{sp}).
#' @param imp_index a logical-vector indicating which values of \sQuote{x} have
#' been imputed. If given, it is used for highlighting and the colors are
#' adjusted according to the given colors for imputed variables (see
#' \code{col}).
#' @param prop a logical indicating whether the proportion of missing/imputed
#' values should be used rather than the total amount.
#' @param polysRegion a numeric vector specifying the region that each polygon
#' belongs to.
#' @param range a numeric vector of length two specifying the range (minimum
#' and maximum) of the proportion or amount of missing/imputed values to be
#' used for the color scheme.
#' @param n for \code{colormapMiss}, the number of equally spaced cut-off
#' points for a discretized color scheme.  If this is not a positive integer, a
#' continuous color scheme is used (the default).  In the latter case, the
#' number of rectangles to be drawn in the legend can be specified in
#' \code{colormapMissLegend}.  A reasonably large number makes it appear
#' continuously.
#' @param col the color range (start end end) to be used.  RGB colors may be
#' specified as character strings or as objects of class
#' "\code{\link[colorspace]{RGB}}".  HCL colors need to be specified as objects
#' of class "\code{\link[colorspace]{polarLUV}}".  If only one color is
#' supplied, it is used as end color, while the start color is taken to be
#' transparent for RGB or white for HCL.
#' @param gamma numeric; the display \emph{gamma} value (see
#' \code{\link[colorspace]{hex}}).
#' @param fixup a logical indicating whether the colors should be corrected to
#' valid RGB values (see \code{\link[colorspace]{hex}}).
#' @param coords a matrix or \code{data.frame} with two columns giving the
#' coordinates for the labels.
#' @param numbers a logical indicating whether the corresponding proportions or
#' numbers of missing/imputed values should be used as labels for the regions.
#' @param digits the number of digits to be used in the labels (in case of
#' proportions).
#' @param cex.numbers the character expansion factor to be used for the labels.
#' @param col.numbers the color to be used for the labels.
#' @param legend a logical indicating whether a legend should be plotted.
#' @param interactive a logical indicating whether more detailed information
#' about missing/imputed values should be displayed interactively (see
#' \sQuote{Details}).
#' @param xleft left \emph{x} position of the legend.
#' @param ybottom bottom \emph{y} position of the legend.
#' @param xright right \emph{x} position of the legend.
#' @param ytop top \emph{y} position of the legend.
#' @param cmap a list as returned by \code{colormapMiss} that contains the
#' required information for the legend.
#' @param horizontal a logical indicating whether the legend should be drawn
#' horizontally or vertically.
#' @param \dots further arguments to be passed to \code{plot}.
#' @return \code{colormapMiss} returns a list with the following components:
#' - nmiss a numeric vector containing the number of missing/imputed
#' values in each region.
#' - nobs a numeric vector containing the number of observations in
#' each region.
#' - pmiss a numeric vector containing the proportion of missing
#' values in each region.
#' - prop a logical indicating whether the proportion of
#' missing/imputed values have been used rather than the total amount.
#' - range the range of the proportion or amount of missing/imputed
#' values corresponding to the color range.
#' - n either a positive integer giving the number of equally spaced
#' cut-off points for a discretized color scheme, or \code{NULL} for a
#' continuous color scheme.
#' - start the start color of the color scheme.
#' - end the end color of the color scheme.
#' - space a character string giving the color space (either
#' \code{"rgb"} for RGB colors or \code{"hcl"} for HCL colors).
#' - gamma numeric; the display \emph{gamma} value (see
#' \code{\link[colorspace]{hex}}).
#' - fixup a logical indicating whether the colors have been
#' corrected to valid RGB values (see \code{\link[colorspace]{hex}}).
#' @note Some of the argument names and positions have changed with versions
#' 1.3 and 1.4 due to extended functionality and for more consistency with
#' other plot functions in \code{VIM}.  For back compatibility, the arguments
#' \code{cex.text} and \code{col.text} can still be supplied to \code{\dots{}}
#' and are handled correctly.  Nevertheless, they are deprecated and no longer
#' documented.  Use \code{cex.numbers} and \code{col.numbers} instead.
#' @author Andreas Alfons, modifications to show imputed values by Bernd
#' Prantner
#' @seealso \code{\link{colSequence}}, \code{\link{growdotMiss}},
#' \code{\link{mapMiss}}
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  \emph{Journal of Advances in Data Analysis
#' and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @export colormapMiss
colormapMiss <- function(x, region, map, imp_index = NULL,
                         prop = TRUE, polysRegion = 1:length(x), range = NULL,
                         n = NULL, col = c("red","orange"),
                         gamma = 2.2, fixup = TRUE, coords = NULL, 
                         numbers = TRUE, digits = 2, cex.numbers = 0.8, 
                         col.numbers = par("fg"), legend = TRUE, 
                         interactive = TRUE, ...) {
  UseMethod("colormapMiss", x)
}

#' @rdname colormapMiss
#' @export

colormapMiss.data.frame <- function(x, region, map, imp_index = NULL,
                                    prop = TRUE, polysRegion = 1:length(x), range = NULL,
                                    n = NULL, col = c("red","orange"),
                                    gamma = 2.2, fixup = TRUE, coords = NULL, 
                                    numbers = TRUE, digits = 2, cex.numbers = 0.8, 
                                    col.numbers = par("fg"), legend = TRUE, 
                                    interactive = TRUE, ...) {
  colormapMiss_work(x, region, map,imp_index, prop, polysRegion, range, n, col,
                    gamma, fixup, coords, numbers, digits, cex.numbers, col.numbers,
                    legend, interactive, ...)
}

#' @rdname colormapMiss
#' @export

colormapMiss.survey.design <- function(x, region, map, imp_index = NULL,
                                       prop = TRUE, polysRegion = 1:length(x), range = NULL,
                                       n = NULL, col = c("red","orange"),
                                       gamma = 2.2, fixup = TRUE, coords = NULL, 
                                       numbers = TRUE, digits = 2, cex.numbers = 0.8, 
                                       col.numbers = par("fg"), legend = TRUE, 
                                       interactive = TRUE, ...) {
  colormapMiss_work(x$variables, region, map,imp_index, prop, polysRegion, range, n, col,
                    gamma, fixup, coords, numbers, digits, cex.numbers, col.numbers,
                    legend, interactive, ...)
}

#' @rdname colormapMiss
#' @export

colormapMiss.default <- function(x, region, map, imp_index = NULL,
                                 prop = TRUE, polysRegion = 1:length(x), range = NULL,
                                 n = NULL, col = c("red","orange"),
                                 gamma = 2.2, fixup = TRUE, coords = NULL, 
                                 numbers = TRUE, digits = 2, cex.numbers = 0.8, 
                                 col.numbers = par("fg"), legend = TRUE, 
                                 interactive = TRUE, ...) {
  colormapMiss_work(as.data.frame(x), region, map,imp_index, prop, polysRegion, range, n, col,
                    gamma, fixup, coords, numbers, digits, cex.numbers, col.numbers,
                    legend, interactive, ...)
}

colormapMiss_work <- function(x, region, map, imp_index = NULL,
		prop = TRUE, polysRegion = 1:length(x), range = NULL,
		n = NULL, col = c("red","orange"),
        #space = c("rgb", "hcl"), 
        gamma = 2.2, fixup = TRUE, coords = NULL, 
        numbers = TRUE, digits = 2, cex.numbers = 0.8, 
        col.numbers = par("fg"), legend = TRUE, 
        interactive = TRUE, ...) {
    
    # back compatibility
    dots <- list(...)
    if(missing(cex.numbers) && "cex.text" %in% names(dots)) {
        cex.numbers <- dots$cex.text
    }
    if(missing(col.numbers) && "col.text" %in% names(dots)) {
        col.numbers <- dots$col.text
    }
	
    # initializations
	imputed <- FALSE
	if(!is.null(imp_index)) {
		if(any(is.na(x))) {
			imputed <- FALSE
			warning("'imp_index' is given, but there are missing values in 'x'! 'imp_index' will be ignored.", call. = FALSE)
		} else {
			if(is.numeric(imp_index) && range(imp_index) == c(0,1)) imp_index <- as.logical(imp_index)
			else if(!is.logical(imp_index)) stop("The missing-index of the imputed Variable must be of the type logical")
			imputed <- TRUE
		}
	}
	x <- as.vector(x)
    region <- as.factor(region)
    if(!is.null(coords)) {  # error messages
        if(!(inherits(coords, c("data.frame","matrix")))) 
            stop("'coords' must be a data.frame or matrix")
        if(ncol(coords) != 2) stop("'coords' must be 2-dimensional")
    }
    if(is.character(map)) map <- get(map, envir=.GlobalEnv)
    prop <- isTRUE(prop)
    # check colors
    if(!is(col, "RGB") && !is(col, "polarLUV") && 
        (!is.character(col) || length(col) == 0 || col == c("red","orange"))) {
		if(!imputed) col <- "red"
		else col <- "orange"
	}
    if(is.character(col)) {
        # colors given as character string
        if(length(col) == 1) {
            start <- par("bg")
            end <- col
        } else {
            start <- col[1]
            end <- col[2]
        }
        space <- "rgb"
    } else {
        space <- if(is(col, "RGB")) "rgb" else "hcl"
        if(nrow(coords(col)) == 1) {
            if(is(col, "RGB")) {
                # RGB colors
                start <- par("bg")
            } else {
                # HCL colors
                start <- polarLUV(0, 0, col@coords[1, "H"])
            }
            end <- col
        } else {
            start <- col[1,]
            end <- col[2,]
        }
    }
    # compute number and proportions of missing values
	if(!imputed) nmiss <- tapply(x, list(region), countNA)
	else {
		getImp <- function(x) length(which(x))
		nmiss <- tapply(unlist(imp_index), list(region), getImp)
	}
    nobs <- tapply(x, list(region), length)
    pmiss <- 100*nmiss/nobs
    # check breakpoints
    if(is.null(range)) {
        range <- c(0, if(prop) ceiling(max(pmiss)) else max(nmiss))
    } else {
        # TODO: check 'range'
    }
    # get colors for regions
    n <- rep(n, length.out=1)
    if(isTRUE(n > 1)) {
        # equally spaced categories
        breaks <- seq(range[1], range[2], length=n+1)
        cat <- cut(if(prop) pmiss else nmiss, breaks, 
            labels=FALSE, include.lowest=TRUE)
        pcol <- seq(0, 1, length=n)
        cols <- colSequence(pcol, start, end, space, gamma=gamma, fixup=fixup)
        cols <- cols[cat]
    } else {
        # continuous color scheme
        n <- NULL
        pcol <- if(prop) pmiss else nmiss
        pcol <- (pcol - range[1])/diff(range)
        cols <- colSequence(pcol, start, end, space, gamma=gamma, fixup=fixup)
    }
    cols <- cols[polysRegion] 
    localPlot <- function(..., cex.text, col.text) plot(...)
    localPlot(map, col=cols, ...)
    if(isTRUE(numbers)) {
        # number or percentage of missings as labels for regions
        if(is.null(coords)) coords <- coordinates(map)
        labs <- if(prop) paste(round(pmiss, digits), "%", sep="") else nmiss
        plabs <- labs[polysRegion]
        plabs[duplicated(polysRegion)] <- ""
        text(coords, labels=plabs, cex=cex.numbers, col=col.numbers)
    }
    # useful statistics for legend
    cmap <- list(nmiss=nmiss, nobs=nobs, pmiss=pmiss, prop=prop, range=range, 
        n=n, start=start, end=end, space=space, gamma=gamma, fixup=fixup)
    if(isTRUE(legend)) {
        usr <- par("usr")
        xrange <- usr[1:2]
        xdiff <- usr[2] - usr[1]
        yrange <- usr[3:4]
        ydiff <- usr[4] - usr[3]
        length <- 1/3
        height <- 0.1*length
        xleft <- xrange[1] + 0.02*xdiff
        xright <- xleft + length*xdiff
        ytop <- yrange[2] - 0.02*ydiff
        ybottom <- ytop - height*ydiff
        colormapMissLegend(xleft, ybottom, xright, ytop, 
            cmap, cex.numbers=cex.numbers, col.numbers=col.numbers)
    }
    if(isTRUE(interactive)) {
        cat("Click on a region to get more information about missings.\n")
        cat("To regain use of the R console, click outside the borders.\n")
        p <- locatorVIM()
        while(!is.null(p)) {
            p <- SpatialPoints(matrix(unlist(p), ncol=2))
            poly <- over(p, map)
            ind <- polysRegion[poly]
			if(!is.na(ind)) {
				if(!imputed) label <- "missings"
				else label <- "imputed missings"
                cat(paste("\n  ", levels(region)[ind], ":", sep=""))
                cat(paste("\n    Number of ", label, ":    ", nmiss[ind]))
                cat(paste("\n    Number of observations:", nobs[ind]))
                cat(paste("\n    Proportion of ", label, ": ", 
                    round(pmiss[ind], digits), "%\n", sep=""))
                p <- locatorVIM()
            } else p <- NULL
        }
    }
    # return statistics invisibly
    invisible(cmap)
}

## legend
#' @export colormapMissLegend
#' @rdname colormapMiss
colormapMissLegend <- function(xleft, ybottom, xright, ytop, cmap, 
#        range, prop = FALSE, col = "red", 
        n = 1000, horizontal = TRUE, digits = 2, 
        cex.numbers = 0.8, col.numbers = par("fg"), 
        ...) {
    # back compatibility
    dots <- list(...)
    dn <- names(dots)
    if(missing(cmap)) {
        if("range" %in% dn) range <- dots$range
        else stop("argument 'range' is missing, with no default")
        prop <- if("prop" %in% dn) dots$prop else FALSE
        col <- if("col" %in% dn) dots$col else "red"
        cmap <- list(prop=prop, range=range, n=NULL, start=par("bg"), 
            end=col, space="rgb", gamma=2.4, fixup=TRUE)
    }
    if(missing(cex.numbers) && "cex.text" %in% dn) cex.numbers <- dots$cex.text
    if(missing(col.numbers) && "col.text" %in% dn) col.numbers <- dots$col.text
    # initializations
    prop <- isTRUE(cmap$prop)
    range <- cmap$range
    cont <- is.null(cmap$n)  # is legend for continuous color scheme?
    n <- if(cont) n else cmap$n
    n <- rep(n, length.out=1)
    # allow to plot legend outside plot region
    op <- par(xpd=TRUE)
    on.exit(par(op))
    # compute steps for legend
    length <- xright - xleft
    height <- ytop - ybottom
    # compute colors for legend
    col <- colSequence(seq(0, 1, length=n), cmap$start, cmap$end, 
        cmap$space, gamma=cmap$gamma, fixup=cmap$fixup)
    # compute grid and position of legend
    grid <- seq(0, 1, length=n+1)
    if(cont) {
        pos <- 0:1
        ann <- range
    } else {
        pos <- grid
        ann <- seq(range[1], range[2], length=n+1)
    }
    ann <- if(prop) paste(format(ann, digits), "%", sep="") else ann
    # plot legend
    # TODO: check space for labels
    if(horizontal) {
        grid <- grid*length + xleft
        if(cont) {
            rect(grid[-(n+1)], ybottom, grid[-1], ytop, col=col, border=NA)
            rect(xleft, ybottom, xright, ytop, border=NULL)
        } else rect(grid[-(n+1)], ybottom, grid[-1], ytop, col=col, border=NULL)
        pos <- pos*length + xleft
        text(pos, ybottom-0.25*height, labels=ann, 
            adj=c(0.5,1), cex=cex.numbers, col=col.numbers)
    } else {
        grid <- grid*height + ybottom
        if(cont) {
            rect(xleft, grid[-(n+1)], xright, grid[-1], col=col, border=NA)
            rect(xleft, ybottom, xright, ytop, border=NULL)
        } else rect(xleft, grid[-(n+1)], xright, grid[-1], col=col, border=NULL)
        pos <- pos*height + ybottom
        text(xright+0.25*length, pos, labels=ann, 
            adj=c(0,0.5), cex=cex.numbers, col=col.numbers)
    }
    invisible()
}
