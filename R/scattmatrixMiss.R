# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner, Matthias Templ
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ----------------------------------------------------------



#' Scatterplot matrix with information about missing/imputed values
#' 
#' Scatterplot matrix in which observations with missing/imputed values in
#' certain variables are highlighted.
#' 
#' `scattmatrixMiss` uses [pairsVIM()] with a panel function
#' that allows highlighting of missing/imputed values.
#' 
#' If `interactive=TRUE`, the variables to be used for highlighting can be
#' selected interactively.  Observations with missing/imputed values in any or
#' in all of the selected variables are highlighted (as determined by
#' `selection`).  A variable can be added to the selection by clicking in
#' a diagonal panel.  If a variable is already selected, clicking on the
#' corresponding diagonal panel removes it from the selection.  Clicking
#' anywhere else quits the interactive session.
#' 
#' The graphical parameter `oma` will be set unless supplied as an
#' argument.
#' 
#' `TKRscattmatrixMiss` behaves like `scattmatrixMiss`, but uses
#' tkrplot to embed the plot in a *Tcl/Tk* window.
#' This is useful if the number of variables is large, because scrollbars allow
#' to move from one part of the plot to another.
#' 
#' @param x a matrix or `data.frame`.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, `x` needs to have
#' [colnames()]). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see `col`).
#' @param highlight a vector giving the variables to be used for highlighting.
#' If `NULL` (the default), all variables are used for highlighting.
#' @param selection the selection method for highlighting missing/imputed
#' values in multiple highlight variables.  Possible values are `"any"`
#' (highlighting of missing/imputed values in *any* of the highlight
#' variables) and `"all"` (highlighting of missing/imputed values in
#' *all* of the highlight variables).
#' @param plotvars a vector giving the variables to be plotted.  If `NULL`
#' (the default), all variables are plotted.
#' @param col a vector of length three giving the colors to be used in the
#' plot.  The second/third color will be used for highlighting missing/imputed
#' values.
#' @param alpha a numeric value between 0 and 1 giving the level of
#' transparency of the colors, or `NULL`.  This can be used to prevent
#' overplotting.
#' @param pch a vector of length two giving the plot characters.  The second
#' plot character will be used for the highlighted observations.
#' @param lty a vector of length two giving the line types for the density
#' plots in the diagonal panels (if `diagonal="density"`).  The second
#' line type is used for the highlighted observations.  If a single value is
#' supplied, it is used for both non-highlighted and highlighted observations.
#' @param diagonal a character string specifying the plot to be drawn in the
#' diagonal panels.  Possible values are `"density"` (density plots for
#' non-highlighted and highlighted observations) and `"none"`.
#' @param interactive a logical indicating whether the variables to be used for
#' highlighting can be selected interactively (see \sQuote{Details}).
#' @param \dots for `scattmatrixMiss`, further arguments and graphical
#' parameters to be passed to [pairsVIM()].  `par("oma")` will
#' be set appropriately unless supplied (see [graphics::par()]).  For
#' `TKRscattmatrixMiss`, further arguments to be passed to
#' `scattmatrixMiss`.
#' @note Some of the argument names and positions have changed with version 1.3
#' due to a re-implementation and for more consistency with other plot
#' functions in `VIM`.  For back compatibility, the argument
#' `colcomb` can still be supplied to \code{\dots{}} and is handled
#' correctly.  Nevertheless, it is deprecated and no longer documented.  Use
#' `highlight` instead.  The arguments `smooth`, `reg.line` and
#' `legend.plot` are no longer used and ignored if supplied.
#' @author Andreas Alfons, Matthias Templ, modifications by Bernd Prantner
#' @seealso [pairsVIM()], [marginmatrix()]
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  *Journal of Advances in Data Analysis
#' and Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(sleep, package = "VIM")
#' ## for missing values
#' x <- sleep[, 1:5]
#' x[,c(1,2,4)] <- log10(x[,c(1,2,4)])
#' scattmatrixMiss(x, highlight = "Dream")
#' 
#' ## for imputed values
#' x_imp <- kNN(sleep[, 1:5])
#' x_imp[,c(1,2,4)] <- log10(x_imp[,c(1,2,4)])
#' scattmatrixMiss(x_imp, delimiter = "_imp", highlight = "Dream")
#' 
#' @export scattmatrixMiss
scattmatrixMiss <- function(x, delimiter = NULL, highlight = NULL, 
                            selection = c("any","all"), plotvars = NULL, 
                            col = c("skyblue","red","orange"), alpha = NULL, 
                            pch = c(1,3), lty = par("lty"), 
                            diagonal = c("density","none"), 
                            interactive = TRUE, ...)  {
  UseMethod("scattmatrixMiss", x)
}

#' @rdname scattmatrixMiss
#' @export

scattmatrixMiss.data.frame <- function(x, delimiter = NULL, highlight = NULL, 
                                       selection = c("any","all"), plotvars = NULL, 
                                       col = c("skyblue","red","orange"), alpha = NULL, 
                                       pch = c(1,3), lty = par("lty"), 
                                       diagonal = c("density","none"), 
                                       interactive = TRUE, ...)  {
  scattmatrixMiss_work(x, delimiter, highlight, selection, plotvars, 
                       col, alpha, pch, lty, diagonal, interactive, ...)
}

#' @rdname scattmatrixMiss
#' @export

scattmatrixMiss.survey.design <- function(x, delimiter = NULL, highlight = NULL, 
                                          selection = c("any","all"), plotvars = NULL, 
                                          col = c("skyblue","red","orange"), alpha = NULL, 
                                          pch = c(1,3), lty = par("lty"), 
                                          diagonal = c("density","none"), 
                                          interactive = TRUE, ...)  {
  scattmatrixMiss_work(x$variables, delimiter, highlight, selection, plotvars, 
                       col, alpha, pch, lty, diagonal, interactive, ...)
}

#' @rdname scattmatrixMiss
#' @export

scattmatrixMiss.default <- function(x, delimiter = NULL, highlight = NULL, 
                                    selection = c("any","all"), plotvars = NULL, 
                                    col = c("skyblue","red","orange"), alpha = NULL, 
                                    pch = c(1,3), lty = par("lty"), 
                                    diagonal = c("density","none"), 
                                    interactive = TRUE, ...)  {
  scattmatrixMiss_work(as.data.frame(x), delimiter, highlight, selection, plotvars, 
                       col, alpha, pch, lty, diagonal, interactive, ...)
}

scattmatrixMiss_work <- function(x, delimiter = NULL, highlight = NULL, 
        selection = c("any","all"), plotvars = NULL, 
        col = c("skyblue","red","orange"), alpha = NULL, 
        pch = c(1,3), lty = par("lty"), 
        diagonal = c("density","none"), 
        interactive = TRUE, ...) {
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
    pz <- ncol(z)
    if(pz < 2) stop("the data to be plotted must be at least 2-dimensional")
    selection <- match.arg(selection)
    if(length(col) == 0) col <- c("skyblue","red","orange")
    if(length(pch) == 0) pch <- c(1,3)
    if(length(col) == 1 && length(pch) == 1) {
        stop("same color and plot symbol for observed and missing values")
    }
    if(length(col) == 1) col <- rep(col, 3)
	else if(length(col) == 2) col <- rep(col,1:2)
    else if(length(col) > 3) col <- col[1:3]
    if(length(pch) == 1) pch <- rep(pch, 2)
    else if(length(pch) > 2) pch <- pch[1:2]
    # semitransparent colors
    colalpha <- alphablend(col, alpha)
    # prepare data
    if(is.data.frame(z)) z <- data.matrix(z)
    else if(mode(z) != "numeric") mode(z) <- "numeric"
    # diagonal panel
    diagonal <- match.arg(diagonal)
    if(diagonal == "density") {
        if(length(lty) == 0) lty <- par("lty")
        if(length(lty) == 1) {
            if(col[1] == col[2]) {
				stop("same color and line type for observed and missing values")
            } else if(col[1] == col[3]) {
				stop("same color and line type for observed and imputed values")
			}
            lty <- rep.int(lty, 2)
        }
        else if(length(lty) > 2) lty <- lty[1:2]
    }
    # initialize call
    localPairs <- function(..., colcomb, smooth, reg.line, legend.plot) {
        pairsVIM(...)
    }
    ca <- as.call(list(localPairs, ...))
    # back compatibility
    if(missing(highlight) && !is.null(ca$colcomb)) {
        if(length(ca$colcomb) && ca$colcomb[1] == "missnonmiss") {
            highlight <- NULL
        } else highlight <- ca$colcomb
    }
    if(interactive) {
        # 'gap', 'oma' and 'layout' are needed later on
        if(is.null(ca$oma)) {
            # only 'oma' is used by 'pairsVIM' for outer margins
            ca$oma <- rep.int(4, 4)
            if(!is.null(ca$main)) ca$oma[3] <- 6
            if(!is.null(ca$sub)) ca$oma[1] <- 5
        }
        if(is.null(ca$gap)) ca$gap <- 1
        if(is.null(ca$layout)) ca$layout <- "matrix"
    }
    createPlot <- function() {
        # find observations with missings
        if(is.null(highlight)) {
			if(!imputed) NAvec <- isNA(x, selection)
			else NAvec <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
		}
        else {
			if(!imputed) NAvec <- isNA(x[, highlight], selection)
			else NAvec <- isImp(x[, highlight, drop=FALSE], pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
		}
        # panel functions
        panel.miss <- function(x, y, ...) {
            if(!imputed) {
				miss <- NULL
				color <- colalpha[2]
			} else {
				tmp <- isImp(cbind(x,y), pos = 1, delimiter = delimiter, imp_var = imp_var, selection = "none")
				miss <- cbind(tmp[["misspos"]],tmp[["missh"]])
				color <- colalpha[3]
			}
			xOK <- x[!NAvec, ,drop = FALSE]
            yOK <- y[!NAvec, ,drop = FALSE]
            points(xOK, yOK, col=colalpha[1], pch=pch[1], ...) 
            rugNA(xOK, yOK, miss = miss[!NAvec,], side=1, col=colalpha[1])
			rugNA(xOK, yOK, miss = miss[!NAvec,], side=2, col=colalpha[1])
            xNA <- x[NAvec]
            yNA <- y[NAvec]
			points(xNA, yNA, col=color, pch=pch[2], ...)
			rugNA(xNA, yNA, miss = miss[NAvec,], side=1, col=color)
			rugNA(xNA, yNA, miss = miss[NAvec,], side=2, col=color)
        }
        panel.density <- function(x, ...) {
            if(!all(is.na(x))) {
                xobs <- x[!NAvec]
                xobs <- xobs[is.finite(xobs)]
                xNA <- x[NAvec]
                xNA <- xNA[is.finite(xNA)]
                rx <- range(x, finite=TRUE)
                if(par("ylog")) {
                    # y-axis should not be on logarithmic scale
                    localPlot <- function(..., type, log, main, sub, xlab, ylab,
                            ann, axes, frame.plot, panel.first, panel.last) {
                        par(new=TRUE)
                        log <- if(par("xlog")) "x" else ""
                        plot(..., type="n", log=log, ann=FALSE, axes=FALSE)
                    }
                    localPlot(rx, rx, ...)
                }
                if(length(xobs)) dobs <- density(xobs, from=rx[1], to=rx[2])
                else dobs <- list(x=NULL, y=NULL)
                if(length(xNA)) dNA <- density(xNA, from=rx[1], to=rx[2])
                else dNA <- list(x=NULL, y=NULL)
                dy <- c(dobs$y, dNA$y)  # y-values of both densities
                if(length(dy)) {  # cannot compute maximum otherwise 
                    mdy <- max(c(dobs$y, dNA$y))
                    lines(dobs$x, rx[1]+dobs$y*diff(rx)/mdy, 
                        col=col[1], lty=lty[1], 
                        ...)
					if(!imputed) color <- col[2]
					else color <- col[3]
                    lines(dNA$x, rx[1]+dNA$y*diff(rx)/mdy, 
                        col=color, lty=lty[2], 
                        ...)
                }
            }
        }
#        panel.text <- function(x, y, txt, cex, font, ...) {
#            usr <- par("usr")
#            txt.width <- strwidth(txt, cex=cex, font=font, ...)
#            txt.height <- strheight(txt, cex=cex, font=font, ...)
#            if(txt.width < diff(usr[1:2]) && txt.height < diff(usr[3:4])) {
#                text(x, y, txt, cex = cex, font = font, ...)
#            }
#        }
        ca$x <- z
        ca$panel <- panel.miss
        ca$lower <- NULL
        ca$upper <- NULL
        ca$diagonal <- switch(diagonal, density=panel.density, none=NULL)
#        ca$text.panel <- panel.text
        eval(ca)
    }
    createPlot()
    
    # check for interactive graphics device
    dev <- names(dev.cur())
    interactiveDevices <- c("X11","quartz","windows")
    if(interactive && any(!is.na(charmatch(interactiveDevices, dev)))) {
        cat(paste("\nClick in a diagonal panel to add to", 
                "or remove from the highlight selection.\n"))
        cat(paste("To regain use of the VIM GUI and the R console,",
                "click anywhere else in the graphics window.\n\n"))
        # retrieve geometry of graphics device
        gap <- ca$gap
        oma <- ca$oma
        rf <- if(pz == 2) 5/6 else 2/3  # reduction factor for line height
        op <- par(mar=oma*rf, usr=c(0,1,0,1))
        on.exit(par(op))
        xcenter <- seq(from=1/(2*pz), by=1/pz, length.out=pz)
        ycenter <- if(ca$layout == "matrix") rev(xcenter) else xcenter
        cxy <- par("cxy")  # cxy[2] gives the line height of the graphics device
        lxy <- 1/(2*pz) - gap*cxy[2]*rf/2  # half side length of the panels
        xleft <- xcenter - lxy
        ybottom <- ycenter - lxy
        xright <- xcenter + lxy
        ytop <- ycenter + lxy
        # initializations for selection
        cn <- colnames(x)
        if(is.null(highlight)) highlight <- cn
        else if(!is.character(highlight)) highlight <- cn[highlight]
        plotvars <- colnames(z)
        # start interactive session
        highlightInfo(highlight, selection, imputed)  # print out current selection
        pt <- locatorVIM(error=TRUE)
        while(!is.null(pt)  && class(pt) != "try-error") {
            i <- which(pt$y > ybottom & pt$y < ytop)
            j <- which(pt$x > xleft & pt$x < xright)
#            # can't happen since margins must be non-negative
#            if(length(i) > 0) i <- i[1]
#            if(length(j) > 0) j <- j[1]
            if(length(i) && length(j) && i == j) {
                highlight <- 
                    if(plotvars[i] %in% highlight) 
                        setdiff(highlight, plotvars[i]) 
                    else c(highlight, plotvars[i])
                createPlot()
                highlightInfo(highlight, selection, imputed)  # print out current selection
#                # make sure user coordinate system remains as set up above 
#                # before locator is called again (pairs seems to leave user 
#                # coordinates in an uncontrollable state if 'row1attop=FALSE') 
#                par(mar=oma*rf, usr=c(0,1,0,1))
                pt <- locatorVIM(error=TRUE)
            }
            else pt <- NULL
        }
        if(class(pt) == "try-error") on.exit()
    }
    
    invisible()
}
