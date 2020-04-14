# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner and Matthias Templ
#          Vienna University of Technology
# ----------------------------------------------------------



#' Spineplot with information about missing/imputed values
#' 
#' Spineplot or spinogram with highlighting of missing/imputed values in other
#' variables by splitting each cell into two parts.  Additionally, information
#' about missing/imputed values in the variable of interest is shown on the
#' right hand side.
#' 
#' A spineplot is created if the variable of interest is categorial and a
#' spinogram if it is numerical.  The horizontal axis is scaled according to
#' relative frequencies of the categories/classes.  If more than one variable
#' is supplied, the cells are split according to missingness/number of imputed
#' values in the additional variables.  Thus the proportion of highlighted
#' observations in each category/class is displayed on the vertical axis. Since
#' the height of each cell corresponds to the proportion of highlighted
#' observations, it is now possible to compare the proportions of
#' missing/imputed values among the different categories/classes.
#' 
#' If `only.miss=TRUE`, the missing/imputed values in the variable of
#' interest are also visualized by a cell in the spine plot or spinogram.  If
#' additional variables are supplied, this cell is again split into two parts
#' according to missingness/number if imputed values in the additional
#' variables.
#' 
#' Otherwise, a small spineplot that visualizes missing/imputed values in the
#' variable of interest is drawn on the right hand side.  The first cell
#' corresponds to observed values and the second cell to missing/imputed
#' values.  Each of the two cells is again split into two parts according to
#' missingness/number of imputed values in the additional variables.  Note that
#' this display does not make sense if only one variable is supplied, therefore
#' `only.miss` is ignored in that case.
#' 
#' If `interactive=TRUE`, clicking in the left margin of the plot results
#' in switching to the previous variable and clicking in the right margin
#' results in switching to the next variable.  Clicking anywhere else on the
#' graphics device quits the interactive session.
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
#' @param breaks if the variable of interest is numeric, `breaks` controls
#' the breakpoints (see [graphics::hist()] for possible values).
#' @param right logical; if `TRUE` and the variable of interest is
#' numeric, the spinogram cells are right-closed (left-open) intervals.
#' @param col a vector of length six giving the colors to be used. If only one
#' color is supplied, the bars are transparent and the supplied color is used
#' for highlighting missing/imputed values.  Else if two colors are supplied,
#' they are recycled.
#' @param border the color to be used for the border of the cells.  Use
#' `border=NA` to omit borders.
#' @param main,sub main and sub title.
#' @param xlab,ylab axis labels.
#' @param axes a logical indicating whether axes should be drawn on the plot.
#' @param labels if the variable of interest is categorical, either a logical
#' indicating whether labels should be plotted below each cell, or a character
#' vector giving the labels.  This is ignored if the variable of interest is
#' numeric.
#' @param only.miss logical; if `TRUE`, the missing/imputed values in the
#' variable of interest are also visualized by a cell in the spineplot or
#' spinogram.  Otherwise, a small spineplot is drawn on the right hand side
#' (see \sQuote{Details}).
#' @param miss.labels either a logical indicating whether label(s) should be
#' plotted below the cell(s) on the right hand side, or a character string or
#' vector giving the label(s) (see \sQuote{Details}).
#' @param interactive a logical indicating whether the variables can be
#' switched interactively (see \sQuote{Details}).
#' @param \dots further graphical parameters to be passed to
#' [graphics::title()] and [graphics::axis()].
#' @return a table containing the frequencies corresponding to the cells.
#' @note Some of the argument names and positions have changed with version 1.3
#' due to extended functionality and for more consistency with other plot
#' functions in `VIM`.  For back compatibility, the arguments
#' `xaxlabels` and `missaxlabels` can still be supplied to
#' \code{\dots{}} and are handled correctly.  Nevertheless, they are deprecated
#' and no longer documented.  Use `labels` and `miss.labels` instead.
#' 
#' The code is based on the function [graphics::spineplot()] by Achim
#' Zeileis.
#' @author Andreas Alfons, Matthias Templ, modifications by Bernd Prantner
#' @seealso [histMiss()], [barMiss()],
#' [mosaicMiss()]
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  *Journal of Advances in Data Analysis
#' and Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(tao, package = "VIM")
#' data(sleep, package = "VIM")
#' ## for missing values
#' spineMiss(tao[, c("Air.Temp", "Humidity")])
#' spineMiss(sleep[, c("Exp", "Sleep")])
#' 
#' ## for imputed values
#' spineMiss(kNN(tao[, c("Air.Temp", "Humidity")]), delimiter = "_imp")
#' spineMiss(kNN(sleep[, c("Exp", "Sleep")]), delimiter = "_imp")
#' 
#' @export spineMiss
spineMiss <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
        breaks = "Sturges", right = TRUE, 
        col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
        border = NULL, main = NULL, sub = NULL, 
        xlab = NULL, ylab = NULL, axes = TRUE, 
        labels = axes, only.miss = TRUE, 
        miss.labels = axes, interactive = TRUE, ...) {
    
	imputed <- FALSE # indicates if there are Variables with missing-index
    # initializations and error messages
    if(is.null(dim(x))) {  # vector
        n <- length(x)
        p <- 1
        if(n == 0) stop("'x' must have positive length")
    } else {  # matrix or data.frame
        if(!(inherits(x, c("data.frame","matrix")))) { 
            stop("'x' must be a data.frame or matrix")
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
	# FIXME: kontrolle der breaks?
	
	if(length(col) == 0) col <- c("skyblue","red","skyblue4","red4","orange","orange4")
	else if(length(col) == 1) col <- c(rep.int(c("transparent", col), 2),rep.int(col,2))
	else if(length(col) == 3 || length(col) == 5) col <- rep.int(col[1:2], 3)
	else if(length(col) != 6) col <- rep(col, length.out=6)
    localAxis <- function(..., xaxlabels, missaxlabels) axis(...)
    localTitle <- function(..., xaxlabels, missaxlabels) title(...)
    
    # back compatibility
    dots <- list(...)
    nmdots <- names(dots)
    if(missing(labels) && "xaxlabels" %in% nmdots) labels <- dots$xaxlabels
    if(missing(miss.labels) && "missaxlabels" %in% nmdots) {
        miss.labels <- dots$missaxlabels
    }
    
    # workhorse to create plot
    createPlot <- function(main=NULL, sub=NULL, 
            xlab=NULL, ylab=NULL, labels=axes) {
        # prepare data
        if(is.null(dim(x))) xpos <- x
        else if(p == 1) {
            xpos <- x[,1]
            if(is.null(xlab)) xlab <- colnames(x)  # default x-axis label
        } else {
            xpos <- x[, pos]  # plot variable
            xh <- x[, -pos, drop=FALSE]  # highlight variables
            if(is.null(xlab)) xlab <- colnames(x)[pos]  # default x-axis label
        }
        x.categorical <- is.categorical(xpos)
        xpos <- if(x.categorical) as.factor(xpos) else as.numeric(xpos)
       	if(p == 2 && is.null(ylab)) {  # default y-axis label
			if(!imputed) ylab <- paste("missing/observed in", colnames(x)[-pos])
			else ylab <- paste("imputed/observed in", colnames(x)[-pos])
		}
        if(x.categorical) {
            x.axis <- TRUE
            if(is.logical(labels)) {
                if(!is.na(labels) && labels) labels <- NULL
                else x.axis <- FALSE
            }
        } else x.axis <- axes
        miss.axis <- TRUE
        if(is.logical(miss.labels)) {
            if(!is.na(miss.labels) && miss.labels) miss.labels <- NULL
            else miss.axis <- FALSE
        }
        
		impp <- FALSE # indicates if the current variable has imputed missings
		# get missings/imputed missings and plot limits
		if(!imputed) { # spineMiss
			misspos <- isNA(xpos)
		} else { # spineImp
			tmp <- isImp(x, pos = pos, delimiter = delimiter, imp_var = imp_var, selection = selection)
			misspos <- tmp[["misspos"]]
			impp <- tmp[["impp"]]
			missh <- tmp[["missh"]]
		}
		
        # get missings
        if(p == 1) {
			misshf <- factor(rep.int(0, n), levels=1:0)
		} else {
			if(!imputed) missh <- isNA(xh, selection) # spineMiss
			misshf <- factor(ifelse(missh, 1, 0), levels=1:0)
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
            x.axis <- FALSE
            if(only.miss) {
                nx <- 1
                if(all(iInf)) {
                    x.at <- 1.08
                    plot(c(0, x.at), 0:1, type="n", axes=FALSE, ann=FALSE, 
                        yaxs="i", main="", sub="", xlab="", ylab="")
                    if(miss.axis) {
                        miss.at <- x.at
                        if(is.null(miss.labels)){
							if(!imputed) miss.labels <- "missing"
							else miss.labels <- "imputed"
						} 
                    }
                } else {
                    x.at <- 0.08 + 0:1
                    y.at <- c(0, cumsum(prop.table(table(misshf[!iInf]))))
                    xleft <- rep.int(x.at[1], 2)
                    ybottom <- y.at[1:2]
                    xright <- rep(x.at[2], 2)
                    ytop <- y.at[2:3]
                    xlim <- c(0, x.at[2])
                    plot(xlim, 0:1, type="n", axes=FALSE, ann=FALSE, 
                        yaxs="i", main="", sub="", xlab="", ylab="")
					if(!imputed) color <- col[4:3]
					else color <- col[c(6,3)]
                    rect(xleft, ybottom, xright, ytop, 
                        col=color, border=border, xpd=TRUE)
                    if(miss.axis) {
                        miss.at <- mean(x.at)
                        if(is.null(miss.labels)){
							if(!imputed) miss.labels <- "missing"
							else miss.labels <- "imputed"
                    	}
                	}
            	}
			} else {
                br <- 0:1
                xlim <- c(0, br[2]*1.19)
                plot(xlim, 0:1, type="n", axes=FALSE, ann=FALSE, 
                    yaxs="i", main="", sub="", xlab="", ylab="")
            }
        } else {
            useNA <- if(only.miss) "always" else "no"
            if(x.categorical) {
                tab <- table(xpos, misshf, useNA=useNA)[, 1:2, drop=FALSE]
                nx <- NROW(tab)
				# Workaround for imputed values
				if(any(misspos) && imputed && only.miss) {
					tab[nx,] <- table(misspos, misshf)[2,]
				}
		        # compute rectangle positions on x-axis
                if(only.miss) off <- c(rep.int(0.02, nx-2), 0.08, 0) 
                else off <- c(rep.int(0.02, nx-1), 0)
                x.at <- c(0, cumsum(prop.table(margin.table(tab, 1)) + off))
                # axis labels
                if(x.axis) {
                    if(only.miss) { 
                        x.t <- (x.at[1:(nx-1)]+x.at[2:nx]-off[1:(nx-1)])/2
                    } else x.t <- (x.at[-(nx+1)]+x.at[-1]-off)/2
                    if(is.null(labels)) labels <- levels(xpos)
                    else labels <- rep(labels, length.out=length(levels(xpos)))
                }
                if(only.miss && miss.axis) {
                    miss.at <- (x.at[nx]+x.at[nx+1]-off[nx])/2
                    if(is.null(miss.labels)){
						if(!imputed) miss.labels <- "missing"
						else miss.labels <- "imputed"
					}
                    else miss.labels <- rep(miss.labels, length.out=1)
                }
            } else {
                # compute breaks
                breaks <- hist(xpos[!iInf], breaks=breaks, plot=FALSE)$breaks
                # categorize x
                x1 <- cut(xpos[!iInf], breaks=breaks, 
                    include.lowest=TRUE, right=right)
                # construct table
                tab <- table(x1, misshf[!iInf], useNA=useNA)[, 1:2, drop=FALSE]
                nx <- NROW(tab)
				# Workaround for imputed values
				if(any(misspos) && imputed && only.miss) {
					tab[nx,] <- table(misspos, misshf)[2,]
					x1[n+length(which(misspos))] <- NA
				}
				
                # compute rectangle positions on x-axis
                off <- if(only.miss) c(rep.int(0, nx-2), 0.08, 0) else 0
                x.at <- c(0, cumsum(prop.table(table(x1, useNA=useNA)) + off))
                # axis labels
                if(x.axis) {
                    x.t <- if(only.miss) x.at[1:nx] - c(0, off[-nx]) else x.at
                    labels <- breaks
                }
                if(only.miss && miss.axis) {
                    miss.at <- (x.at[nx]+x.at[nx+1]-off[nx])/2
                    if(is.null(miss.labels)) {
						if(!imputed) miss.labels <- "missing"
						else miss.labels <- "imputed"
					}
                    else miss.labels <- rep(miss.labels, length.out=1)
                }
            }
           
            # compute rectangle positions on y-axis
            y.at <- rbind(0, apply(prop.table(tab, 1), 1, cumsum))
            
            # compute coordinates of rectangles
            ybottom <- as.vector(y.at[-3,])
            ytop <- as.vector(y.at[-1,])
            xleft <- rep(x.at[1:nx], rep(2, nx))
            xright <- rep(x.at[2:(nx+1)] - off, rep(2, nx))
            
            # get x-axis limits and colors for rectangles
            br <- c(0, 1 + sum(off))
			if(!imputed) {
					col1 <- col[2:1]
					col2 <- col[4:3]
				} else {
					col1 <- col[c(5,1)]
					col2 <- col[c(6,3)]
				}
            if(only.miss) {
                xlim <- br
                cols <- c(rep(col1, nx-1), col2)
            } else {
                xlim <- c(0, br[2]*1.19)
                cols <- rep(col1, nx)
            }

            # set up plot
            plot(xlim, 0:1, type="n", axes=FALSE, ann=FALSE, 
                yaxs="i", main="", sub="", xlab="", ylab="")
            # plot rectangles
            rect(xleft, ybottom, xright, ytop, 
                col=cols, border=border, xpd=TRUE)
        }
        
        # additional information about missings
        if(!only.miss) {  # spine plot for missings in both variables
            missposf <- factor(ifelse(misspos, 1, 0), levels=0:1)
            tab1 <- table(missposf, misshf)
            # rectangle positions
            off1 <- c(0.01, 0)
            x1.at <- br[2]*1.08 + 
                c(0, cumsum(prop.table(margin.table(tab1, 1))*br[2]*0.1 + off1))
            y1.at <- rbind(0, apply(prop.table(tab1, 1), 1, cumsum))
            # compute coordinates
            y1bottom <- as.vector(y1.at[-3,])
            y1top <- as.vector(y1.at[-1,])
            x1left <- rep(x1.at[1:2], c(2,2))
            x1right <- rep(x1.at[2:3] - off1, c(2,2))
			if(!imputed) color <- col[c(2,1,4,3)]
			else color <- col[c(5,1,6,3)]
            rect(x1left, y1bottom, x1right, y1top, 
                col=color, border=border, xpd=TRUE)
            if(miss.axis) {
                miss.at <- (x1.at[1:2]+x1.at[2:3] - off1)/2
                if(is.null(miss.labels)) {
					if(!imputed) miss.labels <- c("observed","missing")
					else miss.labels <- c("observed","imputed")
				} else miss.labels <- rep(miss.labels, length.out=2)
            }
        }
        
        # plot annotation
        if(axes) localAxis(side=2, ...)  # y-axis
        localTitle(main, sub, xlab, ylab, ...)  # plot annotation
        x.line <- if(only.miss) x.at[nx]-0.04 else br[2]*1.04
        abline(v=x.line, col="lightgrey")
        
        # x-axis
        if(x.axis || miss.axis) {
            has.las <- "dots" %in% nmdots
            dots$side <- 1
            dots$at <- c(if(x.axis) x.t, if(miss.axis) miss.at)
            dots$labels <- c(if(x.axis) labels, if(miss.axis) miss.labels)
            if(is.null(dots$line)) dots$line <- par("mgp")[3]
            if(x.categorical || has.las || par("las") %in% 2:3) {
                # can draw labels in one call to axis
                if(x.axis && !x.categorical) {
                    localAxis(side=1, at=x.t, labels=FALSE, ...)
                }
                x.axes <- TRUE
                dots$lty <- 0
                if(x.categorical && !has.las) dots$las <- 3
                las <- if(has.las) dots$las else par("las")
                if(las %in% 2:3) {
                    space.vert <- (par("oma")[1]+par("mar")[1]-
                            dots$line-par("mgp")[2])*par("csi")
                    ok <- prettyLabels(dots$labels, 
                        dots$at, space.vert, dots$cex.axis)
                    if(any(ok)) {
                        dots$at <- dots$at[ok]
                        dots$labels <- dots$labels[ok]
                    } else x.axes <- FALSE
                }
                if(x.axes) do.call(localAxis, dots)
            } else {
                # numeric labels parallel to axis, but labels for 
                # additional information about missings perpendicular
                which.axis <- c(if(x.axis) rep.int("x", length(labels)), 
                    if(miss.axis) rep.int("miss", length(miss.labels)))
                space.vert <- (par("oma")[1]+par("mar")[1]-
                        dots$line-par("mgp")[2])*par("csi")
                rotate <- which.axis == "miss"
                ok <- prettyLabels(dots$labels, dots$at, 
                    space.vert, dots$cex.axis, rotate)
                if(x.axis) {
                    x.ok <- ok[which.axis == "ok"]
                    labels[!x.ok] <- ""
                    localAxis(side=1, at=x.t, labels=labels, ...)
                }
                if(miss.axis) {
                    miss.ok <- ok[which.axis == "miss"]
                    if(any(miss.ok)) {
                        dots$at <- miss.at[miss.ok]
                        dots$labels <- miss.labels[miss.ok]
                        dots$lty <- 0
                        dots$las <- 3
                        do.call(localAxis, dots)
                    }
                }
            }
        }
        
        if(allNAInf) return() 
        else {
			if(!imputed) colnames(tab) <- c("missing","observed")
			else colnames(tab) <- c("imputed","observed")
            names(dimnames(tab)) <- NULL
            return(as.table(tab))
        }
    }
    s <- createPlot(main, sub, xlab, ylab, labels)
    
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
            s <- createPlot(labels=if(is.logical(labels)) labels else axes)
            usr <- par("usr")
            pt <- locatorVIM()
        }
    }
    invisible(s)
}
