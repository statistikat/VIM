# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner, Matthias Templ
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ----------------------------------------------------------



#' Parallel boxplots with information about missing/imputed values
#' 
#' Boxplot of one variable of interest plus information about missing/imputed
#' values in other variables.
#' 
#' This plot consists of several boxplots. First, a standard boxplot of the
#' variable of interest is produced. Second, boxplots grouped by observed and
#' missing/imputed values according to `selection` are produced for the
#' variable of interest.
#' 
#' Additionally, the frequencies of the missing/imputed values can be
#' represented by numbers.  If so, the first line corresponds to the observed
#' values of the variable of interest and their distribution in the different
#' groups, the second line to the missing/imputed values.
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
#' Additional variables in `x` are used for grouping according to
#' missingness/number of imputed missings.
#' @param selection the selection method for grouping according to
#' missingness/number of imputed missings in multiple additional variables.
#' Possible values are `"none"` (grouping according to missingness/number
#' of imputed missings in every other variable that contains missing/imputed
#' values), `"any"` (grouping according to missingness/number of imputed
#' missings in *any* of the additional variables) and `"all"`
#' (grouping according to missingness/number of imputed missings in *all*
#' of the additional variables).
#' @param col a vector of length five giving the colors to be used in the plot.
#' The first color is used for the boxplots of the available data, the
#' second/fourth are used for missing/imputed data, respectively, and the
#' third/fifth color for the frequencies of missing/imputed values in both
#' variables (see \sQuote{Details}).  If only one color is supplied, it is used
#' for the boxplots for missing/imputed data, whereas the boxplots for the
#' available data are transparent.  Else if two colors are supplied, the second
#' one is recycled.
#' @param numbers a logical indicating whether the frequencies of
#' missing/imputed values should be displayed (see \sQuote{Details}).
#' @param cex.numbers the character expansion factor to be used for the
#' frequencies of the missing/imputed values.
#' @param xlim,ylim axis limits.
#' @param main,sub main and sub title.
#' @param xlab,ylab axis labels.
#' @param axes a logical indicating whether axes should be drawn on the plot.
#' @param frame.plot a logical indicating whether a box should be drawn around
#' the plot.
#' @param labels either a logical indicating whether labels should be plotted
#' below each box, or a character vector giving the labels.
#' @param interactive a logical indicating whether variables can be switched
#' interactively (see \sQuote{Details}).
#' @param \dots for `pbox`, further arguments and graphical parameters to
#' be passed to [graphics::boxplot()] and other functions.  For
#' `TKRpbox`, further arguments to be passed to `pbox`.
#' @return a list as returned by [graphics::boxplot()].
#' @note Some of the argument names and positions have changed with version 1.3
#' due to extended functionality and for more consistency with other plot
#' functions in `VIM`.  For back compatibility, the arguments `names`
#' and `cex.text` can still be supplied to \code{\dots{}} and are handled
#' correctly.  Nevertheless, they are deprecated and no longer documented.  Use
#' `labels` and `cex.numbers` instead.
#' @author Andreas Alfons, Matthias Templ, modifications by Bernd Prantner
#' @seealso [parcoordMiss()]
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  *Journal of Advances in Data Analysis
#' and Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(chorizonDL, package = "VIM")
#' ## for missing values
#' pbox(log(chorizonDL[, c(4,5,8,10,11,16:17,19,25,29,37,38,40)]))
#' 
#' ## for imputed values
#' pbox(kNN(log(chorizonDL[, c(4,8,10,11,17,19,25,29,37,38,40)])),
#'      delimiter = "_imp")
#' 
#' @export pbox
pbox <- function(x, delimiter = NULL, pos = 1, selection = c("none","any","all"), 
                 col = c("skyblue","red","red4","orange","orange4"), numbers = TRUE, 
                 cex.numbers = par("cex"), xlim = NULL, ylim = NULL, 
                 main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
                 axes = TRUE, frame.plot = axes, labels = axes, 
                 interactive = TRUE, ...) {
  UseMethod("pbox", x)
}

#' @rdname pbox
#' @export

pbox.data.frame <- function(x, delimiter = NULL, pos = 1, selection = c("none","any","all"), 
                            col = c("skyblue","red","red4","orange","orange4"), numbers = TRUE, 
                            cex.numbers = par("cex"), xlim = NULL, ylim = NULL, 
                            main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
                            axes = TRUE, frame.plot = axes, labels = axes, 
                            interactive = TRUE, ...) {
  pbox_work(x, delimiter, pos, selection, col, numbers, cex.numbers, xlim, ylim, main, sub,
            xlab, ylab, axes, frame.plot, labels, interactive, ...)
}

#' @rdname pbox
#' @export

pbox.survey.design <- function(x, delimiter = NULL, pos = 1, selection = c("none","any","all"), 
                               col = c("skyblue","red","red4","orange","orange4"), numbers = TRUE, 
                               cex.numbers = par("cex"), xlim = NULL, ylim = NULL, 
                               main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
                               axes = TRUE, frame.plot = axes, labels = axes, 
                               interactive = TRUE, ...) {
  pbox_work(x$variables, delimiter, pos, selection, col, numbers, cex.numbers, xlim, ylim, main, sub,
            xlab, ylab, axes, frame.plot, labels, interactive, ...)
}

#' @rdname pbox
#' @export

pbox.default <- function(x, delimiter = NULL, pos = 1, selection = c("none","any","all"), 
                         col = c("skyblue","red","red4","orange","orange4"), numbers = TRUE, 
                         cex.numbers = par("cex"), xlim = NULL, ylim = NULL, 
                         main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
                         axes = TRUE, frame.plot = axes, labels = axes, 
                         interactive = TRUE, ...) {
  pbox_work(as.data.frame(x), delimiter, pos, selection, col, numbers, cex.numbers, xlim, ylim, main, sub,
            xlab, ylab, axes, frame.plot, labels, interactive, ...)
}

pbox_work <- function(x, delimiter = NULL, pos = 1, selection = c("none","any","all"), 
        col = c("skyblue","red","red4","orange","orange4"), numbers = TRUE, 
        cex.numbers = par("cex"), xlim = NULL, ylim = NULL, 
        main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
        axes = TRUE, frame.plot = axes, labels = axes, 
        interactive = TRUE, ...) {

    # initializations and error messages
	imputed <- FALSE # indicates if there are Variables with missing-index
    if(is.null(dim(x))) {  # vector
        n <- length(x)
        p <- 1
        if(n == 0) stop("'x' must have positive length")
    } else {  # matrix or data.frame
        if(!(inherits(x, c("data.frame","matrix")))) { 
            stop("'x' must be a data.frame or matrix")
        }
		
		##delimiterh ##
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
        pos <- 1
        interactive <- FALSE
    } else {
        if(!is.numeric(pos) || length(pos) != 1 ||(p < pos)) {
            stop("'pos' must be an integer specifying one column of 'x' and must be lesser than the number of colums of 'x'")
        }
        if(p == 2) selection <- "none"
        else selection <- match.arg(selection)
    }

    if(length(col) == 0) col = c("skyblue","red","red4","orange","orange4")
    else if(length(col) == 1) col <- c("transparent", rep.int(col, 4))
    else if(length(col) == 2 || length(col) == 4)  col <- c(col, rep(col[2],3))
	else if(length(col) != 5) col <- c(col[1],rep(col[2:3],2))
    
    # define local function and initialize call
    localBoxplot <- function(..., plot, log, 
        axes, frame.plot, horizontal, add) {
        boxplot(..., add=TRUE, axes=FALSE)
    }
    ca <- as.call(list(localBoxplot, ...))
    nmdots <- names(ca)[-1]
    
    # back compatibility
    if(missing(cex.numbers) && "cex.text" %in% nmdots) {
        cex.numbers <- ca$cex.text
    }
    if(missing(labels) && "names" %in% nmdots) labels <- ca$names
    
    # workhorse to create plot
    createPlot <- function(main=NULL, sub=NULL, 
            xlab=NULL, ylab=NULL, labels=axes, at=NULL) {
        
        # prepare data
        if(is.null(dim(x))) {
            xpos <- as.numeric(x)
            cn <- ""
        } else if(p == 1) {
            xpos <- as.numeric(x[,1])
            cn <- colnames(x)  # default x-axis label
        } else {
            xpos <- as.numeric(x[, pos])  # plot variable
            cn <- colnames(x)[pos]  # default x-axis label
        }
        
		# count missings/imputed missings
        if(p == 1) {
			if(!imputed) nNA <- countNA(xpos)
			else nNA <- countImp(x[, pos, drop = FALSE], delimiter, imp_var)
            nh <- 0  # no highlight variables with missings
        } else {
           	if(!imputed) nNA <- apply(x, 2, countNA)
			else nNA <- countImp(x, delimiter, imp_var)
            # number of highlight variables with missings
            nh <- length(which(nNA[-pos] > 0))
        }
		
        # plot x-axis?
        x.axis <- nh > 0
        if(is.logical(labels)) {
            if(is.na(labels) || !labels) x.axis <- FALSE
            labels <- NULL
        }
        
        # get positions on x-axis for boxes
        if(nh) {
            if(selection == "none") {
                xh <- x[, 1:p != pos & nNA > 0, drop=FALSE]  # highlight vars
                nb <- 1 + 2*nh  # number of boxes to draw
                ind1 <- seq(from=2, by=2, length.out=nh)
                ind2 <- seq(from=3, by=2, length.out=nh)
                ord <- order(c(1, ind1, ind2))
                if(length(at) == 0) {
                    at1 <- (1:nh)*3
                    at2 <- (1:nh)*3+1
                    at <- c(1, at1, at2)[ord]
                } else if(length(at) != nb) {
                    stop(gettextf("'at' must have length %i", nb))
                }
                if(length(labels) == 0) {
                    labels1 <- labels2 <- colnames(xh)
                    labels1 <- paste("obs. in", labels1)
					if(!imputed) labels2 <- paste("miss. in", labels2)
					else labels2 <- paste("imp. in", labels2)
                    labels <- c(cn, labels1, labels2)[ord]
                } else if(length(labels) != nb) {
                    stop(gettextf("'labels' must have length %i", nb))
                }
                # indices of missings/imputed missings (for each variable that contains missings/imputed missings)
                if(!imputed) i <- lapply(xh, function(x) is.na(x))
				else {
					tmp <- isImp(cbind(x[,pos, drop = FALSE],xh), pos = 1, delimiter, imp_var, selection)
					i <- as.list(as.data.frame(tmp[["missh"]]))
					misspos <- tmp[["misspos"]]
				}
                boxlist1 <- lapply(i, function(i,x) x[!i], xpos)
                boxlist2 <- lapply(i, function(i,x) x[i], xpos)
                boxlist <- c(list(xpos), boxlist1, boxlist2)[ord]
            } else {
                xh <- x[, -pos, drop=FALSE]  # highlight vars
                nb <- 3
                if(length(at) == 0) at <- c(1, 3, 4)
                else if(length(at) != 3) stop("'at' must have length 3")
                if(length(labels) == 0) {
                    if(selection == "any") {
						if(!imputed) labels <- c(cn, "all observed", "any missing")
						else labels <- c(cn, "all observed", "any imputed")
                    } else {
						if(!imputed) labels <- c(cn, "any observed", "all missing")
						else labels <- c(cn, "any observed", "all imputed")
					}
                }
                else if(length(labels) != 3) stop("'labels' must have length 3")
                if(!imputed) i <- isNA(xh, selection)  # indices of missings
				else { # indices of imputed missings
					tmp <- isImp(x, pos = pos, delimiter, imp_var, selection)
					i <- tmp[["missh"]]
					misspos <- tmp[["misspos"]]
				}
				
                boxlist <- list(xpos, observed=xpos[!i], missing=xpos[i])
            }
        } else {
            nb <- 1
            if(length(at) == 0) at <- 1
            else if(length(at) != 1) stop("'at' must have length 1")
            if(length(labels) == 0) labels <- cn
            else if(length(labels) != 1) stop("'labels' must have length 1")
        }
        
        # lower margin might have to be adjusted
        if(is.null(ca$mar)) {
            mar <- par("mar")
            if(x.axis && (is.null(ca$las) || 
                    (!is.null(ca$las) && ca$las %in% 2:3))) {
                # missings in at least one of the highlight variables, 
                # hence we need to extend the lower plot margin
                if(selection == "none") mar[1] <- mar[1] + 3
                else mar[1] <- mar[1] + 1.5
            }
        } else mar <- ca$mar
        op <- par(mar=mar)
        on.exit(par(op))
        
        # default axis limits
#        if(is.null(xlim)) {
#            if(!is.null(at)) xlim <- c(min(at)-0.5, max(at)+0.5)
#            else xlim <- c(0.5, 1.5+nh*3)
#        }
        if(is.null(xlim)) xlim <- c(min(at)-0.5, max(at)+0.5)
        # check for infinite values
        iInf <- is.infinite(xpos)  # indicates infinite values
        anyInf <- any(iInf)
		if(!imputed || nh == 0) misspos <- is.na(xpos)
        allNAInf <- all(misspos | iInf)
        if(anyInf) {
            if(is.null(dim(x))) cnw <- "'x'" 
            else cnw <- paste("variable '", cn, "'", sep="")
            warning(cnw, " contains infinite values")
        }
        if(is.null(ylim)) {
            if(allNAInf) ylim <- rep.int(0, 2)
            else if(anyInf) ylim <- range(xpos[!iInf], na.rm=TRUE)
            else ylim <- range(xpos, na.rm=TRUE)
        }
        
        # initialize plot
        initializeWindow <- function(..., range, notch, width, 
                varwidth, outline, names, plot, notch.frac, border, 
                frame.plot, horizontal, add, at, show.names, pars, 
                cex.text, col, bg, pch, cex, lty, lwd) {
            plot.new()
            plot.window(...)
        }
        initializeWindow(xlim, ylim, ...)
        
        if(numbers){
            if(nh) {
                num <- lapply(boxlist, length)
				if(!imputed) numNA <- lapply(boxlist, countNA)
				else if(selection == "none") {
					numNA1 <- lapply(i, function(i,x) length(which(x[!i])), misspos)
					numNA2 <- lapply(i, function(i,x) length(which(x[i])), misspos)
					numNA <- c(length(which(misspos)), numNA1, numNA2)[ord]
				} else {
					numNA <- list(length(which(misspos)), length(which(misspos[!i])), length(which(misspos[i])))
					names(numNA) <- names(boxlist)
				}
            } else {
                num <- n
                numNA <- nNA[pos]
            }
            space.vert <- par("pin")[2]*0.05/1.1
            num.ok <- prettyLabels(num, at, space.vert, 
                cex.numbers, rotate=rep(FALSE, nb), xlim=par("usr")[1:2])
            numNA.ok <- prettyLabels(numNA, at, space.vert, 
                cex.numbers, rotate=rep(FALSE, nb), xlim=par("usr")[1:2])
            if(!any(c(num.ok, numNA.ok))) {
                numbers <- FALSE
                warning("not enough space to display frequencies")
            }
        }
        if(numbers) {
            # define grid
            # order of graphical parameters matters
            op <- c(op, par(c("xlog", "ylog", "plt", "usr", "yaxp")))
            plty <- c(op$plt[4] - diff(op$plt[3:4])/c(1, 1.1), op$plt[4])
            # extend usr coordinates
            gridy <- c(op$usr[3] - c(0.1, 0)*diff(op$usr[3:4]), op$usr[4])
            op$usr <- c(op$usr[1:2], gridy[c(1,3)])
            # set plot region for boxplots
            par(plt=c(op$plt[1:2], plty[2:3]), usr=c(op$usr[1:2], gridy[2:3]))
        }
        
		if(!imputed){
			colNum <- rep.int(col[1:2], nh)
			colNA <- c(col[2], rep.int(col[2:3], nh))
		} else {
			colNum <- rep.int(col[c(1,4)], nh)
			colNA <-  c(col[4], rep.int(col[4:5], nh))
		}
        # create boxplots
        if(allNAInf) b <- NULL        
        else {
            if(nh) {
                ca$x <- lapply(boxlist, function(x) x[!is.infinite(x)])
                if(is.null(ca$varwidth)) ca$varwidth <- TRUE
                ca$names <- labels
                ca$col <- c("transparent", colNum)
                ca$at <- at
                b <- eval(ca)
            } else {
                ca$x <- xpos[!iInf]
                ca$names <- labels
                ca$col <- "transparent"
                ca$at <- at
                b <- eval(ca)
            }
        }
        
        if(numbers) {
            par(xlog=op$xlog, ylog=FALSE, 
                plt=op$plt, usr=c(op$usr[1:2], c(0,1.1)))
            box(col="transparent")  # reset clipping region
            abline(h=0.1, col="lightgrey")
            num[!num.ok] <- ""
            text(at, 0.075, labels=num, cex=cex.numbers, 
                col=c(par("fg"), colNum))
            numNA[!numNA.ok] <- ""
            text(at, 0.025, labels=numNA, cex=cex.numbers, 
                col=colNA)
            par(op[c("xlog", "ylog", "plt", "usr", "yaxp")]) # reset plot region
        }
        
        # axes and box
        if(x.axis) {
            localXAxis <- function(..., range, notch, width, varwidth, 
                outline, names, plot, notch.frac, log, border, 
                frame.plot, horizontal, add, show.names, pars, 
                cex.text, col, bg, pch, cex, lty, lwd) {
                axis(..., lty=0)
            }
            dots <- list(...)
            dots$side <- 1
            dots$at <- at
            dots$labels <- c("", labels[-1])
            if(is.null(dots$line)) dots$line <- par("mgp")[3]
            if(is.null(dots$las)) dots$las <- 3
            if(dots$las %in% 2:3) {
                space.vert <- (par("oma")[1]+par("mar")[1]-
                        dots$line-par("mgp")[2])*par("csi")
                ok <- prettyLabels(dots$labels, dots$at, 
                    space.vert, dots$cex.axis)
                if(any(ok)) {
                    dots$at <- dots$at[ok]
                    dots$labels <- dots$labels[ok]
                } else x.axis <- FALSE
            }
        }
        if(x.axis) do.call(localXAxis, dots)
        if(axes) {
            localYAxis <- function(..., range, notch, width, varwidth, 
                outline, names, plot, notch.frac, log, border, 
                frame.plot, horizontal, add, at, show.names, 
                pars, cex.text, col, bg, pch, cex, lty, lwd) {
                axis(...)
            }
            localYAxis(side=2, ...)  # y-axis
        }
        if(frame.plot) {
            localBox <- function(..., range, notch, width, varwidth, 
                outline, names, plot, notch.frac, log, border, 
                frame.plot, horizontal, add, at, show.names, 
                pars, cex.text, col, bg, pch, cex, lty, lwd) {
                box(...)
            }
            localBox(...)
        }
        localTitle <- function(..., range, notch, width, varwidth, 
            outline, names, plot, notch.frac, log, border, 
            frame.plot, horizontal, add, at, show.names, 
            pars, cex.text, col, bg, pch, cex, lty, lwd) {
            title(...)
        }
        if(is.null(ylab)) ylab <- cn
        localTitle(main, sub, xlab, ylab, ...)
        return(b)
    }
    b <- createPlot(main, sub, xlab, ylab, labels, ca$at)
    
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
            b <- createPlot(labels=if(is.logical(labels)) labels else axes)
            usr <- par("usr")
            pt <- locatorVIM()
        }
    }
    
    invisible(b)
}
