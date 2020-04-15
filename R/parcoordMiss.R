# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner, Matthias Templ
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ----------------------------------------------------------



#' Parallel coordinate plot with information about missing/imputed values
#' 
#' Parallel coordinate plot with adjustments for missing/imputed values.
#' Missing values in the plotted variables may be represented by a point above
#' the corresponding coordinate axis to prevent disconnected lines. In
#' addition, observations with missing/imputed values in selected variables may
#' be highlighted.
#' 
#' In parallel coordinate plots, the variables are represented by parallel
#' axes.  Each observation of the scaled data is shown as a line.  Observations
#' with missing/imputed values in selected variables may thereby be
#' highlighted.  However, plotting variables with missing values results in
#' disconnected lines, making it impossible to trace the respective
#' observations across the graph.  As a remedy, missing values may be
#' represented by a point above the corresponding coordinate axis, which is
#' separated from the main plot by a small gap and a horizontal line, as
#' determined by `plotNA`.  Connected lines can then be drawn for all
#' observations.  Nevertheless, a caveat of this display is that it may draw
#' attention away from the main relationships between the variables.
#' 
#' If `interactive` is `TRUE`, it is possible switch between this
#' display and the standard display without the separate level for missing
#' values by clicking in the top margin of the plot. In addition, the variables
#' to be used for highlighting can be selected interactively.  Observations
#' with missing/imputed values in any or in all of the selected variables are
#' highlighted (as determined by `selection`).  A variable can be added to
#' the selection by clicking on a coordinate axis.  If a variable is already
#' selected, clicking on its coordinate axis removes it from the selection.
#' Clicking anywhere outside the plot region (except the top margin, if
#' missing/imputed values exist) quits the interactive session.
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
#' @param col if `plotNA` is `TRUE`, a vector of length six giving
#' the colors to be used for observations with different combinations of
#' observed and missing/imputed values in the plot variables and highlight
#' variables (vectors of length one or two are recycled).  Otherwise, a vector
#' of length two giving the colors for non-highlighted and highlighted
#' observations (if a single color is supplied, it is used for both).
#' @param plotNA a logical indicating whether missing values in the plot
#' variables should be represented by a point above the corresponding
#' coordinate axis to prevent disconnected lines.
#' @param alpha a numeric value between 0 and 1 giving the level of
#' transparency of the colors, or `NULL`.  This can be used to prevent
#' overplotting.
#' @param lty if `plotNA` is `TRUE`, a vector of length four giving
#' the line types to be used for observations with different combinations of
#' observed and missing/imputed values in the plot variables and highlight
#' variables (vectors of length one or two are recycled).  Otherwise, a vector
#' of length two giving the line types for non-highlighted and highlighted
#' observations (if a single line type is supplied, it is used for both).
#' @param xlim,ylim axis limits.
#' @param main,sub main and sub title.
#' @param xlab,ylab axis labels.
#' @param labels either a logical indicating whether labels should be plotted
#' below each coordinate axis, or a character vector giving the labels.
#' @param xpd a logical indicating whether the lines should be allowed to go
#' outside the plot region.  If `NULL`, it defaults to `TRUE` unless
#' axis limits are specified.
#' @param interactive a logical indicating whether interactive features should
#' be enabled (see \sQuote{Details}).
#' @param \dots for `parcoordMiss`, further graphical parameters to be
#' passed down (see [graphics::par()]).  For `TKRparcoordMiss`,
#' further arguments to be passed to `parcoordMiss`.
#' @note Some of the argument names and positions have changed with versions
#' 1.3 and 1.4 due to extended functionality and for more consistency with
#' other plot functions in `VIM`.  For back compatibility, the arguments
#' `colcomb` and `xaxlabels` can still be supplied to \code{\dots{}}
#' and are handled correctly.  Nevertheless, they are deprecated and no longer
#' documented.  Use `highlight` and `labels` instead.
#' @author Andreas Alfons, Matthias Templ, modifications by Bernd Prantner
#' @seealso [pbox()]
#' @references Wegman, E. J. (1990) Hyperdimensional data analysis using
#' parallel coordinates. *Journal of the American Statistical Association*
#' **85 (411)**, 664--675.
#' 
#' M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete data using
#' visualization tools.  *Journal of Advances in Data Analysis and
#' Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @family plotting functions
#' @examples
#' 
#' data(chorizonDL, package = "VIM")
#' ## for missing values
#' parcoordMiss(chorizonDL[,c(15,101:110)], 
#'     plotvars=2:11, interactive = FALSE)
#' legend("top", col = c("skyblue", "red"), lwd = c(1,1), 
#'     legend = c("observed in Bi", "missing in Bi"))
#' 
#' ## for imputed values
#' parcoordMiss(kNN(chorizonDL[,c(15,101:110)]), delimiter = "_imp" ,
#'     plotvars=2:11, interactive = FALSE)
#' legend("top", col = c("skyblue", "orange"), lwd = c(1,1), 
#'     legend = c("observed in Bi", "imputed in Bi"))
#' 
#' @export parcoordMiss
parcoordMiss <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                         plotvars = NULL, plotNA = TRUE, 
                         col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                         alpha = NULL, lty = par("lty"), xlim = NULL, 
                         ylim = NULL, main = NULL, sub = NULL, 
                         xlab = NULL, ylab = NULL, labels = TRUE, 
                         xpd = NULL, interactive = TRUE, ...) {
  UseMethod("parcoordMiss", x)
}

#' @rdname parcoordMiss
#' @export

parcoordMiss.data.frame <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                                    plotvars = NULL, plotNA = TRUE, 
                                    col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                    alpha = NULL, lty = par("lty"), xlim = NULL, 
                                    ylim = NULL, main = NULL, sub = NULL, 
                                    xlab = NULL, ylab = NULL, labels = TRUE, 
                                    xpd = NULL, interactive = TRUE, ...) {
  parcoordMiss_work(x, delimiter, highlight, selection, plotvars, plotNA, col,
                    alpha, lty, xlim, ylim, main, sub, xlab, ylab, labels,
                    xpd, interactive, ...)
}

#' @rdname parcoordMiss
#' @export

parcoordMiss.survey.design <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                                       plotvars = NULL, plotNA = TRUE, 
                                       col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                       alpha = NULL, lty = par("lty"), xlim = NULL, 
                                       ylim = NULL, main = NULL, sub = NULL, 
                                       xlab = NULL, ylab = NULL, labels = TRUE, 
                                       xpd = NULL, interactive = TRUE, ...) {
  parcoordMiss_work(x$variables, delimiter, highlight, selection, plotvars, plotNA, col,
                    alpha, lty, xlim, ylim, main, sub, xlab, ylab, labels,
                    xpd, interactive, ...)
}

#' @rdname parcoordMiss
#' @export

parcoordMiss.default <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                                 plotvars = NULL, plotNA = TRUE, 
                                 col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                 alpha = NULL, lty = par("lty"), xlim = NULL, 
                                 ylim = NULL, main = NULL, sub = NULL, 
                                 xlab = NULL, ylab = NULL, labels = TRUE, 
                                 xpd = NULL, interactive = TRUE, ...) {
  parcoordMiss_work(as.data.frame(x), delimiter, highlight, selection, plotvars, plotNA, col,
                    alpha, lty, xlim, ylim, main, sub, xlab, ylab, labels,
                    xpd, interactive, ...)
}

parcoordMiss_work <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
        plotvars = NULL, plotNA = TRUE, 
		col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
        alpha = NULL, lty = par("lty"), xlim = NULL, 
        ylim = NULL, main = NULL, sub = NULL, 
        xlab = NULL, ylab = NULL, labels = TRUE, 
        xpd = NULL, interactive = TRUE, ...) {
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
    if(length(highlight) > ncol(x)) stop("'highlight' is too long")
    if(length(plotvars) > ncol(x)) stop("'plotvars' is too long")
    z <- if(is.null(plotvars)) x else x[, plotvars, drop=FALSE]
    pz <- ncol(z)
    if(pz < 2) stop("the data to be plotted must be at least 2-dimensional")
    selection <- match.arg(selection)
    plotNA <- isTRUE(plotNA)
    if(length(col) == 0) col <- c("skyblue","red","skyblue4","red4","orange","orange4")
    if(length(lty) == 0) lty <- par("lty")
    if(length(col) == 1 && length(lty) == 1) {
        stop("same color and line type for observed and missing values")
    }
    if(length(col) == 1) col <- rep.int(col, 6)
    else if(length(col) == 3 || length(col) == 5) col <- rep.int(col[1:2], 3)
    else if(length(col) != 6) col <- rep(col, length.out=6)
    if(length(lty) == 1) lty <- rep.int(lty, 4)
    else if(length(lty) == 3) lty <- rep.int(lty[1:2], 2)
    else if(length(lty) != 4) lty <- rep(lty, length.out=4)
    # semitransparent colors
    if(!is.null(alpha)) col <- alphablend(col, alpha)  
    # prepare data
    if(is.data.frame(z)) z <- data.matrix(z)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
    if(!imputed) missz <- isNA(z, selection="any")
	else missz <- isImp(z, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
    haveNA <- any(missz)
    # default axis limits
    if(is.null(xpd)) xpd <- is.null(xlim) && is.null(ylim)
    if(is.null(xlim)) xlim <- c(1, pz)
    setYlim <- is.null(ylim)
    # back compatibility
    dots <- list(...)
    nmdots <- names(dots)
    if(missing(highlight) && "colcomb" %in% nmdots) {
        if(length(dots$colcomb) && dots$colcomb[1] == "missnonmiss") {
            highlight <- NULL
        } else highlight <- dots$colcomb
    }
    if(missing(labels) && "xaxlabels" %in% nmdots) labels <- dots$xaxlabels
    localWindow <- function(..., colcomb, xaxlabels, log, asp, yaxs) {
        plot.window(..., yaxs=if(is.null(dots$yaxs)) "i" else dots$yaxs)
    }
    localLines <- function(..., colcomb, xaxlabels) lines(...)
    localAxis <- function(..., colcomb, xaxlabels) axis(...)
    localTitle <- function(..., colcomb, xaxlabels) title(...)
    # plot variable names on x-axis?
    x.axis <- TRUE
    if(is.logical(labels)) {
        if(isTRUE(labels)) labels <- NULL
        else x.axis <- FALSE
    }
    # check for infinite values
    iInf <- is.infinite(z)
    for(i in 1:pz) {
        if(any(iInf[, i])) {
            warning(gettextf("variable '%s' contains infinite values", 
                    colnames(z)[i]))
        }
    }
    createPlot <- function() {
        # additional initializations
        showNA <- plotNA && haveNA
        if(showNA){
            yNA <- 1.08
            if(setYlim) ylim <- c(0, yNA)
        } else if(setYlim) ylim <- 0:1
        # find observations with missings in highlight variables
        if(is.null(highlight)) {
			if(!imputed) missh <- isNA(x, selection)
			else missh <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
		} else {
			if(!imputed) missh <- isNA(x[, highlight, drop=FALSE], selection)
			else missh <- isImp(x[, highlight, drop=FALSE], pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
		}
        # initialize plot
        plot.new()
        localWindow(xlim, ylim, ...)
        # get range and transform variables
        sz <- apply(z, 2, 
            function(x) {
                if(!imputed) iNA <- is.na(x)
				else iNA <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = "none")[["missh"]]
                s <- if(showNA) ifelse(iNA, yNA, NA) else rep.int(NA, length(x))
                if(any(!iNA)) {
                    iInf <- is.infinite(x)
                    if(any(!iInf)) {
                        r <- range(x[!iInf], na.rm=TRUE)
                        s[!iNA & !iInf] <- (x[!iNA & !iInf]-r[1])/(r[2]-r[1])
                    }
                    s[iInf & x == -Inf] <- 0
                    s[iInf & x == Inf] <- 1
                }
                s
            })
        # plot spearator for NAs in plot variables
        if(showNA) {
            ysep <- (yNA+1)/2
            lines(xlim, rep.int(ysep, 2), col="lightgrey")
            # plot observations with missings only in plot variables
            ind <- which(missz & !missh)
            if(nind <- length(ind)) {
                xobs <- rep.int(c(1:pz, NA), nind)
                szobs <- as.vector(t(cbind(sz[ind, , drop=FALSE], NA)))
                localLines(xobs, szobs, col=col[3], lty=lty[3], xpd=xpd, ...)
            }
            # indices for regular observations
            ind <- which(!missz & !missh)
        } else ind <- which(!missh)
        # plot regular observations
        if(nind <- length(ind)) {
            xobs <- rep.int(c(1:pz, NA), nind)
            szobs <- as.vector(t(cbind(sz[ind, , drop=FALSE], NA)))
            localLines(xobs, szobs, col=col[1], lty=lty[1], xpd=xpd, ...)
        }
        if(showNA) {
            # plot observations with missings in plot and highlight variables
            ind <- which(missz & missh)
            if(nind <- length(ind)) {
                xobs <- rep.int(c(1:pz, NA), nind)
                szobs <- as.vector(t(cbind(sz[ind, , drop=FALSE], NA)))
				if(!imputed) color <- col[4]
				else color <- col[6]
                localLines(xobs, szobs, col=color, lty=lty[4], xpd=xpd, ...)
            }
            # indices for observations with missings only in highlight variables
            ind <- which(!missz & missh)
        } else ind <- which(missh)
        # plot observations with missings only in highlight variables
        if(nind <- length(ind)) {
            xobs <- rep.int(c(1:pz, NA), nind)
            szobs <- as.vector(t(cbind(sz[ind, , drop=FALSE], NA)))
			if(!imputed) color <- col[2]
			else color <- col[5]
            localLines(xobs, szobs, col=color, lty=lty[2], xpd=xpd, ...)
        }
        # plot coordinate axes
        lines(as.vector(rbind(1:pz, 1:pz, NA)), rep.int(c(0,1,NA), pz), 
            col="lightgrey", xpd=xpd)
        # x-axis
        if(x.axis) {
            dots$side <- 1
            dots$at <- 1:pz
            if(is.null(labels)) dots$labels <- colnames(z) 
            else dots$labels <- rep(labels, length.out=pz)
            dots$lty <- 0
            if(is.null(dots$las)) dots$las <- 2
            if(dots$las %in% 2:3) {
                space.vert <- (par("mar")[1]+par("oma")[1]-1)*par("csi")
                ok <- prettyLabels(dots$labels, dots$at, 
                    space.vert, dots$cex.axis)
                if(any(ok)) {
                    dots$at <- dots$at[ok]
                    dots$labels <- dots$labels[ok]
                } else x.axis <- FALSE
            }
        }
        if(x.axis) do.call(localAxis, dots)  # x-axis
        localTitle(main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
    }
    createPlot()
    interactiveDevices <- c("X11","quartz","windows")
    dev <- names(dev.cur())
    if(interactive && any(!is.na(charmatch(interactiveDevices, dev)))) {
        cat(paste("\nClick on a coordinate axis to add to", 
                "or remove from the highlight selection.\n"))
        if(haveNA) {
			if(!imputed) label <- "missing"
			else label <- "imputed missing"
            cat(paste("Click in the top margin to toggle visualizing", 
                    label, " values in the plot variables.\n"))
            cat(paste("To regain use of the VIM GUI and the R console,",
                    "click in any of the other plot margins.\n\n"))
        } else {
            cat(paste("To regain use of the VIM GUI and the R console,",
                    "click outside the plot region.\n\n"))
        }
        # initializations for selection
        cn <- colnames(x)
        if(is.null(highlight)) highlight <- cn
        else if(!is.character(highlight)) highlight <- cn[highlight]
        plotvars <- colnames(z)
        # start interactive session
        highlightInfo(highlight, selection, imputed)  # print out current selection
        usr <- par("usr")
        pt <- locatorVIM(error=TRUE)
        while(!is.null(pt) && class(pt) != "try-error" && 
                max(1, usr[1]) <= pt$x && pt$x < min(px, usr[2]) && 
                max(0, usr[3]) <= pt$y && 
                if(haveNA) TRUE else pt$y <= min(1, usr[4])) {
            if(pt$y <= min(1, usr[4])) {
                # variable selected or deselected
                i <- round(pt$x)
                highlight <- 
                    if(plotvars[i] %in% highlight) 
                        setdiff(highlight, plotvars[i]) 
                    else c(highlight, plotvars[i])
                createPlot()
                highlightInfo(highlight, selection, imputed)  # print out current selection
            } else {
                # toggle separate NA level for missings in plot variables
                plotNA <- !plotNA
                createPlot()
            }
            pt <- locatorVIM(error=TRUE)
        }
        if(class(pt) == "try-error") on.exit()
    }
    invisible()
}
