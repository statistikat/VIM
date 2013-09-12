# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner, Matthias Templ
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ----------------------------------------------------------

parcoordMiss <- function(x, delimiter = NULL, highlight = NULL, selection = c("any","all"), 
                         plotvars = NULL, plotNA = TRUE, 
                         col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                         alpha = NULL, lty = par("lty"), xlim = NULL, 
                         ylim = NULL, main = NULL, sub = NULL, 
                         xlab = NULL, ylab = NULL, labels = TRUE, 
                         xpd = NULL, interactive = TRUE, ...) {
  UseMethod("parcoordMiss", x)
}

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
