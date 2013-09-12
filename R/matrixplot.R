# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner, Matthias Templ
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ----------------------------------------------------------

matrixplot <- function(x, delimiter = NULL, sortby = NULL,
                       col = c("red","orange"),
                       gamma = 2.2, 
                       fixup = TRUE, xlim = NULL, ylim = NULL, 
                       main = NULL, sub = NULL, xlab = NULL, 
                       ylab = NULL, axes = TRUE, labels = axes, 
                       xpd = NULL, interactive = TRUE, ...) {
  UseMethod("matrixplot", x)
}

matrixplot.data.frame <- function(x, delimiter = NULL, sortby = NULL,
                                  col = c("red","orange"),
                                  gamma = 2.2, 
                                  fixup = TRUE, xlim = NULL, ylim = NULL, 
                                  main = NULL, sub = NULL, xlab = NULL, 
                                  ylab = NULL, axes = TRUE, labels = axes, 
                                  xpd = NULL, interactive = TRUE, ...) {
  matrixplot_work(x, delimiter, sortby, col, gamma, fixup, xlim, ylim,
                  main, sub, xlab, ylab, axes, labels, xpd, interactive, ...)
}

matrixplot.survey.design <- function(x, delimiter = NULL, sortby = NULL,
                                     col = c("red","orange"),
                                     gamma = 2.2, 
                                     fixup = TRUE, xlim = NULL, ylim = NULL, 
                                     main = NULL, sub = NULL, xlab = NULL, 
                                     ylab = NULL, axes = TRUE, labels = axes, 
                                     xpd = NULL, interactive = TRUE, ...) {
  matrixplot_work(x$variables, delimiter, sortby, col, gamma, fixup, xlim, ylim,
                  main, sub, xlab, ylab, axes, labels, xpd, interactive, ...)
}

matrixplot.default <- function(x, delimiter = NULL, sortby = NULL,
                               col = c("red","orange"),
                               gamma = 2.2, 
                               fixup = TRUE, xlim = NULL, ylim = NULL, 
                               main = NULL, sub = NULL, xlab = NULL, 
                               ylab = NULL, axes = TRUE, labels = axes, 
                               xpd = NULL, interactive = TRUE, ...) {
  matrixplot_work(as.data.frame(x), delimiter, sortby, col, gamma, fixup, xlim, ylim,
                  main, sub, xlab, ylab, axes, labels, xpd, interactive, ...)
}

matrixplot_work <- function(x, delimiter = NULL, sortby = NULL,
		col = c("red","orange"), 
        #space = c("rgb", "hcl"), 
        gamma = 2.2, 
        fixup = TRUE, xlim = NULL, ylim = NULL, 
        main = NULL, sub = NULL, xlab = NULL, 
        ylab = NULL, axes = TRUE, labels = axes, 
        xpd = NULL, interactive = TRUE, ...) {
    # initializations and error messages
    if(!(inherits(x, c("data.frame","matrix")))) 
        stop("'x' must be a data.frame or matrix")
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
    if(!is.null(sortby) && length(sortby) != 1) 
        stop("'sortby' must have length 1")
    # prepare data
    if(is.data.frame(x)) x <- data.matrix(x)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
    if(is.null(colnames(x))) colnames(x) <- defaultNames(p)
    # check for infinite values
    iInf <- is.infinite(x)
    for(i in 1:p) {
        if(any(iInf[, i])) {
            warning(gettextf("variable '%s' contains infinite values", 
                    colnames(x)[i]))
        }
    }
    # define rectangles
    xl <- (1:p)-0.5
    xr <- (1:p)+0.5
    yb <- (1:n)-0.5
    yt <- (1:n)+0.5
    rects <- merge(data.frame(yb,yt), data.frame(xl,xr))
    # check colors
    if(!is(col, "RGB") && !is(col, "polarLUV") && 
        (!is.character(col) || length(col) == 0)) col <- c("red","orange")
    if(is.character(col)) {
        # colors given as character string
        if(length(col) == 1) {
            start <- par("bg")
            end <- "black"
			col <- rep(col,2)
        } else if(length(col) == 2) {
			start <- par("bg")
			end <- "black"
		}else if(length(col) == 3) {
            start <- par("bg")
            end <- col[1]
            col <- col[2:3]
        } else {
            start <- col[1]
            end <- col[2]
            col <- col[3:4]
        }
        space <- "rgb"
    } else {
        space <- if(is(col, "RGB")) "rgb" else "hcl"
        if(nrow(coords(col)) == 1) {
            if(is(col, "RGB")) {
                # RGB colors
                start <- par("bg")
                end <- "black"
            } else {
                # HCL colors
                start <- c(100, 0, col@coords[1, "H"])
                end <- c(0, 0, col@coords[1, "H"])
            }
            col <- rep(hex(col, gamma=gamma, fixup=fixup),2)
        } else if(nrow(coords(col)) == 2) {
			if(is(col, "RGB")){
				# RGB colors
				start <- par("bg")
				end <- "black"
			} else {
				# HCL colors
				start <- c(100, 0, col@coords[1, "H"])
				end <- c(0, 0, col@coords[1, "H"])
			}
			col <- hex(col, gamma=gamma, fixup=fixup)
		} else if(nrow(coords(col)) == 3) {
            if(is(col, "RGB")){
                # RGB colors
                start <- par("bg")
            } else {
                # HCL colors
                start <- polarLUV(100, 0, col@coords[1, "H"])
            }
            end <- col[1,]
			col <- hex(col[2:3,], gamma=gamma, fixup=fixup)
        } else {
            start <- col[1,]
            end <- col[2,]
            col <- hex(col[3:4,], gamma=gamma, fixup=fixup)
        }
    }
    if(is.character(start)) startcol <- start 
    else startcol <- hex(start, gamma=gamma, fixup=fixup)
    if(is.character(end)) endcol <- end 
    else endcol <- hex(end, gamma=gamma, fixup=fixup)
    # function to get color sequence (or red/orange if missing/imputed)
	getCol <- function(x, ord = NULL) {
        iOK <- !is.na(x)
		cols <- rep.int(col[1], n)
		if(imputed) {
			# character vector for possible prefixes for the delimiter
			escape <- getEscapeChars()
			# search escape-vector for possible prefixes
			for(i in 1:length(escape)) {
				indexp <- colnames(imp_var) %in% paste(colnames(x),delimiter,sep=escape[i])
				# end loop if a match is found
				if(any(indexp))	break
			}
			if(any(indexp)) {
				iOK <- !imp_var[,indexp]
				if(!is.null(ord)) iOK <- iOK[ord]
				cols <- rep.int(col[2], n)
			}
		}
        if(any(iOK)) {
            iInf <- is.infinite(x)
            if(any(!iInf)) {
                r <- range(x[!iInf], na.rm=TRUE)
                if(r[1] == r[2]) xs <- rep.int(r[1], length(which(iOK & !iInf)))
                else xs <- (x[iOK & !iInf]-r[1])/(r[2]-r[1])
                cols[iOK & !iInf] <- colSequence(xs, start, end, space=space)
            }
            cols[iInf & x == -Inf] <- startcol
            cols[iInf & x == Inf] <- endcol
        }
        cols
    }
    # create plot
    dots <- list(...)
    if(is.null(xpd)) xpd <- is.null(xlim) && is.null(ylim)
    if(is.null(xlim)) xlim <- c(0.5, p+0.5)
    if(is.null(ylim)) ylim <- c(0.5, n+0.5)
    initializeWindow <- function(..., log, asp, yaxs) {
        plot.new()
        plot.window(..., yaxs="r")
    }
    initializeWindow(xlim=xlim, ylim=ylim, ...)  # dummy initialization
    yaxp <- par("yaxp")  # retrieve y-axis tickmarks
    localWindow <- function(..., log, asp, yaxs) {
        plot.window(..., yaxs=if(is.null(dots$yaxs)) "i" else dots$yaxs)
    }
    localWindow(xlim=xlim, ylim=ylim, ...)
    par(yaxp=yaxp)  # reset y-axis tickmarks to make sure they include 0
    createPlot <- function() {
		allNA <- is.na(x)
		if(imputed) {
			tmp_imp <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = "none")[["missh"]]
			allNA[,colnames(tmp)] <- tmp_imp
		}
		allNA <- all(allNA)
        if(allNA) {		# only missings/imputed missings
			if(!imputed) color <- col[1]
			else color <- col[2]
			cols <- rep(color, n*p)  
		}
        else if(is.null(sortby)) { # get colors
            if(!imputed) cols <- as.vector(apply(x, 2, getCol)) 
			else {
				cols <- vector()
				for (i in 1:p) {
					cols <- append(cols,getCol(x[, i, drop=FALSE]))
				}
			}
        } else {
            ord <- order(x[,sortby])  # get order
            if(!imputed) cols <- as.vector(apply(x[ord,, drop=FALSE], 2, getCol)) # get colors
			else {
				cols <- vector()
				for (i in 1:p) {
					cols <- append(cols,getCol(x[ord, i, drop=FALSE], ord = ord))
				}
			}
        }
        rect(rects$xl, rects$yb, rects$xr, rects$yt, 
            col=cols, border=NA, xpd=xpd)
    }
    createPlot()
    # axes
    x.axis <- TRUE
    if(is.logical(labels)) {
        if(!is.na(labels) && labels) labels <- NULL
        else x.axis <- FALSE
    }
    if(x.axis) {
        dots$side <- 1
        dots$at <- 1:p
        if(is.null(labels)) dots$labels <- colnames(x) 
        else dots$labels <- rep(labels, length.out=p)
        dots$lty <- 0
        if(is.null(dots$las)) dots$las <- 3
        if(dots$las %in% 2:3) {
            space.vert <- (par("mar")[1]+par("oma")[1]-1)*par("csi")
            ok <- prettyLabels(dots$labels, dots$at, space.vert, dots$cex.axis)
            if(any(ok)) {
                dots$at <- dots$at[ok]
                dots$labels <- dots$labels[ok]
            } else x.axis <- FALSE
        }
    }
    if(x.axis) {
        do.call(axis, dots)  # x-axis
    }
    if(axes) axis(2, xpd=NA, ...) # y-axis
    # plot annotation
    if(is.null(ylab)) ylab <- "Index"
    title(main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
    interactiveDevices <- c("X11","quartz","windows")
    dev <- names(dev.cur())
    if(interactive && any(!is.na(charmatch(interactiveDevices, dev)))) {
        cat("\nClick in a column to sort by the corresponding variable.\n")
        cat(paste("To regain use of the VIM GUI and the R console,",
                  "click outside the plot region.\n\n"))
        usr <- par("usr")
        pt <- locatorVIM()
        while(!is.null(pt) && 
                max(0.5, usr[1]) <= pt$x && pt$x < min(p+0.5, usr[2]) && 
                max(0.5, usr[3]) <= pt$y && pt$y <= min(n+0.5, usr[4])) {
            sortby <- round(pt$x)
            svar <- colnames(x)[sortby]  # new sort variable
            cat(gettextf("Matrix plot sorted by variable '%s'.\n", svar))
            createPlot()
            pt <- locatorVIM()
        }
    }
    invisible()
}

# compatibility wrapper
iimagMiss <- function (x, delimiter = NULL, sortby = NULL, col = c("red","orange"), main = NULL, 
        sub = NULL, xlab = NULL, ylab = NULL, 
        xlim = NULL, ylim = NULL, axes = TRUE, 
        xaxlabels = NULL, las = 3, interactive = TRUE, 
        ...) {
    if(is.null(xaxlabels)) xaxlabels <- axes
    matrixplot(x, delimiter=delimiter,sortby=sortby, col=col, xlim=xlim, ylim=ylim, 
        main=main, sub=sub, xlab=xlab, axes=axes, labels=xaxlabels, 
        interactive=TRUE, las=las, ...)
}
