# ---------------------------------------
# Author: Andreas Alfons ,Bernd Prantner
#         and Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------

barMiss <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                    col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                    border = NULL, main = NULL, sub = NULL, 
                    xlab = NULL, ylab = NULL, axes = TRUE, 
                    labels = axes, only.miss = TRUE, 
                    miss.labels = axes, interactive = TRUE, ...) {
  UseMethod("barMiss", x)
}

barMiss.data.frame <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                               col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                               border = NULL, main = NULL, sub = NULL, 
                               xlab = NULL, ylab = NULL, axes = TRUE, 
                               labels = axes, only.miss = TRUE, 
                               miss.labels = axes, interactive = TRUE, ...) {
  barMiss_work(x, delimiter, pos, selection, col, border, main, sub, xlab, ylab, axes, labels, only.miss,
               miss.labels, interactive, ...)
}

barMiss.survey.design <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                                  col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                  border = NULL, main = NULL, sub = NULL, 
                                  xlab = NULL, ylab = NULL, axes = TRUE, 
                                  labels = axes, only.miss = TRUE, 
                                  miss.labels = axes, interactive = TRUE, ...) {
  barMiss_work(x$variables, delimiter, pos, selection, col, border, main, sub, xlab, ylab, axes, labels, only.miss,
               miss.labels, interactive, ...)
}

barMiss.default <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                            col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                            border = NULL, main = NULL, sub = NULL, 
                            xlab = NULL, ylab = NULL, axes = TRUE, 
                            labels = axes, only.miss = TRUE, 
                            miss.labels = axes, interactive = TRUE, ...) {
  barMiss_work(as.data.frame(x), delimiter, pos, selection, col, border, main, sub, xlab, ylab, axes, labels, only.miss,
               miss.labels, interactive, ...)
}

barMiss_work <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
        col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
        border = NULL, main = NULL, sub = NULL, 
        xlab = NULL, ylab = NULL, axes = TRUE, 
        labels = axes, only.miss = TRUE, 
        miss.labels = axes, interactive = TRUE, ...) {
    
	imputed <- FALSE # indicates if there are Variables with missing-index
    # initializations and error messages
    if(is.null(dim(x))) {  # vector
		# call histMiss if the plot variable is continuous
		if(is.continuous(x)) {
			histMiss(x, delimiter=delimiter, pos=pos, selection=selection, col=col, 
					border=border, main=main, sub=sub, xlab=xlab, ylab=ylab,
					axes=axes, only.miss=only.miss, 
					miss.labels=miss.labels, interactive=interactive, ...)
			return(invisible(1))
		}
        n <- length(x)
        p <- 1
        if(n == 0) stop("'x' must have positive length")
    } else {  # matrix or data.frame
        if(!(inherits(x, c("data.frame","matrix")))) { 
            stop("'x' must be a data.frame or matrix")
        }
		# call histMiss if the plot variable is continuous
		if(is.continuous(x[, pos])) {
			histMiss(x, delimiter=delimiter, pos=pos, selection=selection, col=col, 
					border=border, main=main, sub=sub, xlab=xlab, ylab=ylab,
					axes=axes, only.miss=only.miss, 
					miss.labels=miss.labels, interactive=interactive, ...)
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
    has.axisnames <- "axisnames" %in% nmdots
    if(missing(labels)) {
        if(has.axisnames) {
            if(dots$axisnames) {
                if("names.arg" %in% nmdots) labels <- dots$names.arg
                else labels <- TRUE
            } else labels <- FALSE
        } else if("names.arg" %in% nmdots) labels <- dots$names.arg
    }
    if(missing(miss.labels)) {
        if(has.axisnames) {
            if(dots$axisnames) {
                if("names.miss" %in% nmdots) miss.labels <- dots$names.miss
                else miss.labels <- TRUE
            } else miss.labels <- FALSE
        } else if("names.miss" %in% nmdots) miss.labels <- dots$names.miss
    }
    
    # workhorse to create plot
    createPlot <- function(main=NULL, sub=NULL, 
            xlab=NULL, ylab=NULL, labels=axes) {
        # prepare data
        if(is.null(dim(x))) xpos <- as.factor(x)
        else if(p == 1) {
            xpos <- as.factor(x[,1])
            if(is.null(xlab)) xlab <- colnames(x)  # default x-axis label
        } else {
            xpos <- as.factor(x[, pos])  # plot variable
            xh <- x[, -pos, drop=FALSE]  # highlight variables
            if(is.null(xlab)) xlab <- colnames(x)[pos]  # default x-axis label
        }
		
		if(p == 2 && is.null(ylab)) {  # default y-axis label
			if(!imputed) ylab <- paste("missing/observed in", colnames(x)[-pos])
			else ylab <- paste("imputed/observed in", colnames(x)[-pos])
		}
		
        # plot annotation
        x.axis <- TRUE
        if(is.logical(labels)) {
            if(!is.na(labels) && labels) labels <- NULL
            else x.axis <- FALSE
        }
        miss.axis <- TRUE
        if(is.logical(miss.labels)) {
            if(!is.na(miss.labels) && miss.labels) miss.labels <- NULL
            else miss.axis <- FALSE
        }
        
		impp <- FALSE # indicates if the current variable has imputed missings
		# get missings/imputed missings and plot limits
		if(!imputed) { # barMiss
			misspos <- isNA(xpos)
		} else { # barImp
			tmp <- isImp(x, pos = pos, delimiter = delimiter, imp_var = imp_var, selection = selection)
			misspos <- tmp[["misspos"]]
			impp <- tmp[["impp"]]
			missh <- tmp[["missh"]]
		}
		missposf <- factor(ifelse(misspos, 1, 0), levels=0:1)
		
   
		if(p == 1) ct <- table(missposf)[2]  # number of missings
		else {
			if(!imputed) missh <- isNA(xh, selection) # barMiss
			
			misshf <- factor(ifelse(missh, 1, 0), levels=1:0)
			ct <- table(misshf, missposf)  # contingency table for missings
			ct[2,] <- ct[1,] + ct[2,]  # y-coordinates for rectangles
			if(only.miss) ct <- ct[,2]
		}
	
		allNA <- all(misspos)
    	if(allNA) {
		n <- 5
        counts <- 0
		} else {
            n <- length(levels(xpos))
			counts <- summary(xpos[!misspos])
        }
        # extend x-axis limits
        br <- c(0.2, n*1.2)
        h <- br[2] - br[1]
        if(only.miss) {
            xlim <- c(br[1], br[2]+1+0.08*h)
            ylim <- c(0, max(summary(xpos)))
        } else {
            xlim <- c(br[1], br[2]+0.155*h)
            ylim <- c(0, max(counts))
        }
        if(allNA) {
            b <- NULL
            labels <- character()
            plot(xlim, ylim, type="n", ann=FALSE, axes=FALSE, yaxs="i")
            if(only.miss && axes) localAxis(side=2, ...)  # y-axis
        } else {
#            if(p > 1 && any(missh)) {
#                # missings in highlight variables: stacked barplot
#                counts <- table(missh, xpos)
#                b <- barplot(counts, col=col[2:1], border=border, 
#                    main="", sub="", xlab="", ylab="", xlim=xlim, 
#                    ylim=ylim, axes=FALSE, axisnames=FALSE)
#            } else {  # simple barplot
#                b <- barplot(counts, col=col[1], border=border, 
#                    main="", sub="", xlab="", ylab="", xlim=xlim, 
#                    ylim=ylim, axes=FALSE, axisnames=FALSE)
#            }
            b <- barplot(counts, col=col[1], border=border, 
                main="", sub="", xlab="", ylab="", xlim=xlim, 
                ylim=ylim, axes=FALSE, axisnames=FALSE)
            if(p > 1 && any(missh)) {  # add barplot for missings
                if(imputed) color <- col[5]
				else color <- col[2]
				countsmiss <- table(xpos[missh], useNA="no")
                b <- barplot(countsmiss, col=color, border=border, 
                    add=TRUE, axes=FALSE, axisnames=FALSE)
            }
			else if(p == 1 && impp == TRUE && any(misspos)) {
				countsmiss <- table(xpos[missh], useNA="no")
                b <- barplot(countsmiss, col=col[5], border=border, 
                    add=TRUE, axes=FALSE, axisnames=FALSE)
				
			}
            if(x.axis) {
                if(is.null(labels)) labels <- levels(xpos)
                else labels <- rep(labels, length.out=length(levels(xpos)))
            }
            if(axes) localAxis(side=2, ...)  # y-axis
        }
        localTitle(main, sub, xlab, ylab, ...)  # plot annotation
        abline(v=br[2]+0.04*h, col="lightgrey")
        
        # additional information about missings
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
            if(miss.axis) {
                miss.at <- xleft+(xright-xleft)/2
                if(is.null(miss.labels)) {
					if(!imputed) miss.labels <- "missing"
					else miss.labels <- "imputed"
				}
                else miss.labels <- rep(miss.labels, length.out=1)
				
            }
        } else {  # stacked barplot for observed/missing in first variable
            usr <- par("usr")
            par(usr=c(usr[1:2], 0, max(ct[2,])))  # modify user coordinates
            on.exit(par(usr=usr))  # reset user coordinates on exit
            zero <- br[2]+0.08*h
            xleft <- zero + c(0,0,1.5,1.5)*0.03*h
            ybottom <- c(0,ct[1,1],0,ct[1,2])
            xright <- zero + c(1,1,2.5,2.5)*0.03*h
            ytop <- ct
			if(!imputed) color <- col[c(2,1,4,3)]
			else color <- col[c(5,1,6,3)]
            rect(xleft, ybottom, xright, ytop, 
                col=color, border=border, xpd=TRUE)
            if(miss.axis) {
                miss.at <- zero + c(0.5,2)*0.03*h
                if(is.null(miss.labels)) {
					if(!imputed) miss.labels <- c("observed","missing")
					else miss.labels <- c("observed","imputed")
				}
                else miss.labels <- rep(miss.labels, length.out=2)
            }
            if(axes) localAxis(side=4, ...)
        }
        
        # x-axis
        if(x.axis || miss.axis) {
            x.axes <- TRUE
            dots$side <- 1
            dots$at <- c(if(x.axis) b, if(miss.axis) miss.at)
            dots$labels <- c(if(x.axis) labels, if(miss.axis) miss.labels)
            if(is.null(dots$line)) dots$line <- par("mgp")[3]
            dots$lty <- 0
            if(is.null(dots$las)) dots$las <- 3
            if(dots$las %in% 2:3) {
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
        }
        
        return(b)
    }
    b <- createPlot(main, sub, xlab, ylab, labels)

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
            #b <- createPlot()
            b <- 
                if(is.continuous(x[, pos])) {
                    histMiss(if(imputed) cbind(x,imp_var) else x, delimiter = delimiter, pos=pos, selection=selection, col=col, 
                        border=border, axes=axes, only.miss=only.miss, 
                        miss.labels=miss.labels, interactive=FALSE, ...)
				} else createPlot(labels=if(is.logical(labels)) labels else axes)
            usr <- par("usr")
            pt <- locatorVIM()
        }
    }
    
    invisible(b)
}