# ------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ------------------------------------------

histMiss <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                     breaks = "Sturges", right = TRUE, 
                     col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                     border = NULL, main = NULL, sub = NULL, 
                     xlab = NULL, ylab = NULL, axes = TRUE, 
                     only.miss = TRUE, miss.labels = axes, 
                     interactive = TRUE, ...) {
  UseMethod("histMiss", x)
}

histMiss.data.frame <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                                breaks = "Sturges", right = TRUE, 
                                col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                border = NULL, main = NULL, sub = NULL, 
                                xlab = NULL, ylab = NULL, axes = TRUE, 
                                only.miss = TRUE, miss.labels = axes, 
                                interactive = TRUE, ...) {
  histMiss_work(x, delimiter, pos, selection, breaks, right, col, border, main, sub,
            xlab, ylab, axes, only.miss, miss.labels, interactive, ...)
}

histMiss.survey.design <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                                   breaks = "Sturges", right = TRUE, 
                                   col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                                   border = NULL, main = NULL, sub = NULL, 
                                   xlab = NULL, ylab = NULL, axes = TRUE, 
                                   only.miss = TRUE, miss.labels = axes, 
                                   interactive = TRUE, ...) {
  histMiss_work(x$variables, delimiter, pos, selection, breaks, right, col, border, main, sub,
                xlab, ylab, axes, only.miss, miss.labels, interactive, ...)
}

histMiss.default <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
                             breaks = "Sturges", right = TRUE, 
                             col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
                             border = NULL, main = NULL, sub = NULL, 
                             xlab = NULL, ylab = NULL, axes = TRUE, 
                             only.miss = TRUE, miss.labels = axes, 
                             interactive = TRUE, ...) {
  histMiss_work(as.data.frame(x), delimiter, pos, selection, breaks, right, col, border, main, sub,
                xlab, ylab, axes, only.miss, miss.labels, interactive, ...)
}

histMiss_work <- function(x, delimiter = NULL, pos = 1, selection = c("any","all"), 
		breaks = "Sturges", right = TRUE, 
		col = c("skyblue","red","skyblue4","red4","orange","orange4"), 
		border = NULL, main = NULL, sub = NULL, 
		xlab = NULL, ylab = NULL, axes = TRUE, 
		only.miss = TRUE, miss.labels = axes, 
		interactive = TRUE, ...) {
	

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
			rect(xleft, ybottom, xright, ytop, 
					col=color, border=border, xpd=TRUE)
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
