# ----------------------------------------------------------
# Authors: Andreas Alfons, Bernd Prantner and Matthias Templ
#          Vienna University of Technology
# ----------------------------------------------------------

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
