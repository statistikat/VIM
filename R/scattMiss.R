# ---------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         Vienna University of Technology
# ---------------------------------------

scattMiss <- function(x, delimiter = NULL, side = 1, col = c("skyblue","red","orange","lightgrey"), 
        alpha = NULL, lty = c("dashed","dotted"), 
        lwd = par("lwd"), quantiles = c(0.5, 0.975), 
        inEllipse = FALSE, zeros = FALSE, 
        xlim = NULL, ylim = NULL, main = NULL, 
        sub = NULL, xlab = NULL, ylab = NULL, 
        interactive = TRUE, ...) {
    # error messages
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
    if(ncol(x) != 2) stop("'x' must be 2-dimensional")
    if(length(col) == 0) col <- c("skyblue","red","orange","lightgrey")
    else if(length(col) == 1) col <- c(rep.int(col, 3), "lightgrey")
    else if(length(col) == 2) col <- c(rep(col,1:2), "lightgrey")
    else if(length(col) == 3) col <- c(col,"lightgrey")
    else if(length(col) != 4) stop("'col' must be a vector of length 3 or 4")
    if(length(side) == 0) s <- 1
    else {
        s <- side[1]
        if(!(s %in% 1:2)) stop("'side' must be 1 or 2")
    }
    if(length(lty) == 0) lty <- c("dashed","dotted")
    else if(length(lty) == 1) lty <- rep.int(lty, 2)
    else if(length(lty) > 2) lty <- lty[1:2]
    if(length(lwd) == 0) lwd <- rep.int(par("lwd"), 2)
    else if(length(lwd) == 1) lwd <- rep.int(lwd, 2)
    else if(length(lwd) > 2) lwd <- lwd[1:2]
    if(!is.logical(zeros) || length(zeros) == 0) zeros <- FALSE
    zeros <- rep(sapply(zeros, isTRUE), length.out=2)
    # prepare data
    if(is.data.frame(x)) x <- data.matrix(x)
    else if(mode(x) != "numeric") mode(x) <- "numeric"
    iInf <- apply(x, 1, function(x) any(is.infinite(x)))
    if(any(iInf)) {
        x <- x[!iInf, , drop=FALSE]
        warning("'x' contains infinite values")
    }
    # default axis labels
    if(is.null(colnames(x))) {
        if(is.null(xlab)) xlab <- ""
        if(is.null(ylab)) ylab <- ""
    }
    # semitransparent colors
    if(!is.null(alpha)) col <- alphablend(col, alpha)
    # count missings
    n <- nrow(x)
	if(!imputed) {
		nNA <- apply(x, 2, countNA)
	} else {
		nNA <- countImp(x, delimiter, imp_var)
	} 
    plot.ellipse <- !is.null(quantiles) && all(n - nNA > 2)
    createPlot <- function() {
        if(is.null(xlim) && nNA[1] == n) xlim <- rep.int(0, 2)
        if(is.null(ylim) && nNA[2] == n) ylim <- rep.int(0, 2)
        sm <- s %% 2 + 1
        if(nNA[sm]) {  # missings in other variable
            if(plot.ellipse) {
                # define ellipse
                xMcd <- x
                if(zeros[1]) xMcd <- xMcd[xMcd[,1] != 0,]
                if(zeros[2]) xMcd <- xMcd[xMcd[,2] != 0,]
                cov <- covMcd(xMcd, alpha=0.75)
                cen <- cov$cen
                cov <- cov$cov
                cov.svd <- svd(cov, nv=0)
                r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
                t <- 2*pi*(0:100)/100  # parameter for ellipse
                qch <- qchisq(quantiles, 2)  # quantiles of qchisquare dist.
                getEll <- function(q, t, r, cen) {
                    e <- cbind(cos(t) * sqrt(q), sin(t) * sqrt(q))
                    t(r %*% t(e)) + rep(1, 101) %o% cen
                }
                tt <- sapply(qch, getEll, t, r, cen, simplify=FALSE)
                ttmax <- tt[[which.max(quantiles)]]
                # axis limits
                if(is.null(xlim)) {
                    xlim <- range(c(x[,1], ttmax[,1]), na.rm=TRUE)
                }
                if(is.null(ylim)) {
                    ylim <- range(c(x[,2], ttmax[,2]), na.rm=TRUE)
                }
            }
			if(!imputed) miss <- is.na(x)
			else {
				tmp <- isImp(x, pos = 1, delimiter = delimiter, imp_var = imp_var, selection = "none")
				miss <- cbind(tmp[["misspos"]],tmp[["missh"]])
			}
            xsmiss <- x[!miss[,s] & miss[,sm], s]
			localPlot <- function(..., type, plot.first, plot.last) {
                plot(..., 
                    panel.first={
                        if(plot.ellipse) # plot ellipses
                            for(t in tt) lines(t[,1], t[,2], col=col[4], 
                                    lty=lty[2], lwd=lwd[2])
                        # plot points instead of lines when imputed values exist 
						if(!imputed) {
							if(plot.ellipse && inEllipse) {
	                            xsell <- xsmiss[min(ttmax[,s]) <= xsmiss & 
	                                    xsmiss <= max(ttmax[,s])]
	                            if(length(xsell)) {
	                                rho <- cov[1,2]/sqrt(cov[1,1]*cov[2,2])
	                                phalf <- rho*sqrt(cov[sm,sm])*
	                                    (xsell-cen[s])/sqrt(cov[s,s])
	                                q <- cov[sm,sm]*((xsell-cen[s])^2/cov[s,s]-
	                                        (1-rho^2)*qch[length(qch)])
	                                z <- rbind(phalf + sqrt(phalf^2-q), 
	                                    phalf - sqrt(phalf^2-q))
	                                xsmell <- z + cen[sm]
	                                xsell <- as.vector(rbind(xsell, xsell, 
	                                        rep(NA, length(xsell))))
	                                xsmell <- as.vector(rbind(xsmell, 
	                                        rep(NA, ncol(xsmell))))
	                                if(s == 1) lines(xsell, xsmell, col=col[2], 
	                                        lty=lty[1], lwd=lwd[1])
	                                else lines(xsmell, xsell, col=col[2], 
	                                        lty=lty[1], lwd=lwd[1])
	                            }
	                        } else {
	                            if(s == 1) {
	                                abline(v=xsmiss, col=col[2], 
	                                    lty=lty[1], lwd=lwd[1])
	                            } else {
	                                abline(h=xsmiss, col=col[2], 
	                                    lty=lty[1], lwd=lwd[1])
	                            }
	                        }
						}
                    })
            }
        } else {
            localPlot <- function(..., type, plot.first, plot.last) plot(...)
        }
        localPlot(x, col=col[1], xlim=xlim, ylim=ylim, 
            main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
		# plot points instead of lines when imputed values exist	
		if(!imputed) miss <- NULL
		else points(x[miss[,sm],], col=col[3])
		rugNA(x[,1], x[, 2], miss = miss, side=s, col=ifelse(!imputed,col[2],col[3]))
    }
    createPlot()
    dev <- names(dev.cur())
    interactiveDevices <- c("X11","quartz","windows")
    if(interactive && any(!is.na(charmatch(interactiveDevices, dev)))) {
        cat(paste("\nClick in bottom or left margin to",
                "change the 'side' argument accordingly.\n"))
        cat(paste("To regain use of the VIM GUI and the R console,",
                "click anywhere else in the graphics window.\n\n"))
        usr <- par("usr")
        pt <- locatorVIM()
        while(!is.null(pt) && 
            ((usr[1] <= pt$x && pt$x <= usr[2] && pt$y < usr[3]) || 
                (pt$x < usr[1] && usr[3] <= pt$y && pt$y <= usr[4]))) {
            s <- if(usr[1] <= pt$x && pt$x <= usr[2] && pt$y < usr[3]) 1 else 2
            createPlot()
            usr <- par("usr")
            pt <- locatorVIM()
        }
    }
    invisible()
}
