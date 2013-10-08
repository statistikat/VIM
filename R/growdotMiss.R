# ---------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         and Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------



#' Growing dot map with information about missing/imputed values
#' 
#' Map with dots whose sizes correspond to the values in a certain variable.
#' Observations with missing/imputed values in additional variables are
#' highlighted.
#' 
#' The smallest dots correspond to the 10\% quantile and the largest dots to
#' the 99\% quantile.  In between, the dots grow exponentially, with \code{exp}
#' defining the shape of the exponential function.  Missings/imputed missings
#' in the variable of interest will be drawn as rectangles.
#' 
#' If \code{interactive=TRUE}, detailed information for an observation can be
#' printed on the console by clicking on the corresponding point.  Clicking in
#' a region that does not contain any points quits the interactive session.
#' 
#' @aliases growdotMiss bubbleMiss
#' @param x a vector, matrix or \code{data.frame}.
#' @param coords a matrix or \code{data.frame} with two columns giving the
#' spatial coordinates of the observations.
#' @param map a background map to be passed to \code{\link{bgmap}}.
#' @param pos a numeric value giving the index of the variable determining the
#' dot sizes.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, \code{x} needs to have
#' \code{\link{colnames}}). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see \code{col}).
#' @param selection the selection method for highlighting missing/imputed
#' values in multiple additional variables.  Possible values are \code{"any"}
#' (highlighting of missing/imputed values in \emph{any} of the additional
#' variables) and \code{"all"} (highlighting of missing/imputed values in
#' \emph{all} of the additional variables).
#' @param log a logical indicating whether the variable given by \code{pos}
#' should be log-transformed.
#' @param col a vector of length six giving the colors to be used in the plot.
#' If only one color is supplied, it is used for the borders of non-highlighted
#' dots and the surface area of highlighted dots.  Else if two colors are
#' supplied, they are recycled.
#' @param border a vector of length four giving the colors to be used for the
#' borders of the growing dots.  Use \code{NA} to omit borders.
#' @param alpha a numeric value between 0 and 1 giving the level of
#' transparency of the colors, or \code{NULL}.  This can be used to prevent
#' overplotting.
#' @param scale scaling factor of the map.
#' @param size a vector of length two giving the sizes for the smallest and
#' largest dots.
#' @param exp a vector of length three giving the factors that define the shape
#' of the exponential function (see \sQuote{Details}).
#' @param col.map the color to be used for the background map.
#' @param legend a logical indicating whether a legend should be plotted.
#' @param legtitle the title for the legend.
#' @param cex.legtitle the character expansion factor to be used for the title
#' of the legend.
#' @param cex.legtext the character expansion factor to be used in the legend.
#' @param ncircles the number of circles displayed in the legend.
#' @param ndigits the number of digits displayed in the legend.  Note that \
#' this is just a suggestion (see \code{\link{format}}).
#' @param interactive a logical indicating whether information about certain
#' observations can be displayed interactively (see \sQuote{Details}).
#' @param \dots for \code{growdotMiss}, further arguments and graphical
#' parameters to be passed to \code{\link{bgmap}}.  For \code{bubbleMiss}, the
#' arguments to be passed to \code{growdotMiss}.
#' @note The function was renamed to \code{growdotMiss} in version 1.3.
#' \code{bubbleMiss} is a (deprecated) wrapper for \code{growdotMiss} for back
#' compatibility with older versions. However, due to extended functionality,
#' some of the argument positions have changed.
#' 
#' The code is based on \code{\link[StatDA]{bubbleFIN}} from package
#' \code{StatDA}.
#' @author Andreas Alfons, Matthias Templ, Peter Filzmoser, Bernd Prantner
#' @seealso \code{\link{bgmap}}, \code{\link{mapMiss}},
#' \code{\link{colormapMiss}}
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  \emph{Journal of Advances in Data Analysis
#' and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(chorizonDL, package = "VIM")
#' data(kola.background, package = "VIM")
#' coo <- chorizonDL[, c("XCOO", "YCOO")]
#' ## for missing values
#' x <- chorizonDL[, c("Ca","As", "Bi")]
#' growdotMiss(x, coo, kola.background, border = "white")
#' 
#' ## for imputed values
#' x_imp <- kNN(chorizonDL[,c("Ca","As","Bi" )])
#' growdotMiss(x_imp, coo, kola.background, delimiter = "_imp", border = "white")
#' 
#' @export growdotMiss
#' @S3method growdotMiss data.frame
#' @S3method growdotMiss survey.design
#' @S3method growdotMiss default
growdotMiss <- function(x, coords, map, pos=1, delimiter = NULL, selection = c("any","all"), 
                        log = FALSE, col = c("skyblue", "red", "skyblue4", "red4", "orange", "orange4"), 
                        border = par("bg"), alpha = NULL, scale = NULL, 
                        size = NULL, exp = c(0, 0.95, 0.05), 
                        col.map = grey(0.5), legend = TRUE, 
                        legtitle = "Legend", cex.legtitle = par("cex"), 
                        cex.legtext = par("cex"), ncircles = 6, ndigits = 1, 
                        interactive = TRUE, ...)  {
  UseMethod("growdotMiss", x)
}

growdotMiss.data.frame <- function(x, coords, map, pos=1, delimiter = NULL, selection = c("any","all"), 
                                   log = FALSE, col = c("skyblue", "red", "skyblue4", "red4", "orange", "orange4"), 
                                   border = par("bg"), alpha = NULL, scale = NULL, 
                                   size = NULL, exp = c(0, 0.95, 0.05), 
                                   col.map = grey(0.5), legend = TRUE, 
                                   legtitle = "Legend", cex.legtitle = par("cex"), 
                                   cex.legtext = par("cex"), ncircles = 6, ndigits = 1, 
                                   interactive = TRUE, ...)  {
  growdotMiss_work(x, coords, map, pos, delimiter, selection, log, col, border, alpha, scale, size,
                   exp, col.map, legend, legtitle, cex.legtitle, cex.legtext, ncircles,
                   ndigits, interactive,...)
}

growdotMiss.survey.design <- function(x, coords, map, pos=1, delimiter = NULL, selection = c("any","all"), 
                                      log = FALSE, col = c("skyblue", "red", "skyblue4", "red4", "orange", "orange4"), 
                                      border = par("bg"), alpha = NULL, scale = NULL, 
                                      size = NULL, exp = c(0, 0.95, 0.05), 
                                      col.map = grey(0.5), legend = TRUE, 
                                      legtitle = "Legend", cex.legtitle = par("cex"), 
                                      cex.legtext = par("cex"), ncircles = 6, ndigits = 1, 
                                      interactive = TRUE, ...)  {
  growdotMiss_work(x$variables, coords, map, pos, delimiter, selection, log, col, border, alpha, scale, size,
                   exp, col.map, legend, legtitle, cex.legtitle, cex.legtext, ncircles,
                   ndigits, interactive,...)
}

growdotMiss.default <- function(x, coords, map, pos=1, delimiter = NULL, selection = c("any","all"), 
                                log = FALSE, col = c("skyblue", "red", "skyblue4", "red4", "orange", "orange4"), 
                                border = par("bg"), alpha = NULL, scale = NULL, 
                                size = NULL, exp = c(0, 0.95, 0.05), 
                                col.map = grey(0.5), legend = TRUE, 
                                legtitle = "Legend", cex.legtitle = par("cex"), 
                                cex.legtext = par("cex"), ncircles = 6, ndigits = 1, 
                                interactive = TRUE, ...)  {
  growdotMiss_work(as.data.frame(x), coords, map, pos, delimiter, selection, log, col, border, alpha, scale, size,
                   exp, col.map, legend, legtitle, cex.legtitle, cex.legtext, ncircles,
                   ndigits, interactive,...)
}

# code is based on Peter Filzmoser's function 'bubbleFIN' in package 'StatDA'
# FIXME: infinite values
growdotMiss_work <- function(x, coords, map, pos=1, delimiter = NULL, selection = c("any","all"), 
        log = FALSE, col = c("skyblue", "red", "skyblue4", "red4", "orange", "orange4"), 
        border = par("bg"), alpha = NULL, scale = NULL, 
        size = NULL, exp = c(0, 0.95, 0.05), 
        col.map = grey(0.5), legend = TRUE, 
        legtitle = "Legend", cex.legtitle = par("cex"), 
        cex.legtext = par("cex"), ncircles = 6, ndigits = 1, 
        interactive = TRUE, ...) {
    # ncircles ... number of circles for the legend
    # ndigits ... number of digits for the legend
    # error messages
	imputed <- FALSE # indicates if there are Variables with missing-index
    if(is.null(dim(x))) {
        nx <- length(x)
        px <- 1
    } else {
        if(!inherits(x, c("data.frame","matrix"))) {
            stop("'x must be a data.frame or matrix")
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
        nx <- nrow(x)
        px <- ncol(x)
        if(px == 0) stop("'x' has no columns")
    }
    if(!(inherits(coords, c("data.frame","matrix")))) {
        stop("'coords' must be a data.frame or matrix")
    }
    if(ncol(coords) != 2) stop("'coords' must be 2-dimensional")
    if(nx != nrow(coords)) { 
    	stop("'x' and 'coords' must have equal number of elements/rows")
    }
#    if(length(col) == 0) col <- c("skyblue","red","red4")
#    else if(length(col) == 1) {
#        border <- c(col, "transparent", "transparent")
#        col <- c("transparent", col, col)
#    } else if(length(col) == 2) col <- rep(col, 1:2)
#    else if(length(col) > 3) col <- col[1:3]
#    if(length(border) == 0) border <- par("bg")
#    else if(length(border) == 1) border <- rep.int(border, 3)
#    else if(length(border) == 2) border <- rep(border, 1:2)
#    else if(length(border) > 3) border <- border[1:3]
    if(length(col) == 0) col <- c("skyblue", "red", "skyblue4", "red4", "orange", "orange4")
    else if(length(col) == 1) {
        border <- rep.int(c(col, "transparent"), 2)
		col <- c(rep.int(c("transparent", col), 2),rep.int(col,2))
    } else if(length(col) == 3 || length(col) == 5) col <- rep.int(col[1:2], 3)
	else if(length(col) != 6) col <- rep(col, length.out=6)
    if(length(border) == 0) border <- par("bg")
    else if(length(border) == 1) lty <- rep.int(border, 4)
    else if(length(border) == 3) border <- rep.int(border[1:2], 2)
    else if(length(border) != 4) border <- rep(border, length.out=4)
    coords <- as.data.frame(coords)
    if(px > 1) {
        if(!is.numeric(pos) || length(pos) != 1 || (px < pos)) {
            stop("'pos' must be an integer specifying one column of 'x'")
        }
        selection <- match.arg(selection)
    }
    if(!is.null(alpha)) {
        col <- alphablend(col, alpha)  # semitransparent colors
        border <- alphablend(border, alpha)  # semitransparent borders
    }
    # initialize plot
    bgmap(map, col=col.map, ...)
    if(px == 1) {
        if(!imputed) missPos <- is.na(x)  # indicates missings in plot variable
		else missPos <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
        missOther <- rep.int(FALSE, nx)
        z <- as.numeric(x[!missPos])  # observed values in plot variable
        miss <- rep.int(FALSE, length(z))
    } else {
        if(!imputed) {
			missPos <- is.na(x[, pos])  # indicates missings in plot variable
        	missOther <- isNA(x[, -pos, drop=FALSE], selection)
        	z <- as.numeric(x[!missPos, pos])  # observed values in plot variable
        	miss <- isNA(x[!missPos, -pos, drop=FALSE], selection)
		} else {
			tmp <- isImp(x, pos = pos, delimiter = delimiter, imp_var = imp_var, selection = selection)
			missPos <- tmp[["misspos"]]
			missOther <- tmp[["missh"]]
			z <- as.numeric(x[!missPos, pos])  # observed values in plot variable
			miss <- isImp(x[!missPos, -pos ,drop=FALSE], pos = NULL, delimiter = delimiter, imp_var = imp_var[!missPos,], selection = selection)[["missh"]]
		}
    }
    if(log) {
        if(any(z < 0)) stop("cannot use logarithm with negative values")
        z <- log10(z)
    }
    if(is.null(size)) {  # default size depends on area and sample density
        # retrieve bounding box for background map
        usr <- par("usr")
        xr <- usr[1:2]
        if(par("xaxs") == "r") xr <- xr + c(1,-1)*diff(xr)*0.04/1.08
        yr <- usr[3:4]
        if(par("yaxs") == "r") yr <- yr + c(1,-1)*diff(yr)*0.04/1.08
        # area of bounding box
        Abox <- diff(xr)*diff(yr)
        maxsize <- sqrt(Abox/nx)
        size <- c(maxsize/10, maxsize)
        scale <- NULL
    }
    if(length(z)) {
        mnz <- min(z)
        zz <- if(mnz < 0) z + abs(mnz) else z 
        q1 <- quantile(zz, 0.1)
        q2 <- quantile(zz, 0.99)
        c <- q1 / (q2/q1)^(exp[1]/exp[2])
        C <- q2 / (q1/q2)^(exp[3]/exp[2])
        xi <- pmax(pmin(zz,C), c)
        di <- size[1] * (size[2]/size[1])^(log10(xi/c)/log10(C/c))
        if(!is.null(scale)) di <- scale * di
        coordsobs <- coords[!(missPos | missOther),]
        diobs <- di[!miss]
        ordobs <- order(z[!miss], decreasing=TRUE)
        circles(coordsobs[ordobs, 1], coordsobs[ordobs, 2], 
            diobs[ordobs]/2, col=col[1], border=border[1])
        # observations with missings in other variables
        coordsmiss <- coords[!missPos & missOther,]
        dimiss <- di[miss]
        ordmiss <- order(z[miss], decreasing=TRUE)
		if(!imputed) color <- col[2]
		else color <- col[5]
        circles(coordsmiss[ordmiss, 1], coordsmiss[ordmiss, 2], 
            dimiss[ordmiss]/2, col=color, border=border[2])
    }
    # missings in plot variable
    if(any(missPos)) {
#        sqx <- (C+c)/2
#        s <- size[1] * (size[2]/size[1])^(log10(sqx/c)/log10(C/c)) / 2
        s <- mean(size) * 0.35
        if(!is.null(scale)) s <- scale * s
        cp <- coords[missPos & !missOther, , drop=FALSE]
        co <- coords[missPos & missOther, , drop=FALSE]
        rect(cp[,1]-s, cp[,2]-s, cp[,1]+s, cp[,2]+s, 
            col=col[3], border=border[3])
		if(!imputed) color <- col[4]
		else color <- col[6]
        rect(co[,1]-s, co[,2]-s, co[,1]+s, co[,2]+s, 
            col=color, border=border[4])
    }
    # add legend (top right)
    if(length(z) && legend) {
        probs <- seq(1, 0, length.out=ncircles)
        diq <- quantile(di, probs=probs)
        #zq <- quantile(x[!missPos, pos], probs=probs)
        if(px == 1) zq <- quantile(x[!missPos], probs=probs) 
        else zq <- quantile(x[!missPos, pos], probs=probs)
        lsheight <- strheight(legtitle, cex=cex.legtitle)
        legtext <- format(zq, digits=ndigits)
        maxsheight <- max(strheight(legtext, cex=cex.legtext))
        maxswidth <- max(strwidth(legtext, cex=cex.legtext))
        xmax <- max(coords[,1])
        ymax <- max(coords[,2])
        xt <- xmax - maxswidth
        xc <- xt - max(diq)
        yc <- ymax - lsheight*2
        yc <- c(yc, yc - maxsheight*1.5*(1:(length(diq)-1)))
        circles(xc, yc, diq/2, col=col[1], border=border[1])
        text(xt, yc, legtext, adj=0, cex=cex.legtext)
        lswidth <- strwidth(legtitle, cex=cex.legtitle)
        tswidth <- max(diq)*2 + maxswidth
        if(lswidth > tswidth) 
            text(xmax, ymax, legtitle, adj=1, cex=cex.legtitle)
        else text(xmax-tswidth, ymax, legtitle, adj=0, cex=cex.legtitle)
    }
    if(interactive) {
        cat("\nClick on a point to get more information.\n")
        cat(paste("To regain use of the VIM GUI and the R console,",
                  "click in a region that does not contain any points.\n\n"))
        identifyPt <- function(p, x) {  # function to identify closest point
            if(is.null(p) || nrow(x) == 0) return(NA)
            d <- sqrt(colSums((t(x)-p)^2))
            m <- min(d, na.rm=TRUE)
            r <- apply(x,2,range, na.rm=TRUE)
            r <- max(r[2,]-r[1,])
            if(m/r < 0.05) which(d == min(d, na.rm=TRUE))
            else NA
        }
        pt <- locatorVIM()
        ind <- identifyPt(unlist(pt), coords)  # get closest point
        while(!is.na(ind)) {
#            print(x[ind,])
            if(px == 1) print(x[ind])  # print values for
            else print(x[ind,])       # the identified point
            pt <- locatorVIM()
            ind <- identifyPt(unlist(pt), coords)
        }
    }
    invisible()
}

# compatibility wrapper
bubbleMiss <- function(...) {
    growdotMiss(...)
}

# modified version of Peter Filzmoser's function in package 'StatDA'
circles <- function(x, y, radius, col=NA, border=par("fg")) {
    #draw circles
    nmax <- max(length(x), length(y));
    if (length(x) < nmax) x <- rep(x, length=nmax);
    if (length(y) < nmax) y <- rep(y, length=nmax);
    if (length(col) < nmax) col <- rep(col, length=nmax);
    if (length(border) < nmax) border <- rep(border, length=nmax);
    if (length(radius) < nmax) radius <- rep(radius, length=nmax);
    theta <- 2* pi * seq(0, 355, by=5) / 360;
    ct <- cos(theta);
    st <- sin(theta);
    #for(i in 1:nmax)
    #    polygon(x[i] + ct * radius[i], y[i] + st * radius[i], 
    #        col=col[i], border=border[i]);
    xmat <- mapply(function(x,r,c) x+c*r, x, radius, MoreArgs=list(ct))
    ymat <- mapply(function(y,r,s) y+s*r, y, radius, MoreArgs=list(st))
    xvec <- as.vector(rbind(xmat, rep(NA, length(x))))
    yvec <- as.vector(rbind(ymat, rep(NA, length(y))))
    polygon(xvec, yvec, col=col, border=border)
}
