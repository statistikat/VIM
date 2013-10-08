# ----------------------------------------------------------
# Authors: Matthias Templ, Andreas Alfons, Bernd Prantner
#          and Daniel Schopfhauser
#          Vienna University of Technology
# ----------------------------------------------------------



#' Map with information about missing/imputed values
#' 
#' Map of observed and missing/imputed values.
#' 
#' If \code{interactive=TRUE}, detailed information for an observation can be
#' printed on the console by clicking on the corresponding point.  Clicking in
#' a region that does not contain any points quits the interactive session.
#' 
#' @param x a vector, matrix or \code{data.frame}.
#' @param coords a \code{data.frame} or matrix with two columns giving the
#' spatial coordinates of the observations.
#' @param map a background map to be passed to \code{\link{bgmap}}.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, \code{x} needs to have
#' \code{\link{colnames}}). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see \code{col}).
#' @param selection the selection method for displaying missing/imputed values
#' in the map.  Possible values are \code{"any"} (display missing/imputed
#' values in \emph{any} variable) and \code{"all"} (display missing/imputed
#' values in \emph{all} variables).
#' @param col a vector of length three giving the colors to be used for
#' observed, missing and imputed values.  If a single color is supplied, it is
#' used for all values.
#' @param alpha a numeric value between 0 and 1 giving the level of
#' transparency of the colors, or \code{NULL}.  This can be used to prevent
#' overplotting.
#' @param pch a vector of length two giving the plot characters to be used for
#' observed and missing/imputed values.  If a single plot character is
#' supplied, it will be used for both.
#' @param col.map the color to be used for the background map.
#' @param legend a logical indicating whether a legend should be plotted.
#' @param interactive a logical indicating whether information about selected
#' observations can be displayed interactively (see \sQuote{Details}).
#' @param \dots further graphical parameters to be passed to
#' \code{\link{bgmap}} and \code{\link[graphics]{points}}.
#' @author Matthias Templ, Andreas Alfons, modifications by Bernd Prantner
#' @seealso \code{\link{bgmap}}, \code{\link{bubbleMiss}},
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
#' x <- chorizonDL[, c("As", "Bi")]
#' mapMiss(x, coo, kola.background)
#' 
#' ## for imputed values
#' x_imp <- kNN(chorizonDL[, c("As", "Bi")])
#' mapMiss(x_imp, coo, kola.background, delimiter = "_imp")
#' 
#' @export mapMiss
#' @S3method mapMiss data.frame
#' @S3method mapMiss survey.design
#' @S3method mapMiss default
mapMiss <- function(x, coords, map, delimiter = NULL, selection = c("any","all"), 
                    col = c("skyblue","red","orange"), alpha = NULL, 
                    pch = c(19,15), col.map = grey(0.5), 
                    legend = TRUE, interactive = TRUE, ...) {
  UseMethod("mapMiss", x)
}

mapMiss.data.frame <- function(x, coords, map, delimiter = NULL, selection = c("any","all"), 
                               col = c("skyblue","red","orange"), alpha = NULL, 
                               pch = c(19,15), col.map = grey(0.5), 
                               legend = TRUE, interactive = TRUE, ...) {
  mapMiss_work(x, coords, map, delimiter, selection, col, alpha, pch, col.map,
               legend, interactive,...)
}

mapMiss.survey.design <- function(x, coords, map, delimiter = NULL, selection = c("any","all"), 
    col = c("skyblue","red","orange"), alpha = NULL, 
    pch = c(19,15), col.map = grey(0.5), 
    legend = TRUE, interactive = TRUE, ...) {
  mapMiss_work(x$variables, coords, map, delimiter, selection, col, alpha, pch, col.map,
               legend, interactive,...)
}

mapMiss.default <- function(x, coords, map, delimiter = NULL, selection = c("any","all"), 
                            col = c("skyblue","red","orange"), alpha = NULL, 
                            pch = c(19,15), col.map = grey(0.5), 
                            legend = TRUE, interactive = TRUE, ...) {
  mapMiss_work(as.data.frame(x), coords, map, delimiter, selection, col, alpha, pch, col.map,
               legend, interactive,...)
}

mapMiss_work <- function(x, coords, map, delimiter = NULL, selection = c("any","all"), 
        col = c("skyblue","red","orange"), alpha = NULL, 
        pch = c(19,15), col.map = grey(0.5), 
        legend = TRUE, interactive = TRUE, ...) {
    # error messages
	imputed <- FALSE # indicates if there are Variables with missing-index
    if(is.vector(x)) {
        nx <- length(x)
        px <- 1
    } else {
        if(!inherits(x, c("data.frame","matrix"))) {
            stop("'x must be a vector, data.frame or matrix")
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
    if(px > 1) selection <- match.arg(selection)
    if(length(col) == 0) col <- c("skyblue","red","orange")
    if(length(pch) == 0) pch <- c(19,15)
    if(length(col) == 1 && length(pch) == 1) {
        stop("same color and plot symbol for observed and missing values")
    }
    if(length(col) == 1) col <- rep(col, 3)
	else if(length(col) == 2) col <- rep(col,1:2)
    else if(length(col) > 3) col <- col[1:3]
    if(length(pch) == 1) pch <- rep(pch, 2)
    else if(length(pch) > 2) pch <- pch[1:2]
    # semitransparent colors
    if(!is.null(alpha)) col <- alphablend(col, alpha)  
    # vector that indicates missings
    if(!imputed) {
		miss <- isNA(x, selection)
		color <- col[2]
	} else {
		miss <- isImp(x, pos = NULL, delimiter = delimiter, imp_var = imp_var, selection = selection)[["missh"]]
		color <- col[3]
	}
    # create plot
    bgmap(map, col=col.map, ...)
    points(coords[!miss,], pch=pch[1], col=col[1], ...)
    points(coords[miss,], pch=pch[2], col=color, ...)
    if(legend) {  # add legend
        if(!imputed) {
			if(px == 1) legtext <- c("observed", "missing")
	        else if(selection == "any") legtext <- c("all observed","any missing")
	        else legtext <- c("any observed", "all missing")
			color <- col[1:2]
		} else {
			if(px == 1) legtext <- c("observed", "imputed")
			else if(selection == "any") legtext <- c("all observed","any imputed")
			else legtext <- c("any observed", "all imputed")
			color <- col[c(1,3)]
		}
		legend("topright", pch=pch, legend=legtext, col=color, bty="n")
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
        pos <- identifyPt(unlist(pt), coords)  # get closest point
        while(!is.na(pos)) {
            print(x[pos,])  # print values for the identified point
            pt <- locatorVIM()
            pos <- identifyPt(unlist(pt), coords)
        }
    }
    invisible()
}
