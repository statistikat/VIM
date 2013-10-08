# ---------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         Vienna University of Technology
# ---------------------------------------



#' Rug representation of missing/imputed values
#' 
#' Add a rug representation of missing/imputed values in only one of the
#' variables to scatterplots.
#' 
#' If \code{side} is 1 or 3, the rug representation consists of values
#' available in \code{x} but missing/imputed in \code{y}.  Else if \code{side}
#' is 2 or 4, it consists of values available in \code{y} but missing/imputed
#' in \code{x}.
#' 
#' @param x,y numeric vectors.
#' @param ticksize the length of the ticks.  Positive lengths give inward
#' ticks.
#' @param side an integer giving the side of the plot to draw the rug
#' representation.
#' @param col the color to be used for the ticks.
#' @param alpha the alpha value (between 0 and 1).
#' @param miss a \code{data.frame} or \code{matrix} with two columns and
#' logical values. If \code{NULL}, \code{x} and \code{y} are searched for
#' missing values, otherwise, the first column of \code{miss} is used to
#' determine the imputed values in \code{x} and the second one for the imputed
#' values in \code{y}.
#' @param lwd the line width to be used for the ticks.
#' @param \dots further arguments to be passed to \code{\link[graphics]{Axis}}.
#' @author Andreas Alfons, modifications by Bernd Prantner
#' @keywords color
#' @examples
#' 
#' data(tao, package = "VIM")
#' ## for missing values
#' x <- tao[, "Air.Temp"]
#' y <- tao[, "Humidity"]
#' plot(x, y)
#' rugNA(x, y, side = 1)
#' rugNA(x, y, side = 2)
#' 
#' ## for imputed values
#' x_imp <- kNN(tao[, c("Air.Temp","Humidity")])
#' x <- x_imp[, "Air.Temp"]
#' y <- x_imp[, "Humidity"]
#' miss <- x_imp[, c("Air.Temp_imp","Humidity_imp")]
#' plot(x, y)
#' rugNA(x, y, side = 1, col = "orange", miss = miss)
#' rugNA(x, y, side = 2, col = "orange", miss = miss)
#' 
#' @export rugNA
rugNA <- function(x, y, ticksize = NULL, side = 1, 
        col = "red", alpha = NULL, miss = NULL, lwd = 0.5, ...) {
    if(length(col) == 0) { # default
		if(is.null(miss)) col <- "red"
		else col <- "orange"
	}
    else if(length(col) > 1) col <- col[1]  # values
    if(!is.null(alpha)) col <- alphablend(col, alpha)  # semitransparent colors
    x <- if(is.null(x)) numeric() else as.vector(x)
    y <- if(is.null(y)) numeric() else as.vector(y)
	if(is.null(miss)) {
		missx <- is.na(x)
		missy <- is.na(y)
	} else {
		missx <- miss[,1]
		missy <- miss[,2]
	}
    at <- if (side %% 2 == 1) x[missy] else y[missx]
    ok <- is.finite(at)
    at <- at[ok]
    ca <- as.call(list(Axis, at=at, ..., side=side))
    ca$tick <- TRUE
    ca$lwd <- 0
    ca$lwd.ticks <- lwd
    ca$col.ticks <- col
    ca$tck <- ticksize
    ca$labels <- FALSE
    eval(ca)
    invisible(at)
}
