# ---------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         Vienna University of Technology
# ---------------------------------------

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
