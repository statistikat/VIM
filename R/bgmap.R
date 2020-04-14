# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------



#' Backgound map
#' 
#' Plot a background map.
#' 
#' 
#' @param map either a matrix or `data.frame` with two columns, a list
#' with components `x` and `y`, or an object of any class that can be
#' used for maps and provides its own plot method (e.g.,
#' `"SpatialPolygons"` from package `sp`).  A list of the previously
#' mentioned types can also be provided.
#' @param add a logical indicating whether `map` should be added to an
#' already existing plot (the default is `FALSE`).
#' @param \dots further arguments and graphical parameters to be passed to
#' `plot` and/or [graphics::lines()].
#' @author Andreas Alfons
#' @seealso [growdotMiss()], [mapMiss()]
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  *Journal of Advances in Data Analysis
#' and Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(kola.background, package = "VIM")
#' bgmap(kola.background)
#' 
#' @export bgmap
bgmap <- function(map, add=FALSE, ...) {
    if(is.character(map)) map <- get(map, envir=.GlobalEnv)
    # workhorse
    bgmap.plot <- function(x, add=FALSE, main="", xlab="", ylab="", 
                           axes=FALSE, ...) {
        if(inherits(x, c("matrix","data.frame","list"))) {
            if(!add) plot(x, type="n", main=main,      # set up
                xlab=xlab, ylab=ylab, axes=axes, ...)  # new plot
            lines(x, ...)  # draw lines
        } else {  # map with own class and plot method
            if(add) par(new=TRUE)
            plot(x, main=main, xlab=xlab, ylab=ylab, axes=axes, ...)
        }
        invisible()
    }
    if(inherits(map, "list") && !all(c("x","y") %in% names(map))) {
        # list containing the parts of the map
        mapply(bgmap.plot, map, c(add, rep(TRUE, length(map)-1)), ...)
    }
    else bgmap.plot(map, add=add, ...)
    invisible()
}
