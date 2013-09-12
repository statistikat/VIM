# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

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
