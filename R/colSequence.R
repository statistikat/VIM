# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------

colSequence <- function(p, start, end, space = c("hcl", "rgb"), ...) {
    # initializations
    anyHCL <- is(start, "polarLUV") || is(end, "polarLUV")
    anyRGB <- is.character(start) || is(start, "RGB") || 
        is.character(end) || is(end, "RGB")
    if(anyHCL && anyRGB) stop("mix of HCL and RGB colors")
#    if(!isTRUE(space[1] == "hcl") && anyHCL) {
#        space <- "hcl"
#        warning("colors supplied as \"polarLUV\": using HCL space")
#    } else if(!isTRUE(space[1] == "rgb") && anyRGB) {
#        space <- "rgb"
#        warning("colors supplied as strings or \"RGB\": using RGB space")
#    } else space <- match.arg(space)
    if(anyHCL) space <- "hcl"
    else if(anyRGB) space <- "rgb"
    else space <- match.arg(space)
    if(is.character(start)) start <- col2rgb(rep(start, length.out=1))/255
    if(is.character(end)) end <- col2rgb(rep(end, length.out=1))/255
    # call function for RGB or HCL
    if(space == "hcl") colSequenceHCL(p, start, end, ...)
    else colSequenceRGB(p, start, end, ...)
}

colSequenceRGB <- function(p, start, end, gamma = 2.2, fixup = TRUE, ...) {
    # initializations
    if(is(start, "RGB")) start <- coords(start)[1,]
    else start <- rep(start, length.out=3)
    if(is(end, "RGB")) end <- coords(end)[1,]
    else end <- rep(end, length.out=3)
    # compute RGB values
    r <- start[1] + p * (end[1] - start[1])
    g <- start[2] + p * (end[2] - start[2])
    b <- start[3] + p * (end[3] - start[3])
    # return hexadecimal strings
    hex(RGB(r, g, b), gamma=gamma, fixup=fixup, ...)
}

colSequenceHCL <- function(p, start, end, gamma = 2.2, fixup = TRUE, ...) {
    # initializations
    if(is(start, "polarLUV")) start <- rev(coords(start)[1,])
    else start <- rep(start, length.out=3)
    if(is(end, "polarLUV")) end <- rev(coords(end)[1,])
    else end <- rep(end, length.out=3)
    # compute HCL values
    h <- start[1] + p * (end[1] - start[1])
    c <- start[2] + p * (end[2] - start[2])
    l <- start[3] + p * (end[3] - start[3])
    # return hexadecimal strings
    hex(polarLUV(L=l, C=c, H=h), gamma=gamma, fixup=fixup, ...)
}
