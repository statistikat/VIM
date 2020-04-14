# ---------------------------------------
# Author: Andreas Alfons
#         Vienna University of Technology
# ---------------------------------------



#' HCL and RGB color sequences
#' 
#' Compute color sequences by linear interpolation based on a continuous color
#' scheme between certain start and end colors.  Color sequences may thereby be
#' computed in the *HCL* or *RGB* color space.
#' 
#'
#' @rdname colSequence 
#' @aliases colSequence colSequenceRGB colSequenceHCL
#' @param p a numeric vector in \eqn{$[0,1]$}{[0,1]} giving values to be used
#' for interpolation between the start and end color (0 corresponds to the
#' start color, 1 to the end color).
#' @param start,end the start and end color, respectively.  For HCL colors,
#' each can be supplied as a vector of length three (hue, chroma, luminance) or
#' an object of class "[colorspace::polarLUV()]".  For RGB colors,
#' each can be supplied as a character string, a vector of length three (red,
#' green, blue) or an object of class "[colorspace::RGB()]".
#' @param space character string; if `start` and `end` are both
#' numeric, this determines whether they refer to HCL or RGB values.  Possible
#' values are `"hcl"` (for the HCL space) or `"rgb"` (for the RGB
#' space).
#' @param fixup a logical indicating whether the colors should be corrected to
#' valid RGB values (see [colorspace::hex()]).
#' @param \dots for `colSequence`, additional arguments to be passed to
#' `colSequenceHCL` or `colSequenceRGB`.  For `colSequenceHCL`
#' and `colSequenceRGB`, additional arguments to be passed to
#' [colorspace::hex()].
#' @return A character vector containing hexadecimal strings of the form
#' `"#RRGGBB"`.
#' @author Andreas Alfons
#' @seealso [colorspace::hex()],
#' [colorspace::sequential_hcl()]
#' @references Zeileis, A., Hornik, K., Murrell, P. (2009) Escaping RGBland:
#' Selecting colors for statistical graphics. *Computational Statistics &
#' Data Analysis*, **53 (9)**, 1259--1270.
#' @keywords color
#' @examples
#' 
#' p <- c(0, 0.3, 0.55, 0.8, 1)
#' 
#' ## HCL colors
#' colSequence(p, c(0, 0, 100), c(0, 100, 50))
#' colSequence(p, polarLUV(L=90, C=30, H=90), c(0, 100, 50))
#' 
#' ## RGB colors
#' colSequence(p, c(1, 1, 1), c(1, 0, 0), space="rgb")
#' colSequence(p, RGB(1, 1, 0), "red")
#'
#' @export colSequence
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
#' @export colSequenceRGB
#' @rdname colSequence
colSequenceRGB <- function(p, start, end, fixup = TRUE, ...) {
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
    hex(RGB(r, g, b), fixup=fixup, ...)
}
#' @export colSequenceHCL
#' @rdname colSequence
colSequenceHCL <- function(p, start, end, fixup = TRUE, ...) {
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
    hex(polarLUV(L=l, C=c, H=h), fixup=fixup, ...)
}
