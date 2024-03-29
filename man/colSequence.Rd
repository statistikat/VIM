% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colSequence.R
\name{colSequence}
\alias{colSequence}
\alias{colSequenceRGB}
\alias{colSequenceHCL}
\title{HCL and RGB color sequences}
\usage{
colSequence(p, start, end, space = c("hcl", "rgb"), ...)

colSequenceRGB(p, start, end, fixup = TRUE, ...)

colSequenceHCL(p, start, end, fixup = TRUE, ...)
}
\arguments{
\item{p}{a numeric vector with values between 0 and 1 giving values to be used
for interpolation between the start and end color (0 corresponds to the
start color, 1 to the end color).}

\item{start, end}{the start and end color, respectively.  For HCL colors,
each can be supplied as a vector of length three (hue, chroma, luminance) or
an object of class "\code{\link[colorspace:polarLUV]{colorspace::polarLUV()}}".  For RGB colors,
each can be supplied as a character string, a vector of length three (red,
green, blue) or an object of class "\code{\link[colorspace:RGB]{colorspace::RGB()}}".}

\item{space}{character string; if \code{start} and \code{end} are both
numeric, this determines whether they refer to HCL or RGB values.  Possible
values are \code{"hcl"} (for the HCL space) or \code{"rgb"} (for the RGB
space).}

\item{\dots}{for \code{colSequence}, additional arguments to be passed to
\code{colSequenceHCL} or \code{colSequenceRGB}.  For \code{colSequenceHCL}
and \code{colSequenceRGB}, additional arguments to be passed to
\code{\link[colorspace:hex]{colorspace::hex()}}.}

\item{fixup}{a logical indicating whether the colors should be corrected to
valid RGB values (see \code{\link[colorspace:hex]{colorspace::hex()}}).}
}
\value{
A character vector containing hexadecimal strings of the form
\code{"#RRGGBB"}.
}
\description{
Compute color sequences by linear interpolation based on a continuous color
scheme between certain start and end colors.  Color sequences may thereby be
computed in the \emph{HCL} or \emph{RGB} color space.
}
\examples{

p <- c(0, 0.3, 0.55, 0.8, 1)

## HCL colors
colSequence(p, c(0, 0, 100), c(0, 100, 50))
colSequence(p, polarLUV(L=90, C=30, H=90), c(0, 100, 50))

## RGB colors
colSequence(p, c(1, 1, 1), c(1, 0, 0), space="rgb")
colSequence(p, RGB(1, 1, 0), "red")

}
\references{
Zeileis, A., Hornik, K., Murrell, P. (2009) Escaping RGBland:
Selecting colors for statistical graphics. \emph{Computational Statistics &
Data Analysis}, \strong{53 (9)}, 1259--1270.
}
\seealso{
\code{\link[colorspace:hex]{colorspace::hex()}},
\code{\link[colorspace:hcl_palettes]{colorspace::sequential_hcl()}}
}
\author{
Andreas Alfons
}
\keyword{color}
