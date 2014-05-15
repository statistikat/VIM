\name{colSequence}
\alias{colSequence}
\alias{colSequenceHCL}
\alias{colSequenceRGB}
\title{HCL and RGB color sequences}
\usage{
colSequence(p, start, end, space = c("hcl", "rgb"), ...)

colSequenceRGB(p, start, end, gamma = 2.2, fixup = TRUE, ...)

colSequenceHCL(p, start, end, gamma = 2.2, fixup = TRUE, ...)
}
\arguments{
  \item{p}{a numeric vector in \eqn{$[0,1]$}{[0,1]} giving
  values to be used for interpolation between the start and
  end color (0 corresponds to the start color, 1 to the end
  color).}

  \item{start,end}{the start and end color, respectively.
  For HCL colors, each can be supplied as a vector of
  length three (hue, chroma, luminance) or an object of
  class "\code{\link[colorspace]{polarLUV}}".  For RGB
  colors, each can be supplied as a character string, a
  vector of length three (red, green, blue) or an object of
  class "\code{\link[colorspace]{RGB}}".}

  \item{space}{character string; if \code{start} and
  \code{end} are both numeric, this determines whether they
  refer to HCL or RGB values.  Possible values are
  \code{"hcl"} (for the HCL space) or \code{"rgb"} (for the
  RGB space).}

  \item{gamma}{numeric; the display \emph{gamma} value (see
  \code{\link[colorspace]{hex}}).}

  \item{fixup}{a logical indicating whether the colors
  should be corrected to valid RGB values (see
  \code{\link[colorspace]{hex}}).}

  \item{\dots}{for \code{colSequence}, additional arguments
  to be passed to \code{colSequenceHCL} or
  \code{colSequenceRGB}.  For \code{colSequenceHCL} and
  \code{colSequenceRGB}, additional arguments to be passed
  to \code{\link[colorspace]{hex}}.}
}
\value{
A character vector containing hexadecimal strings of the
form \code{"#RRGGBB"}.
}
\description{
Compute color sequences by linear interpolation based on a
continuous color scheme between certain start and end
colors.  Color sequences may thereby be computed in the
\emph{HCL} or \emph{RGB} color space.
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
\author{
Andreas Alfons
}
\references{
Zeileis, A., Hornik, K., Murrell, P. (2009) Escaping
RGBland: Selecting colors for statistical graphics.
\emph{Computational Statistics & Data Analysis}, \bold{53
(9)}, 1259--1270.
}
\seealso{
\code{\link[colorspace]{hex}},
\code{\link[colorspace]{sequential_hcl}}
}
\keyword{color}

