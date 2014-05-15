\name{bgmap}
\alias{bgmap}
\title{Backgound map}
\usage{
bgmap(map, add = FALSE, ...)
}
\arguments{
  \item{map}{either a matrix or \code{data.frame} with two
  columns, a list with components \code{x} and \code{y}, or
  an object of any class that can be used for maps and
  provides its own plot method (e.g.,
  \code{"SpatialPolygons"} from package \code{sp}).  A list
  of the previously mentioned types can also be provided.}

  \item{add}{a logical indicating whether \code{map} should
  be added to an already existing plot (the default is
  \code{FALSE}).}

  \item{\dots}{further arguments and graphical parameters
  to be passed to \code{plot} and/or
  \code{\link[graphics]{lines}}.}
}
\description{
Plot a background map.
}
\examples{
data(kola.background, package = "VIM")
bgmap(kola.background)
}
\author{
Andreas Alfons
}
\references{
M. Templ, A. Alfons, P. Filzmoser (2012) Exploring
incomplete data using visualization tools.  \emph{Journal
of Advances in Data Analysis and Classification}, Online
first. DOI: 10.1007/s11634-011-0102-y.
}
\seealso{
\code{\link{growdotMiss}}, \code{\link{mapMiss}}
}
\keyword{hplot}

