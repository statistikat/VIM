\name{print.summary.aggr}
\alias{print.summary.aggr}
\title{Print method for objects of class summary.aggr}
\usage{
\method{print}{summary.aggr}(x, ...)
}
\arguments{
  \item{x}{an object of class \code{"summary.aggr"}.}

  \item{\dots}{Further arguments, currently ignored.}
}
\description{
Print method for objects of class \code{"summary.aggr"}.
}
\examples{
data(sleep, package = "VIM")
s <- summary(aggr(sleep, plot=FALSE))
s
}
\author{
Andreas Alfons, modifications by Bernd Prantner
}
\seealso{
\code{\link{summary.aggr}}, \code{\link{aggr}}
}
\keyword{print}

