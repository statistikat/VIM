\name{print.aggr}
\alias{print.aggr}
\title{Print method for objects of class aggr}
\description{
  Print method for objects of class \code{"aggr"}.
}
\usage{
\method{print}{aggr}(x, digits = NULL, \dots)
}
\arguments{
  \item{x}{an object of class \code{"aggr"}.}
  \item{digits}{the minimum number of significant digits to be used (see 
    \code{\link{print.default}}).}
  \item{\dots}{further arguments (currently ignored).}
}
\author{Matthias Templ, modifications by Andreas Alfons and Bernd Prantner}
\seealso{\code{\link{aggr}}}
\examples{
data(sleep, package = "VIM")
a <- aggr(sleep, plot=FALSE)
a
}
\keyword{print}
