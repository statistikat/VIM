\name{summary.aggr}
\alias{summary.aggr}
\title{Summary method for objects of class aggr}
\description{
  Summary method for objects of class \code{"aggr"}.
}
\usage{
\method{summary}{aggr}(object, \dots)
}
\arguments{
  \item{object}{an object of class \code{"aggr"}.}
  \item{\dots}{further arguments.}
}
\value{
  a list of class \code{"summary.aggr"} containing the following components:
  \item{missings}{a \code{data.frame} containing the amount of missing or imputed values 
    in each variable.}
  \item{combinations}{a \code{data.frame} containing a character vector 
    representing the combinations of variables along with their frequencies 
    and percentages.}
}
\author{Matthias Templ, modifications by Andreas Alfons}
\seealso{
  \code{\link{print.summary.aggr}}, \code{\link{aggr}}
}
\examples{
data(sleep, package = "VIM")
summary(aggr(sleep, plot=FALSE))
}
\keyword{print}
