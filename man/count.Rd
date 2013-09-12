\name{count}
\alias{countInf}
\alias{countNA}
\title{Count number of infinite or missing values}
\description{
    Count the number of infinite or missing values in a vector.
}
\usage{
countInf(x)

countNA(x)
}
\arguments{
    \item{x}{a vector.}
}
\value{
    \code{countInf} returns the number of infinite values in \code{x}.
    
    \code{countNA} returns the number of missing values in \code{x}.
}
\author{Andreas Alfons}
\examples{
data(sleep, package="VIM")
countInf(log(sleep$Dream))
countNA(sleep$Dream)
}
\keyword{utilities}
