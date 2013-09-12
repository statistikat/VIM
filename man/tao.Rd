\name{tao}
\alias{tao}
\docType{data}
\title{Tropical Atmosphere Ocean (TAO) project data}
\description{
  A small subsample of the Tropical Atmosphere Ocean (TAO) project data, 
  derived from the \code{GGOBI} project.
}
\usage{
data(tao)
}
\format{
  A data frame with 736 observations on the following 8 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Latitude}}{a numeric vector}
    \item{\code{Longitude}}{a numeric vector}
    \item{\code{Sea.Surface.Temp}}{a numeric vector}
    \item{\code{Air.Temp}}{a numeric vector}
    \item{\code{Humidity}}{a numeric vector}
    \item{\code{UWind}}{a numeric vector}
    \item{\code{VWind}}{a numeric vector}
  }
}
\details{
All cases recorded for five locations and two time periods.
}
\source{
  \url{http://www.pmel.noaa.gov/tao/}
}
\examples{
data(tao, package = "VIM")
summary(tao)
aggr(tao)
}
\keyword{datasets}
