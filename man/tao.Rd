\docType{data}
\name{tao}
\alias{tao}
\title{Tropical Atmosphere Ocean (TAO) project data}
\format{A data frame with 736 observations on the following 8 variables.
\describe{ \item{Year}{a numeric vector} \item{Latitude}{a
numeric vector} \item{Longitude}{a numeric vector}
\item{Sea.Surface.Temp}{a numeric vector} \item{Air.Temp}{a
numeric vector} \item{Humidity}{a numeric vector}
\item{UWind}{a numeric vector} \item{VWind}{a numeric
vector} }}
\source{
  \url{http://www.pmel.noaa.gov/tao/}
}
\description{
  A small subsample of the Tropical Atmosphere Ocean (TAO)
  project data, derived from the \code{GGOBI} project.
}
\details{
  All cases recorded for five locations and two time
  periods.
}
\examples{
data(tao, package = "VIM")
summary(tao)
aggr(tao)
}
\keyword{datasets}

