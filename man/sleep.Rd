\name{sleep}
\alias{sleep}
\docType{data}
\title{Mammal sleep data}
\description{
  Sleep data with missing values.
}
\usage{
data(sleep)
}
\format{
  A data frame with 62 observations on the following 10 variables.
  \describe{
    \item{\code{BodyWgt}}{a numeric vector}
    \item{\code{BrainWgt}}{a numeric vector}
    \item{\code{NonD}}{a numeric vector}
    \item{\code{Dream}}{a numeric vector}
    \item{\code{Sleep}}{a numeric vector}
    \item{\code{Span}}{a numeric vector}
    \item{\code{Gest}}{a numeric vector}
    \item{\code{Pred}}{a numeric vector}
    \item{\code{Exp}}{a numeric vector}
    \item{\code{Danger}}{a numeric vector}
  }
}
\source{
  Allison, T. and Chichetti, D. (1976) Sleep in mammals: ecological and 
  constitutional correlates. \emph{Science} \bold{194 (4266)}, 732--734.

  The data set was imported from \code{GGobi}.
}
\examples{
data(sleep, package = "VIM")
summary(sleep)
aggr(sleep)
}
\keyword{datasets}
