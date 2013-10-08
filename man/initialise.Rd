\name{initialise}
\alias{initialise}
\title{Initialization of missing values}
\usage{
  initialise(x, mixed, method = "kNN",
    mixed.constant = NULL)
}
\arguments{
  \item{x}{a vector.}

  \item{mixed}{a character vector containing the names of
  variables of type mixed (semi-continous).}

  \item{method}{Method used for Initialization (median or
  kNN)}

  \item{mixed.constant}{vector with length equal to the
  number of semi-continuous variables specifying the point
  of the semi-continuous distribution with non-zero
  probability}
}
\value{
  the initialized vector.
}
\description{
  Rough estimation of missing values in a vector according
  to its type.
}
\details{
  Missing values are imputed with the mean for vectors of
  class \code{"numeric"}, with the median for vectors of
  class \code{"integer"}, and with the mode for vectors of
  class \code{"factor"}.  Hence, \code{x} should be
  prepared in the following way: assign class
  \code{"numeric"} to numeric vectors, assign class
  \code{"integer"} to ordinal vectors, and assign class
  \code{"factor"} to nominal or binary vectors.
}
\note{
  The function is used internally by some imputation
  algorithms.
}
\author{
  Matthias Templ, modifications by Andreas Alfons
}
\keyword{manip}

