% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{prepare}
\alias{prepare}
\title{Transformation and standardization}
\usage{
prepare(
  x,
  scaling = c("none", "classical", "MCD", "robust", "onestep"),
  transformation = c("none", "minus", "reciprocal", "logarithm", "exponential", "boxcox",
    "clr", "ilr", "alr"),
  alpha = NULL,
  powers = NULL,
  start = 0,
  alrVar
)
}
\arguments{
\item{x}{a vector, matrix or \code{data.frame}.}

\item{scaling}{the scaling to be applied to the data.  Possible values are
\code{"none"}, \code{"classical"}, \code{MCD}, \code{"robust"} and
\code{"onestep"}.}

\item{transformation}{the transformation of the data.  Possible values are
\code{"none"}, \code{"minus"}, \code{"reciprocal"}, \code{"logarithm"},
\code{"exponential"}, \code{"boxcox"}, \code{"clr"}, \code{"ilr"} and
\code{"alr"}.}

\item{alpha}{a numeric parameter controlling the size of the subset for the
\emph{MCD} (if \code{scaling="MCD"}). See \code{\link[robustbase:covMcd]{robustbase::covMcd()}}.}

\item{powers}{a numeric vector giving the powers to be used in the Box-Cox
transformation (if \code{transformation="boxcox"}).  If \code{NULL}, the
powers are calculated with function \code{\link[car:powerTransform]{car::powerTransform()}}.}

\item{start}{a constant to be added prior to Box-Cox transformation (if
\code{transformation="boxcox"}).}

\item{alrVar}{variable to be used as denominator in the additive logratio
transformation (if \code{transformation="alr"}).}
}
\value{
Transformed and standardized data.
}
\description{
This function is used by the \code{VIM} GUI for transformation and
standardization of the data.
}
\details{
\strong{Transformation}:

\code{"none"}: no transformation is used.

\code{"logarithm"}: compute the the logarithm (to the base 10).

\code{"boxcox"}: apply a Box-Cox transformation. Powers may be specified or
calculated with the function \code{\link[car:powerTransform]{car::powerTransform()}}.

\strong{Standardization}:

\code{"none"}: no standardization is used.

\code{"classical"}: apply a \emph{z}-Transformation on each variable by
using function \code{\link[=scale]{scale()}}.

\code{"robust"}: apply a robustified \emph{z}-Transformation by using median
and MAD.
}
\examples{

data(sleep, package = "VIM")
x <- sleep[, c("BodyWgt", "BrainWgt")]
prepare(x, scaling = "robust", transformation = "logarithm")

}
\seealso{
\code{\link[=scale]{scale()}}, \code{\link[car:powerTransform]{car::powerTransform()}}
}
\author{
Matthias Templ, modifications by Andreas Alfons
}
\keyword{manip}
