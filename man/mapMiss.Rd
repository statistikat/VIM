\name{mapMiss}
\alias{mapMiss}
\title{Map with information about missing/imputed values}
\usage{
  mapMiss(x, coords, map, delimiter = NULL,
    selection = c("any", "all"),
    col = c("skyblue", "red", "orange"), alpha = NULL,
    pch = c(19, 15), col.map = grey(0.5), legend = TRUE,
    interactive = TRUE, ...)
}
\arguments{
  \item{x}{a vector, matrix or \code{data.frame}.}

  \item{coords}{a \code{data.frame} or matrix with two
  columns giving the spatial coordinates of the
  observations.}

  \item{map}{a background map to be passed to
  \code{\link{bgmap}}.}

  \item{delimiter}{a character-vector to distinguish
  between variables and imputation-indices for imputed
  variables (therefore, \code{x} needs to have
  \code{\link{colnames}}). If given, it is used to
  determine the corresponding imputation-index for any
  imputed variable (a logical-vector indicating which
  values of the variable have been imputed). If such
  imputation-indices are found, they are used for
  highlighting and the colors are adjusted according to the
  given colors for imputed variables (see \code{col}).}

  \item{selection}{the selection method for displaying
  missing/imputed values in the map.  Possible values are
  \code{"any"} (display missing/imputed values in
  \emph{any} variable) and \code{"all"} (display
  missing/imputed values in \emph{all} variables).}

  \item{col}{a vector of length three giving the colors to
  be used for observed, missing and imputed values.  If a
  single color is supplied, it is used for all values.}

  \item{alpha}{a numeric value between 0 and 1 giving the
  level of transparency of the colors, or \code{NULL}.
  This can be used to prevent overplotting.}

  \item{pch}{a vector of length two giving the plot
  characters to be used for observed and missing/imputed
  values.  If a single plot character is supplied, it will
  be used for both.}

  \item{col.map}{the color to be used for the background
  map.}

  \item{legend}{a logical indicating whether a legend
  should be plotted.}

  \item{interactive}{a logical indicating whether
  information about selected observations can be displayed
  interactively (see \sQuote{Details}).}

  \item{\dots}{further graphical parameters to be passed to
  \code{\link{bgmap}} and \code{\link[graphics]{points}}.}
}
\description{
  Map of observed and missing/imputed values.
}
\details{
  If \code{interactive=TRUE}, detailed information for an
  observation can be printed on the console by clicking on
  the corresponding point.  Clicking in a region that does
  not contain any points quits the interactive session.
}
\examples{
data(chorizonDL, package = "VIM")
data(kola.background, package = "VIM")
coo <- chorizonDL[, c("XCOO", "YCOO")]
## for missing values
x <- chorizonDL[, c("As", "Bi")]
mapMiss(x, coo, kola.background)

## for imputed values
x_imp <- kNN(chorizonDL[, c("As", "Bi")])
mapMiss(x_imp, coo, kola.background, delimiter = "_imp")
}
\author{
  Matthias Templ, Andreas Alfons, modifications by Bernd
  Prantner
}
\references{
  M. Templ, A. Alfons, P. Filzmoser (2012) Exploring
  incomplete data using visualization tools.  \emph{Journal
  of Advances in Data Analysis and Classification}, Online
  first. DOI: 10.1007/s11634-011-0102-y.
}
\seealso{
  \code{\link{bgmap}}, \code{\link{bubbleMiss}},
  \code{\link{colormapMiss}}
}
\keyword{hplot}

