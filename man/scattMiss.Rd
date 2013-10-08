\name{scattMiss}
\alias{scattMiss}
\title{Scatterplot with information about missing/imputed values}
\usage{
  scattMiss(x, delimiter = NULL, side = 1,
    col = c("skyblue", "red", "orange", "lightgrey"),
    alpha = NULL, lty = c("dashed", "dotted"),
    lwd = par("lwd"), quantiles = c(0.5, 0.975),
    inEllipse = FALSE, zeros = FALSE, xlim = NULL,
    ylim = NULL, main = NULL, sub = NULL, xlab = NULL,
    ylab = NULL, interactive = TRUE, ...)
}
\arguments{
  \item{x}{a \code{matrix} or \code{data.frame} with two
  columns.}

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

  \item{side}{if \code{side=1}, a rug representation and
  vertical lines are plotted for the missing/imputed values
  in the second variable; if \code{side=2}, a rug
  representation and horizontal lines for the
  missing/imputed values in the first variable.}

  \item{col}{a vector of length four giving the colors to
  be used in the plot. The first color is used for the
  scatterplot, the second/third color for the rug
  representation for missing/imputed values. The second
  color is also used for the lines for missing values.
  Imputed values will be highlighted with the third color,
  and the fourth color is used for the ellipses (see
  \sQuote{Details}). If only one color is supplied, it is
  used for the scatterplot, the rug representation and the
  lines, whereas the default color is used for the
  ellipses.  Else if a vector of length two is supplied,
  the default color is used for the ellipses as well.}

  \item{alpha}{a numeric value between 0 and 1 giving the
  level of transparency of the colors, or \code{NULL}.
  This can be used to prevent overplotting.}

  \item{lty}{a vector of length two giving the line types
  for the lines and ellipses.  If a single value is
  supplied, it will be used for both.}

  \item{lwd}{a vector of length two giving the line widths
  for the lines and ellipses.  If a single value is
  supplied, it will be used for both.}

  \item{quantiles}{a vector giving the quantiles of the
  chi-square distribution to be used for the tolerance
  ellipses, or \code{NULL} to suppress plotting ellipses
  (see \sQuote{Details}).}

  \item{inEllipse}{plot lines only inside the largest
  ellipse.  Ignored if \code{quantiles} is \code{NULL} or
  if there are imputed values.}

  \item{zeros}{a logical vector of length two indicating
  whether the variables are semi-continuous, i.e., contain
  a considerable amount of zeros.  If \code{TRUE}, only the
  non-zero observations are used for computing the
  tolerance ellipses.  If a single logical is supplied, it
  is recycled. Ignored if \code{quantiles} is \code{NULL}.}

  \item{xlim,ylim}{axis limits.}

  \item{main,sub}{main and sub title.}

  \item{xlab,ylab}{axis labels.}

  \item{interactive}{a logical indicating whether the
  \code{side} argument can be changed interactively (see
  \sQuote{Details}).}

  \item{\dots}{further graphical parameters to be passed
  down (see \code{\link[graphics]{par}}).}
}
\description{
  In addition to a standard scatterplot, lines are plotted
  for the missing values in one variable. If there are
  imputed values, they will be highlighted.
}
\details{
  Information about missing values in one variable is
  included as vertical or horizontal lines, as determined
  by the \code{side} argument.  The lines are thereby drawn
  at the observed x- or y-value. In case of imputed values,
  they will additionally be highlighted in the scatterplot.
  Supplementary, percentage coverage ellipses can be drawn
  to give a clue about the shape of the bivariate data
  distribution.

  If \code{interactive}is \code{TRUE}, clicking in the
  bottom margin redraws the plot with information about
  missing/imputed values in the first variable and clicking
  in the left margin redraws the plot with information
  about missing/imputed values in the second variable.
  Clicking anywhere else in the plot quits the interactive
  session.
}
\note{
  The argument \code{zeros} has been introduced in version
  1.4. As a result, some of the argument positions have
  changed.
}
\examples{
data(tao, package = "VIM")
## for missing values
scattMiss(tao[,c("Air.Temp", "Humidity")])

## for imputed values
scattMiss(kNN(tao[,c("Air.Temp", "Humidity")]), delimiter = "_imp")
}
\author{
  Andreas Alfons, modifications by Bernd Prantner
}
\references{
  M. Templ, A. Alfons, P. Filzmoser (2012) Exploring
  incomplete data using visualization tools.  \emph{Journal
  of Advances in Data Analysis and Classification}, Online
  first. DOI: 10.1007/s11634-011-0102-y.
}
\seealso{
  \code{\link{marginplot}}
}
\keyword{hplot}

