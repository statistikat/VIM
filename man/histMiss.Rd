\name{histMiss}
\alias{histMiss}
\title{Histogram with information about missing/imputed values}
\usage{
  histMiss(x, delimiter = NULL, pos = 1,
    selection = c("any", "all"), breaks = "Sturges",
    right = TRUE,
    col = c("skyblue", "red", "skyblue4", "red4", "orange", "orange4"),
    border = NULL, main = NULL, sub = NULL, xlab = NULL,
    ylab = NULL, axes = TRUE, only.miss = TRUE,
    miss.labels = axes, interactive = TRUE, ...)
}
\arguments{
  \item{x}{a vector, matrix or \code{data.frame}.}

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

  \item{pos}{a numeric value giving the index of the
  variable of interest. Additional variables in \code{x}
  are used for highlighting.}

  \item{selection}{the selection method for highlighting
  missing/imputed values in multiple additional variables.
  Possible values are \code{"any"} (highlighting of
  missing/imputed values in \emph{any} of the additional
  variables) and \code{"all"} (highlighting of
  missing/imputed values in \emph{all} of the additional
  variables).}

  \item{breaks}{either a character string naming an
  algorithm to compute the breakpoints (see
  \code{\link{hist}}), or a numeric value giving the number
  of cells.}

  \item{right}{logical; if \code{TRUE}, the histogram cells
  are right-closed (left-open) intervals.}

  \item{col}{a vector of length six giving the colors to be
  used. If only one color is supplied, the bars are
  transparent and the supplied color is used for
  highlighting missing/imputed values.  Else if two colors
  are supplied, they are recycled.}

  \item{border}{the color to be used for the border of the
  cells.  Use \code{border=NA} to omit borders.}

  \item{main,sub}{main and sub title.}

  \item{xlab,ylab}{axis labels.}

  \item{axes}{a logical indicating whether axes should be
  drawn on the plot.}

  \item{only.miss}{logical; if \code{TRUE}, the
  missing/imputed values in the first variable are
  visualized by a single bar.  Otherwise, a small barplot
  is drawn on the right hand side (see \sQuote{Details}).}

  \item{miss.labels}{either a logical indicating whether
  label(s) should be plotted below the bar(s) on the right
  hand side, or a character string or vector giving the
  label(s) (see \sQuote{Details}).}

  \item{interactive}{a logical indicating whether the
  variables can be switched interactively (see
  \sQuote{Details}).}

  \item{\dots}{further graphical parameters to be passed to
  \code{\link[graphics]{title}} and
  \code{\link[graphics]{axis}}.}
}
\value{
  a list with the following components: - breaks the
  breakpoints. - counts the number of observations in each
  cell. - missings the number of highlighted observations
  in each cell. - mids the cell midpoints.
}
\description{
  Histogram with highlighting of missing/imputed values in
  other variables by splitting each bin into two parts.
  Additionally, information about missing/imputed values in
  the variable of interest is shown on the right hand side.
}
\details{
  If more than one variable is supplied, the bins for the
  variable of interest will be split according to
  missingness/number of imputed missings in the additional
  variables.

  If \code{only.miss=TRUE}, the missing/imputed values in
  the variable of interest are visualized by one bar on the
  right hand side.  If additional variables are supplied,
  this bar is again split into two parts according to
  missingness/number of imputed missings in the additional
  variables.

  Otherwise, a small barplot consisting of two bars is
  drawn on the right hand side.  The first bar corresponds
  to observed values in the variable of interest and the
  second bar to missing/imputed values.  Since these two
  bars are not on the same scale as the main barplot, a
  second y-axis is plotted on the right (if
  \code{axes=TRUE}).  Each of the two bars are again split
  into two parts according to missingness/number of imputed
  missings in the additional variables.  Note that this
  display does not make sense if only one variable is
  supplied, therefore \code{only.miss} is ignored in that
  case.

  If \code{interactive=TRUE}, clicking in the left margin
  of the plot results in switching to the previous variable
  and clicking in the right margin results in switching to
  the next variable.  Clicking anywhere else on the
  graphics device quits the interactive session.  When
  switching to a categorical variable, a barplot is
  produced rather than a histogram.
}
\note{
  Some of the argument names and positions have changed
  with version 1.3 due to extended functionality and for
  more consistency with other plot functions in \code{VIM}.
  For back compatibility, the arguments \code{axisnames}
  and \code{names.miss} can still be supplied to
  \code{\dots{}} and are handled correctly.  Nevertheless,
  they are deprecated and no longer documented.  Use
  \code{miss.labels} instead.
}
\examples{
data(tao, package = "VIM")
## for missing values
x <- tao[, c("Air.Temp", "Humidity")]
histMiss(x)
histMiss(x, only.miss = FALSE)

## for imputed values
x_IMPUTED <- kNN(tao[, c("Air.Temp", "Humidity")])
histMiss(x_IMPUTED, delimiter = "_imp")
histMiss(x_IMPUTED, delimiter = "_imp", only.miss = FALSE)
}
\author{
  Andreas Alfons, Bernd Prantner
}
\references{
  M. Templ, A. Alfons, P. Filzmoser (2012) Exploring
  incomplete data using visualization tools.  \emph{Journal
  of Advances in Data Analysis and Classification}, Online
  first. DOI: 10.1007/s11634-011-0102-y.
}
\seealso{
  \code{\link{spineMiss}}, \code{\link{barMiss}}
}
\keyword{hplot}

