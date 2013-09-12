\name{pairsVIM}
\alias{pairsVIM}
\title{Scatterplot Matrices}
\description{
    Create a scatterplot matrix.
}
\usage{
pairsVIM(x, \dots, delimiter = NULL, main = NULL, sub = NULL,
    panel = points, lower = panel, upper = panel, diagonal = NULL,
    labels = TRUE, pos.labels = NULL, cex.labels = NULL,
    font.labels = par("font"), layout = c("matrix","graph"), gap = 1)
}
\arguments{
    \item{x}{a matrix or \code{data.frame}.}
  	\item{delimiter}{a character-vector to distinguish between variables
    	and imputation-indices for imputed variables (therefore, \code{x} needs
    	to have \code{\link{colnames}}). If given, it is used to determine the corresponding
    	imputation-index for any imputed variable (a logical-vector indicating
    	which values of the variable have been imputed). If such imputation-indices
    	are found, they are used for highlighting and the colors are adjusted 
    	according to the given colors for imputed variables (see \code{col}).}
    \item{main, sub}{main and sub title.}
    \item{panel}{a \code{function(x, y, \dots)}, which is used to plot
        the contents of each off-diagonal panel of the display.}
    \item{\dots}{further arguments and graphical parameters to be passed 
        down.  \code{par("oma")} will be set appropriately unless supplied 
        (see \code{\link[graphics]{par}}).}
    \item{lower, upper}{separate panel functions to be used below and above 
        the diagonal, respectively.}
    \item{diagonal}{optional \code{function(x, \dots)} to be
        applied on the diagonal panels.}
    \item{labels}{either a logical indicating whether labels should be 
        plotted in the diagonal panels, or a character vector giving 
        the labels.}
    \item{pos.labels}{the vertical position of the labels in the diagonal 
      panels.}
    \item{cex.labels}{the character expansion factor to be used for the 
        labels.}
    \item{font.labels}{the font to be used for the labels.}
    \item{layout}{a character string giving the layout of the scatterplot 
        matrix.  Possible values are \code{"matrix"} (a matrix-like layout 
        with the first row on top) and \code{"graph"} (a graph-like layout 
        with the first row at the bottom).}
    \item{gap}{a numeric value giving the distance between the panels in 
        margin lines.}
}
\details{
    This function is the workhorse for \code{\link{marginmatrix}} and 
    \code{\link{scattmatrixMiss}}.
    
    The graphical parameter \code{oma} will be set unless supplied as an 
    argument.
    
    A panel function should not attempt to start a new plot, since the 
    coordinate system for each panel is set up by \code{pairsVIM}.
}
\note{
    The code is based on \code{\link[graphics]{pairs}}.  Starting with version 
    1.4, infinite values are no longer removed before passing the \code{x} and 
    \code{y} vectors to the panel functions.
}
\references{
M. Templ, A. Alfons, P. Filzmoser (2012)  
Exploring incomplete data using visualization tools. 
\emph{Journal of Advances in Data Analysis and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.  
}
\author{Andreas Alfons, modifications by Bernd Prantner}
\seealso{\code{\link{marginmatrix}}, \code{\link{scattmatrixMiss}}}
\examples{
data(sleep, package = "VIM")
x <- sleep[, -(8:10)]
x[,c(1,2,4,6,7)] <- log10(x[,c(1,2,4,6,7)])
pairsVIM(x)
}
\keyword{hplot}
