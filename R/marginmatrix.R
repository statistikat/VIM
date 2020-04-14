# --------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         and Daniel Schopfhauser
#         Vienna University of Techology
# --------------------------------------



#' Marginplot Matrix
#' 
#' Create a scatterplot matrix with information about missing/imputed values in
#' the plot margins of each panel.
#' 
#' `marginmatrix` uses [pairsVIM()] with a panel function based
#' on [marginplot()].
#' 
#' The graphical parameter `oma` will be set unless supplied as an
#' argument.
#' 
#' @param x a matrix or `data.frame`.
#' @param delimiter a character-vector to distinguish between variables and
#' imputation-indices for imputed variables (therefore, `x` needs to have
#' [colnames()]). If given, it is used to determine the corresponding
#' imputation-index for any imputed variable (a logical-vector indicating which
#' values of the variable have been imputed). If such imputation-indices are
#' found, they are used for highlighting and the colors are adjusted according
#' to the given colors for imputed variables (see `col`).
#' @param col a vector of length five giving the colors to be used in the
#' marginplots in the off-diagonal panels.  The first color is used for the
#' scatterplot and the boxplots for the available data, the second/fourth color
#' for the univariate scatterplots and boxplots for the missing/imputed values
#' in one variable, and the third/fifth color for the frequency of
#' missing/imputed values in both variables (see \sQuote{Details}).  If only
#' one color is supplied, it is used for the bivariate and univariate
#' scatterplots and the boxplots for missing/imputed values in one variable,
#' whereas the boxplots for the available data are transparent.  Else if two
#' colors are supplied, the second one is recycled.
#' @param alpha a numeric value between 0 and 1 giving the level of
#' transparency of the colors, or `NULL`.  This can be used to prevent
#' overplotting.
#' @param \dots further arguments and graphical parameters to be passed to
#' [pairsVIM()] and [marginplot()].  `par("oma")` will
#' be set appropriately unless supplied (see [graphics::par()]).
#' @author Andreas Alfons, modifications by Bernd Prantner
#' @seealso [marginplot()], [pairsVIM()],
#' [scattmatrixMiss()]
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  *Journal of Advances in Data Analysis
#' and Classification*, Online first. DOI: 10.1007/s11634-011-0102-y.
#' @keywords hplot
#' @examples
#' 
#' data(sleep, package = "VIM")
#' ## for missing values
#' x <- sleep[, 1:5]
#' x[,c(1,2,4)] <- log10(x[,c(1,2,4)])
#' marginmatrix(x)
#' 
#' ## for imputed values
#' x_imp <- kNN(sleep[, 1:5])
#' x_imp[,c(1,2,4)] <- log10(x_imp[,c(1,2,4)])
#' marginmatrix(x_imp, delimiter = "_imp")
#' 
#' @export marginmatrix
marginmatrix <- function(x, delimiter = NULL, 
                         col = c("skyblue","red","red4","orange","orange4"), 
                         alpha = NULL, ...) {
  UseMethod("marginmatrix", x)
}

#' @rdname marginmatrix
#' @export

marginmatrix.data.frame <- function(x, delimiter = NULL, 
                                    col = c("skyblue","red","red4","orange","orange4"), 
                                    alpha = NULL, ...) {
  marginmatrix_work(x, delimiter, col,alpha, ...)
}

#' @rdname marginmatrix
#' @export

marginmatrix.survey.design <- function(x, delimiter = NULL, 
                                       col = c("skyblue","red","red4","orange","orange4"), 
                                       alpha = NULL, ...) {
  marginmatrix_work(x$variables, delimiter, col,alpha, ...)
}

#' @rdname marginmatrix
#' @export

marginmatrix.default <- function(x, delimiter = NULL, 
                                 col = c("skyblue","red","red4","orange","orange4"), 
                                 alpha = NULL, ...) {
  marginmatrix_work(as.data.frame(x), delimiter, col,alpha, ...)
}

marginmatrix_work <- function(x, delimiter = NULL, col = c("skyblue","red","red4","orange","orange4"), 
    alpha = NULL, ...) {
    panel.marginplot <- function(x, y, ...) {
        par(new=TRUE)
        localMarginplot <- function(..., numbers, 
                cex.numbers, ann, axes, frame.plot) {
            marginplot(..., delimiter=delimiter, col=col, alpha=alpha, 
                numbers=FALSE, ann=FALSE, axes=FALSE)
        }
        localMarginplot(cbind(x,y), ...)
    }
    localPairs <- function(..., panel, lower, upper, diagonal, 
            numbers, cex.numbers, ann, axes, frame.plot) {
        pairsVIM(..., delimiter=delimiter, panel=panel.marginplot)
    }
    localPairs(x, ...)
}
