# --------------------------------------
# Author: Andreas Alfons, Bernd Prantner
#         and Daniel Schopfhauser
#         Vienna University of Techology
# --------------------------------------

marginmatrix <- function(x, delimiter = NULL, 
                         col = c("skyblue","red","red4","orange","orange4"), 
                         alpha = NULL, ...) {
  UseMethod("marginmatrix", x)
}

marginmatrix.data.frame <- function(x, delimiter = NULL, 
                                    col = c("skyblue","red","red4","orange","orange4"), 
                                    alpha = NULL, ...) {
  marginmatrix_work(x, delimiter, col,alpha, ...)
}

marginmatrix.survey.design <- function(x, delimiter = NULL, 
                                       col = c("skyblue","red","red4","orange","orange4"), 
                                       alpha = NULL, ...) {
  marginmatrix_work(x$variables, delimiter, col,alpha, ...)
}

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
