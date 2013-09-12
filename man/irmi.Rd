\name{irmi}
\Rdversion{1.1}
\alias{irmi}
\title{
Iterative robust model-based imputation (IRMI) 
}
\description{
In each step of the iteration,
one variable is used as a response variable and the remaining variables
serve as the regressors.
}
\usage{
irmi(x, eps = 5, maxit = 100, mixed = NULL, mixed.constant=NULL, count = NULL, step = FALSE, 
    robust = FALSE, takeAll = TRUE, noise = TRUE, noise.factor = 1,
    force = FALSE, robMethod = "MM", force.mixed = TRUE, mi = 1,
    addMixedFactors = FALSE, trace = FALSE,init.method="kNN")
}
\arguments{
  \item{x}{
data.frame or matrix
}
  \item{eps}{
threshold for convergency
}
\item{maxit}{
maximum number of iterations
}
\item{mixed}{
column index of the semi-continuous variables
}
\item{mixed.constant}{
vector with length equal to the number of semi-continuous variables specifying the point
of the semi-continuous distribution with non-zero probability
}
\item{count}{
column index of count variables
}

  \item{step}{
a stepwise model selection is applied when the parameter is set to TRUE
}
  \item{robust}{
if TRUE, robust regression methods will be applied
}
  \item{takeAll}{
  takes information of (initialised) missings in the response as well for regression imputation.
}
  \item{noise}{
irmi has the option to add a random error term to the imputed values, this
creates the possibility for multiple imputation. The error term has mean 0 and
variance corresponding to the variance of the
regression residuals.
}
  \item{noise.factor}{
amount of noise.
}
  \item{force}{
if TRUE, the algorithm tries to find a solution in any case, possible by using different robust methods automatically.
}
  \item{robMethod}{
regression method when the response is continuous.
}
  \item{force.mixed}{
if TRUE, the algorithm tries to find a solution in any case, possible by using different robust methods automatically.
}
\item{addMixedFactors
}{
  if factor variables for the mixed variables should be created for the regression models
}
  \item{mi}{
number of multiple imputations.
}
  \item{trace}{
Additional information about the iterations when trace equals TRUE.
}
  \item{init.method}{
  Method for initialization of missing values (kNN or median)
}
}
\details{
The method works sequentially and iterative. The method can deal with a mixture of continuous, semi-continuous, ordinal and nominal variables including 
outliers. 

A full description of the method will be uploaded soon in form of a package vignette.
}
\value{
the imputed data set.
}
\references{
M. Templ, A. Kowarik, P. Filzmoser (2011)  
Iterative stepwise regression imputation using standard and robust methods. 
\emph{Journal of Computational Statistics and Data Analysis}, Vol. 55, pp. 2793-2806.  
}
\author{
Matthias Templ, Alexander Kowarik
}
\seealso{
\code{\link[mi]{mi}}
}
\examples{
data(sleep)
irmi(sleep)

data(testdata)
imp_testdata1 <- irmi(testdata$wna,mixed=testdata$mixed)

# mixed.constant != 0 (-10)
testdata$wna$m1[testdata$wna$m1==0] <- -10
testdata$wna$m2 <- log(testdata$wna$m2+0.001)
imp_testdata2 <- irmi(testdata$wna,mixed=testdata$mixed,mixed.constant=c(-10,log(0.001)))
imp_testdata2$m2 <- exp(imp_testdata2$m2)-0.001

}
\keyword{ manip }
