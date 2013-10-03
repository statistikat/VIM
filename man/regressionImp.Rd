\name{regressionImp}
\Rdversion{1.1}
\alias{regressionImp}
\title{
Iterative robust model-based imputation (IRMI) 
}
\description{
In each step of the iteration,
one variable is used as a response variable and the remaining variables
serve as the regressors.
}
\usage{
regressionImp(formula, data,family="AUTO", robust=FALSE,imp_var = TRUE,
    imp_suffix = "imp", mod_cat=FALSE)
}
\arguments{
\item{formula}{ model formula to impute one variable}
\item{data}{A data.frame or survey object containing the data}
\item{family}{family argument for "glm" ("AUTO" tries to choose automatically, only really tested option!!!)}
\item{robust}{TRUE/FALSE if robust regression should be used}
\item{mod_cat}{TRUE/FALSE if TRUE for categorical variables the level with the highest prediction probability
is selected, otherwise it is sampled according to the probabilities.}

}
\details{
  "lm" is used for family "normal" and glm for all other families. (Robust=TRUE: lmrob, glmrob)
}
\value{
the imputed data set.
}
\author{
Alexander Kowarik
}
\seealso{
\code{\link[lm]{lm}}
\code{\link[glm]{glm}}
\code{\link[lmrob]{lmrob}}
\code{\link[glmrob]{glmrob}}
}
\examples{
data(sleep)
sleepImp1 <- regressionImp(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
sleepImp2 <- regressionImp(Sleep+Gest+Span+Dream+NonD~BodyWgt+BrainWgt,data=sleep)
sleepImp3 <- regressionImp(Sleep+Gest+Span+Dream+NonD~BodyWgt+BrainWgt,data=sleep,robust=TRUE)

data(testdata)
imp_testdata1 <- regressionImp(c1+c2~x1+x2,data=testdata$wna)
imp_testdata2 <- regressionImp(x1+b1+c1+c2~x2,data=testdata$wna)
imp_testdata3 <- regressionImp(x1+b1+c1+c2~x2,data=testdata$wna,robust=TRUE)
}
\keyword{ manip }
