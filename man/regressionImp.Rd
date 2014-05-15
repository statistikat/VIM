\name{regressionImp}
\alias{regressionImp}
\title{Regression Imputation}
\usage{
regressionImp(formula, data, family = "AUTO", robust = FALSE,
  imp_var = TRUE, imp_suffix = "imp", mod_cat = FALSE)
}
\arguments{
  \item{formula}{model formula to impute one variable}

  \item{data}{A data.frame or survey object containing the
  data}

  \item{family}{family argument for "glm" ("AUTO" tries to
  choose automatically, only really tested option!!!)}

  \item{robust}{TRUE/FALSE if robust regression should be
  used}

  \item{mod_cat}{TRUE/FALSE if TRUE for categorical
  variables the level with the highest prediction
  probability is selected, otherwise it is sampled
  according to the probabilities.}

  \item{imp_var}{TRUE/FALSE if a TRUE/FALSE variables for
  each imputed variable should be created show the
  imputation status}

  \item{imp_suffix}{suffix used for TF imputation
  variables}
}
\value{
the imputed data set.
}
\description{
Impute missing values based on a regression model.
}
\details{
"lm" is used for family "normal" and glm for all other
families. (Robust=TRUE: lmrob, glmrob)
}
\examples{
data(sleep)
sleepImp1 <- regressionImp(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
sleepImp2 <- regressionImp(Sleep+Gest+Span+Dream+NonD~BodyWgt+BrainWgt,data=sleep)

data(testdata)
imp_testdata1 <- regressionImp(b1+b2~x1+x2,data=testdata$wna)
imp_testdata3 <- regressionImp(x1~x2,data=testdata$wna,robust=TRUE)
}
\author{
Alexander Kowarik
}
\keyword{manip}

