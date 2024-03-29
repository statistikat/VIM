% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgboostImpute.R
\name{xgboostImpute}
\alias{xgboostImpute}
\title{Xgboost Imputation}
\usage{
xgboostImpute(
  formula,
  data,
  imp_var = TRUE,
  imp_suffix = "imp",
  verbose = FALSE,
  nrounds = 100,
  objective = NULL,
  ...
)
}
\arguments{
\item{formula}{model formula for the imputation}

\item{data}{A \code{data.frame} containing the data}

\item{imp_var}{\code{TRUE}/\code{FALSE} if a \code{TRUE}/\code{FALSE} variables for each imputed
variable should be created show the imputation status}

\item{imp_suffix}{suffix used for TF imputation variables}

\item{verbose}{Show the number of observations used for training
and evaluating the RF-Model. This parameter is also passed down to
\code{\link[xgboost:xgb.train]{xgboost::xgboost()}} to show computation status.}

\item{nrounds}{max number of boosting iterations,
argument passed to \code{\link[xgboost:xgb.train]{xgboost::xgboost()}}}

\item{objective}{objective for xgboost,
argument passed to \code{\link[xgboost:xgb.train]{xgboost::xgboost()}}}

\item{...}{Arguments passed to \code{\link[xgboost:xgb.train]{xgboost::xgboost()}}}
}
\value{
the imputed data set.
}
\description{
Impute missing values based on a random forest model using \code{\link[xgboost:xgb.train]{xgboost::xgboost()}}
}
\examples{
data(sleep)
xgboostImpute(Dream~BodyWgt+BrainWgt,data=sleep)
xgboostImpute(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
xgboostImpute(Dream+NonD+Gest~BodyWgt+BrainWgt,data=sleep)

sleepx <- sleep
sleepx$Pred <- as.factor(LETTERS[sleepx$Pred])
sleepx$Pred[1] <- NA
xgboostImpute(Pred~BodyWgt+BrainWgt,data=sleepx)
}
\seealso{
Other imputation methods: 
\code{\link{hotdeck}()},
\code{\link{impPCA}()},
\code{\link{irmi}()},
\code{\link{kNN}()},
\code{\link{matchImpute}()},
\code{\link{medianSamp}()},
\code{\link{rangerImpute}()},
\code{\link{regressionImp}()},
\code{\link{sampleCat}()}
}
\concept{imputation methods}
