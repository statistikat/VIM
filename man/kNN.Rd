\name{kNN}
\alias{gowerD}
\alias{kNN}
\alias{maxCat}
\alias{sampleCat}
\alias{which.minN}
\title{k-Nearest Neighbour Imputation}
\usage{
  kNN(data, variable = colnames(data), metric = NULL,
    k = 5, dist_var = colnames(data), weights = NULL,
    numFun = median, catFun = maxCat, makeNA = NULL,
    NAcond = NULL, impNA = TRUE, donorcond = NULL,
    mixed = vector(), mixed.constant = NULL, trace = FALSE,
    imp_var = TRUE, imp_suffix = "imp", addRandom = FALSE)
}
\arguments{
  \item{data}{data.frame or matrix}

  \item{variable}{variables where missing values should be
  imputed}

  \item{metric}{metric to be used for calculating the
  distances between}

  \item{k}{number of Nearest Neighbours used}

  \item{dist_var}{names or variables to be used for
  distance calculation}

  \item{weights}{weights for the variables for distance
  calculation}

  \item{numFun}{function for aggregating the k Nearest
  Neighbours in the case of a numerical variable}

  \item{catFun}{function for aggregating the k Nearest
  Neighbours in the case of a categorical variable}

  \item{makeNA}{vector of values, that should be converted
  to NA}

  \item{NAcond}{a condition for imputing a NA}

  \item{impNA}{TRUE/FALSE whether NA should be imputed}

  \item{donorcond}{condition for the donors e.g. ">5"}

  \item{trace}{TRUE/FALSE if additional information about
  the imputation process should be printed}

  \item{imp_var}{TRUE/FALSE if a TRUE/FALSE variables for
  each imputed variable should be created show the
  imputation status}

  \item{imp_suffix}{suffix for the TRUE/FALSE variables
  showing the imputation status}

  \item{addRandom}{TRUE/FALSE if an additional random
  variable should be added for distance calculation}

  \item{mixed}{names of mixed variables}

  \item{mixed.constant}{vector with length equal to the
  number of semi-continuous variables specifying the point
  of the semi-continuous distribution with non-zero
  probability}
}
\value{
  the imputed data set.
}
\description{
  k-Nearest Neighbour Imputation based on a variation of
  the Gower Distance for numerical, categorical, ordered
  and semi-continous variables.
}
\details{
  The function sampleCat samples with probabilites
  corresponding to the occurrence of the level in the NNs.
  The function maxCat chooses the level with the most
  occurrences and random if the maximum is not unique. The
  function gowerD is used by kNN to compute the distances
  for numerical, factor ordered and semi-continous
  variables. The function which.minN is used by kNN.
}
\examples{
data(sleep)
kNN(sleep)
}
\author{
  Alexander Kowarik
}
\keyword{manip}

