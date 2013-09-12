\name{hotdeck}
\Rdversion{1.1}
\alias{hotdeck}
\title{
Hot-Deck Imputation
}
\description{
Implementation of the popular Sequential, Random (within a domain) hot-deck algorithm for imputation.
}
\usage{
hotdeck(data, variable = NULL, ord_var = NULL,
    domain_var = NULL, makeNA = NULL, NAcond = NULL, impNA = TRUE,
    donorcond = NULL, imp_var = TRUE, imp_suffix = "imp")
}
\arguments{
  \item{data}{
    data.frame or matrix
  }
  \item{variable}{
    variables where missing values should be imputed
  }
  \item{ord_var}{
    variables for sorting the data set before imputation
  }
  \item{domain_var}{
    variables for building domains and impute within these domains
  }
  \item{makeNA}{
    vector of values, that should be converted to NA
  }
  \item{NAcond}{
    a condition for imputing a NA
  }
  \item{impNA}{
    TRUE/FALSE whether NA should be imputed
  }
  \item{donorcond}{
    condition for the donors e.g. ">5"
  }
  \item{imp_var}{
    TRUE/FALSE if a TRUE/FALSE variables for each imputed variable should be created show the imputation status
  }
  \item{imp_suffix}{
    suffix for the TRUE/FALSE variables showing the imputation status
  }
}
\value{
the imputed data set.
}
\author{
Alexander Kowarik
}
\examples{
data(sleep)
sleepI <- hotdeck(sleep)
sleepI2 <- hotdeck(sleep,ord_var="BodyWgt",domain_var="Pred")
}
\keyword{ manip }
