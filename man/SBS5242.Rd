\docType{data}
\name{SBS5242}
\alias{SBS5242}
\title{Synthetic subset of the Austrian structural business statistics data}
\source{
  \url{http://www.statistik.at}
}
\description{
  Synthetic subset of the Austrian structural business
  statistics (SBS) data, namely NACE code 52.42 (retail
  sale of clothing).
}
\details{
  The Austrian SBS data set consists of more than 320.000
  enterprises. Available raw (unedited) data set: 21669
  observations in 90 variables, structured according NACE
  revision 1.1 with 3891 missing values.

  We investigate 9 variables of NACE 52.42 (retail sale of
  clothing).

  From these confidential raw data set a non-confidential,
  close-to-reality, synthetic data set was generated.
}
\examples{
data(SBS5242)
aggr(SBS5242)
}
\keyword{datasets}

