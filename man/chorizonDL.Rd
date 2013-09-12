\name{chorizonDL}
\alias{chorizonDL}
\docType{data}
\title{C-horizon of the Kola data with missing values}
\description{
  This data set is the same as the \code{\link[mvoutlier]{chorizon}} 
  data set in package \code{mvoutlier}, except that values below the 
  detection limit are coded as \code{NA}.
}
\usage{
data(chorizonDL)
}
\format{
  A data frame with 606 observations on the following 110 variables.
  \describe{
    \item{\code{*ID}}{a numeric vector}
    \item{\code{XCOO}}{a numeric vector}
    \item{\code{YCOO}}{a numeric vector}
    \item{\code{Ag}}{a numeric vector}
    \item{\code{Ag_INAA}}{a numeric vector}
    \item{\code{Al}}{a numeric vector}
    \item{\code{Al2O3}}{a numeric vector}
    \item{\code{As}}{a numeric vector}
    \item{\code{As_INAA}}{a numeric vector}
    \item{\code{Au_INAA}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{Ba}}{a numeric vector}
    \item{\code{Ba_INAA}}{a numeric vector}
    \item{\code{Be}}{a numeric vector}
    \item{\code{Bi}}{a numeric vector}
    \item{\code{Br_IC}}{a numeric vector}
    \item{\code{Br_INAA}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{Ca_INAA}}{a numeric vector}
    \item{\code{CaO}}{a numeric vector}
    \item{\code{Cd}}{a numeric vector}
    \item{\code{Ce_INAA}}{a numeric vector}
    \item{\code{Cl_IC}}{a numeric vector}
    \item{\code{Co}}{a numeric vector}
    \item{\code{Co_INAA}}{a numeric vector}
    \item{\code{EC}}{a numeric vector}
    \item{\code{Cr}}{a numeric vector}
    \item{\code{Cr_INAA}}{a numeric vector}
    \item{\code{Cs_INAA}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
    \item{\code{Eu_INAA}}{a numeric vector}
    \item{\code{F_IC}}{a numeric vector}
    \item{\code{Fe}}{a numeric vector}
    \item{\code{Fe_INAA}}{a numeric vector}
    \item{\code{Fe2O3}}{a numeric vector}
    \item{\code{Hf_INAA}}{a numeric vector}
    \item{\code{Hg}}{a numeric vector}
    \item{\code{Hg_INAA}}{a numeric vector}
    \item{\code{Ir_INAA}}{a numeric vector}
    \item{\code{K}}{a numeric vector}
    \item{\code{K2O}}{a numeric vector}
    \item{\code{La}}{a numeric vector}
    \item{\code{La_INAA}}{a numeric vector}
    \item{\code{Li}}{a numeric vector}
    \item{\code{LOI}}{a numeric vector}
    \item{\code{Lu_INAA}}{a numeric vector}
    \item{\code{wt_INAA}}{a numeric vector}
    \item{\code{Mg}}{a numeric vector}
    \item{\code{MgO}}{a numeric vector}
    \item{\code{Mn}}{a numeric vector}
    \item{\code{MnO}}{a numeric vector}
    \item{\code{Mo}}{a numeric vector}
    \item{\code{Mo_INAA}}{a numeric vector}
    \item{\code{Na}}{a numeric vector}
    \item{\code{Na_INAA}}{a numeric vector}
    \item{\code{Na2O}}{a numeric vector}
    \item{\code{Nd_INAA}}{a numeric vector}
    \item{\code{Ni}}{a numeric vector}
    \item{\code{Ni_INAA}}{a numeric vector}
    \item{\code{NO3_IC}}{a numeric vector}
    \item{\code{P}}{a numeric vector}
    \item{\code{P2O5}}{a numeric vector}
    \item{\code{Pb}}{a numeric vector}
    \item{\code{pH}}{a numeric vector}
    \item{\code{PO4_IC}}{a numeric vector}
    \item{\code{Rb}}{a numeric vector}
    \item{\code{S}}{a numeric vector}
    \item{\code{Sb}}{a numeric vector}
    \item{\code{Sb_INAA}}{a numeric vector}
    \item{\code{Sc}}{a numeric vector}
    \item{\code{Sc_INAA}}{a numeric vector}
    \item{\code{Se}}{a numeric vector}
    \item{\code{Se_INAA}}{a numeric vector}
    \item{\code{Si}}{a numeric vector}
    \item{\code{SiO2}}{a numeric vector}
    \item{\code{Sm_INAA}}{a numeric vector}
    \item{\code{Sn_INAA}}{a numeric vector}
    \item{\code{SO4_IC}}{a numeric vector}
    \item{\code{Sr}}{a numeric vector}
    \item{\code{Sr_INAA}}{a numeric vector}
    \item{\code{SUM_XRF}}{a numeric vector}
    \item{\code{Ta_INAA}}{a numeric vector}
    \item{\code{Tb_INAA}}{a numeric vector}
    \item{\code{Te}}{a numeric vector}
    \item{\code{Th}}{a numeric vector}
    \item{\code{Th_INAA}}{a numeric vector}
    \item{\code{Ti}}{a numeric vector}
    \item{\code{TiO2}}{a numeric vector}
    \item{\code{U_INAA}}{a numeric vector}
    \item{\code{V}}{a numeric vector}
    \item{\code{W_INAA}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
    \item{\code{Yb_INAA}}{a numeric vector}
    \item{\code{Zn}}{a numeric vector}
    \item{\code{Zn_INAA}}{a numeric vector}
    \item{\code{ELEV}}{a numeric vector}
    \item{\code{*COUN}}{a numeric vector}
    \item{\code{*ASP}}{a numeric vector}
    \item{\code{TOPC}}{a numeric vector}
    \item{\code{LITO}}{a numeric vector}
    \item{\code{Al_XRF}}{a numeric vector}
    \item{\code{Ca_XRF}}{a numeric vector}
    \item{\code{Fe_XRF}}{a numeric vector}
    \item{\code{K_XRF}}{a numeric vector}
    \item{\code{Mg_XRF}}{a numeric vector}
    \item{\code{Mn_XRF}}{a numeric vector}
    \item{\code{Na_XRF}}{a numeric vector}
    \item{\code{P_XRF}}{a numeric vector}
    \item{\code{Si_XRF}}{a numeric vector}
    \item{\code{Ti_XRF}}{a numeric vector}
  }
}
\note{
    For a more detailed description of this data set, see 
    \code{\link[mvoutlier]{chorizon}} in package \code{mvoutlier}.
}
\source{
Kola Project (1993-1998)
}
\references{
Reimann, C., Filzmoser, P., Garrett, R.G. and Dutter, R. (2008) 
\emph{Statistical Data Analysis Explained: Applied Environmental
Statistics with R}. Wiley.
}
\seealso{\code{\link[mvoutlier]{chorizon}}}
\examples{
data(chorizonDL, package = "VIM")
summary(chorizonDL)
}
\keyword{datasets}
