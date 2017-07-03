#' @import data.table
#' @import grDevices
#' @import Rcpp
#' @import sp
#' @import stats
#' @import methods
#' @import MASS
#' @import nnet
#' @import e1071
#' @import grid
#' @import robustbase
#' @import colorspace
#' @importFrom car bcPower
#' @importFrom car powerTransform
#' @importFrom vcd mosaic
#' @importFrom vcd labeling_border
#' @importFrom laeken weightedMedian
#' @importFrom laeken weightedMean 
#' @importFrom graphics Axis abline axTicks axis barplot box hist boxplot layout lcm lines locator par plot.new plot.window points
#' @importFrom graphics polygon rect strheight strwidth text title
#' @importFrom utils capture.output flush.console head
#' @useDynLib VIM
NULL
#' C-horizon of the Kola data with missing values
#' 
#' This data set is the same as the \code{\link[mvoutlier]{chorizon}} data set
#' in package \code{mvoutlier}, except that values below the detection limit
#' are coded as \code{NA}.
#' 
#' 
#' @name chorizonDL
#' @docType data
#' @format A data frame with 606 observations on the following 110 variables.
#' \describe{ \item{*ID}{a numeric vector} \item{XCOO}{a
#' numeric vector} \item{YCOO}{a numeric vector} \item{Ag}{a
#' numeric vector} \item{Ag_INAA}{a numeric vector} \item{Al}{a
#' numeric vector} \item{Al2O3}{a numeric vector} \item{As}{a
#' numeric vector} \item{As_INAA}{a numeric vector}
#' \item{Au_INAA}{a numeric vector} \item{B}{a numeric vector}
#' \item{Ba}{a numeric vector} \item{Ba_INAA}{a numeric vector}
#' \item{Be}{a numeric vector} \item{Bi}{a numeric vector}
#' \item{Br_IC}{a numeric vector} \item{Br_INAA}{a numeric
#' vector} \item{Ca}{a numeric vector} \item{Ca_INAA}{a numeric
#' vector} \item{CaO}{a numeric vector} \item{Cd}{a numeric
#' vector} \item{Ce_INAA}{a numeric vector} \item{Cl_IC}{a
#' numeric vector} \item{Co}{a numeric vector} \item{Co_INAA}{a
#' numeric vector} \item{EC}{a numeric vector} \item{Cr}{a
#' numeric vector} \item{Cr_INAA}{a numeric vector}
#' \item{Cs_INAA}{a numeric vector} \item{Cu}{a numeric vector}
#' \item{Eu_INAA}{a numeric vector} \item{F_IC}{a numeric
#' vector} \item{Fe}{a numeric vector} \item{Fe_INAA}{a numeric
#' vector} \item{Fe2O3}{a numeric vector} \item{Hf_INAA}{a
#' numeric vector} \item{Hg}{a numeric vector} \item{Hg_INAA}{a
#' numeric vector} \item{Ir_INAA}{a numeric vector} \item{K}{a
#' numeric vector} \item{K2O}{a numeric vector} \item{La}{a
#' numeric vector} \item{La_INAA}{a numeric vector} \item{Li}{a
#' numeric vector} \item{LOI}{a numeric vector}
#' \item{Lu_INAA}{a numeric vector} \item{wt_INAA}{a numeric
#' vector} \item{Mg}{a numeric vector} \item{MgO}{a numeric
#' vector} \item{Mn}{a numeric vector} \item{MnO}{a numeric
#' vector} \item{Mo}{a numeric vector} \item{Mo_INAA}{a numeric
#' vector} \item{Na}{a numeric vector} \item{Na_INAA}{a numeric
#' vector} \item{Na2O}{a numeric vector} \item{Nd_INAA}{a
#' numeric vector} \item{Ni}{a numeric vector} \item{Ni_INAA}{a
#' numeric vector} \item{NO3_IC}{a numeric vector} \item{P}{a
#' numeric vector} \item{P2O5}{a numeric vector} \item{Pb}{a
#' numeric vector} \item{pH}{a numeric vector} \item{PO4_IC}{a
#' numeric vector} \item{Rb}{a numeric vector} \item{S}{a
#' numeric vector} \item{Sb}{a numeric vector} \item{Sb_INAA}{a
#' numeric vector} \item{Sc}{a numeric vector} \item{Sc_INAA}{a
#' numeric vector} \item{Se}{a numeric vector} \item{Se_INAA}{a
#' numeric vector} \item{Si}{a numeric vector} \item{SiO2}{a
#' numeric vector} \item{Sm_INAA}{a numeric vector}
#' \item{Sn_INAA}{a numeric vector} \item{SO4_IC}{a numeric
#' vector} \item{Sr}{a numeric vector} \item{Sr_INAA}{a numeric
#' vector} \item{SUM_XRF}{a numeric vector} \item{Ta_INAA}{a
#' numeric vector} \item{Tb_INAA}{a numeric vector} \item{Te}{a
#' numeric vector} \item{Th}{a numeric vector} \item{Th_INAA}{a
#' numeric vector} \item{Ti}{a numeric vector} \item{TiO2}{a
#' numeric vector} \item{U_INAA}{a numeric vector} \item{V}{a
#' numeric vector} \item{W_INAA}{a numeric vector} \item{Y}{a
#' numeric vector} \item{Yb_INAA}{a numeric vector} \item{Zn}{a
#' numeric vector} \item{Zn_INAA}{a numeric vector}
#' \item{ELEV}{a numeric vector} \item{*COUN}{a numeric vector}
#' \item{*ASP}{a numeric vector} \item{TOPC}{a numeric vector}
#' \item{LITO}{a numeric vector} \item{Al_XRF}{a numeric
#' vector} \item{Ca_XRF}{a numeric vector} \item{Fe_XRF}{a
#' numeric vector} \item{K_XRF}{a numeric vector}
#' \item{Mg_XRF}{a numeric vector} \item{Mn_XRF}{a numeric
#' vector} \item{Na_XRF}{a numeric vector} \item{P_XRF}{a
#' numeric vector} \item{Si_XRF}{a numeric vector}
#' \item{Ti_XRF}{a numeric vector} }
#' @note For a more detailed description of this data set, see
#' \code{\link[mvoutlier]{chorizon}} in package \code{mvoutlier}.
#' @seealso \code{\link[mvoutlier]{chorizon}}
#' @references Reimann, C., Filzmoser, P., Garrett, R.G. and Dutter, R. (2008)
#' \emph{Statistical Data Analysis Explained: Applied Environmental Statistics
#' with R}. Wiley.
#' @source Kola Project (1993-1998)
#' @keywords datasets
#' @examples
#' 
#' data(chorizonDL, package = "VIM")
#' summary(chorizonDL)
#' 
NULL





#' Background map for the Kola project data
#' 
#' Coordinates of the Kola background map.
#' 
#' 
#' @name kola.background
#' @docType data
#' @references Reimann, C., Filzmoser, P., Garrett, R.G. and Dutter, R. (2008)
#' \emph{Statistical Data Analysis Explained: Applied Environmental Statistics
#' with R}. Wiley, 2008.
#' @source Kola Project (1993-1998)
#' @keywords datasets
#' @examples
#' 
#' data(kola.background, package = "VIM")
#' bgmap(kola.background)
#' 
NULL





#' Synthetic subset of the Austrian structural business statistics data
#' 
#' Synthetic subset of the Austrian structural business statistics (SBS) data,
#' namely NACE code 52.42 (retail sale of clothing).
#' 
#' The Austrian SBS data set consists of more than 320.000 enterprises.
#' Available raw (unedited) data set: 21669 observations in 90 variables,
#' structured according NACE revision 1.1 with 3891 missing values.
#' 
#' We investigate 9 variables of NACE 52.42 (retail sale of clothing).
#' 
#' From these confidential raw data set a non-confidential, close-to-reality,
#' synthetic data set was generated.
#' 
#' @name SBS5242
#' @docType data
#' @source \url{http://www.statistik.at}
#' @keywords datasets
#' @examples
#' 
#' data(SBS5242)
#' aggr(SBS5242)
#' 
NULL





#' Mammal sleep data
#' 
#' Sleep data with missing values.
#' 
#' 
#' @name sleep
#' @docType data
#' @format A data frame with 62 observations on the following 10 variables.
#' \describe{ \item{BodyWgt}{a numeric vector}
#' \item{BrainWgt}{a numeric vector} \item{NonD}{a numeric
#' vector} \item{Dream}{a numeric vector} \item{Sleep}{a
#' numeric vector} \item{Span}{a numeric vector} \item{Gest}{a
#' numeric vector} \item{Pred}{a numeric vector} \item{Exp}{a
#' numeric vector} \item{Danger}{a numeric vector} }
#' @source Allison, T. and Chichetti, D. (1976) Sleep in mammals: ecological
#' and constitutional correlates. \emph{Science} \bold{194 (4266)}, 732--734.
#' 
#' The data set was imported from \code{GGobi}.
#' @keywords datasets
#' @examples
#' 
#' data(sleep, package = "VIM")
#' summary(sleep)
#' aggr(sleep)
#' 
NULL





#' Tropical Atmosphere Ocean (TAO) project data
#' 
#' A small subsample of the Tropical Atmosphere Ocean (TAO) project data,
#' derived from the \code{GGOBI} project.
#' 
#' All cases recorded for five locations and two time periods.
#' 
#' @name tao
#' @docType data
#' @format A data frame with 736 observations on the following 8 variables.
#' \describe{ \item{Year}{a numeric vector} \item{Latitude}{a
#' numeric vector} \item{Longitude}{a numeric vector}
#' \item{Sea.Surface.Temp}{a numeric vector} \item{Air.Temp}{a
#' numeric vector} \item{Humidity}{a numeric vector}
#' \item{UWind}{a numeric vector} \item{VWind}{a numeric
#' vector} }
#' @source \url{http://www.pmel.noaa.gov/tao/}
#' @keywords datasets
#' @examples
#' 
#' data(tao, package = "VIM")
#' summary(tao)
#' aggr(tao)
#' 
NULL





#' Simulated data set for testing purpose
#' 
#' 2 numeric, 2 binary, 2 nominal and 2 mixed (semi-continous) variables
#' 
#' 
#' @name testdata
#' @docType data
#' @format The format is: List of 4 $ wna :'data.frame': 500 obs. of 8
#' variables: ..$ x1: num [1:500] 10.87 9.53 7.83 8.53 8.67 ...  ..$ x2: num
#' [1:500] 10.9 9.32 7.68 8.2 8.41 ...  ..$ c1: Factor w/ 4 levels
#' "a","b","c","d": 3 2 2 1 2 2 1 3 3 2 ...  ..$ c2: Factor w/ 4 levels
#' "a","b","c","d": 2 3 2 2 2 2 2 4 2 2 ...  ..$ b1: Factor w/ 2 levels
#' "0","1": 2 2 1 2 1 2 1 2 1 1 ...  ..$ b2: Factor w/ 2 levels "0","1": 2 2 1
#' 1 1 1 1 2 2 2 ...  ..$ m1: num [1:500] 0 8.29 9.08 0 0 ...  ..$ m2: num
#' [1:500] 10.66 9.39 7.8 8.11 7.33 ...  $ wona :'data.frame': 500 obs. of 8
#' variables: ..$ x1: num [1:500] 10.87 9.53 7.83 8.53 8.67 ...  ..$ x2: num
#' [1:500] 10.9 9.32 7.68 8.2 8.41 ...  ..$ c1: Factor w/ 4 levels
#' "a","b","c","d": 3 2 2 1 2 2 1 3 3 2 ...  ..$ c2: Factor w/ 4 levels
#' "a","b","c","d": 2 3 2 2 2 2 2 4 2 2 ...  ..$ b1: Factor w/ 2 levels
#' "0","1": 2 2 1 2 1 2 1 2 1 1 ...  ..$ b2: Factor w/ 2 levels "0","1": 2 2 1
#' 1 1 1 1 2 2 2 ...  ..$ m1: num [1:500] 0 8.29 9.08 0 0 ...  ..$ m2: num
#' [1:500] 10.66 9.39 7.8 8.11 7.33 ...  $ mixed : chr [1:2] "m1" "m2" $
#' outlierInd: NULL
#' @keywords datasets
#' @examples
#' 
#' data(testdata)
#' 
NULL





#' Visualization and Imputation of Missing Values
#' 
#' This package introduces new tools for the visualization of missing or
#' imputed values in , which can be used for exploring the data and the
#' structure of the missing or imputed values. Depending on this structure,
#' they may help to identify the mechanism generating the missing values or
#' errors, which may have happened in the imputation process. This knowledge is
#' necessary for selecting an appropriate imputation method in order to
#' reliably estimate the missing values. Thus the visualization tools should be
#' applied before imputation and the diagnostic tools afterwards.
#' 
#' Detecting missing values mechanisms is usually done by statistical tests or
#' models.  Visualization of missing and imputed values can support the test
#' decision, but also reveals more details about the data structure. Most
#' notably, statistical requirements for a test can be checked graphically, and
#' problems like outliers or skewed data distributions can be discovered.
#' Furthermore, the included plot methods may also be able to detect missing
#' values mechanisms in the first place.
#' 
#' A graphical user interface available in the package VIMGUI allows an easy
#' handling of the plot methods.  In addition, \code{VIM} can be used for data
#' from essentially any field.
#' 
#' \tabular{ll}{ Package: \tab VIM\cr Version: \tab 3.0.3\cr Date: \tab
#' 2013-01-09\cr Depends: \tab R (>= 2.10),e1071,car, colorspace, nnet,
#' robustbase, tcltk, tkrplot, sp, vcd, Rcpp\cr Imports: \tab car, colorspace,
#' grDevices, robustbase, stats, tcltk, sp, utils, vcd\cr License: \tab GPL (>=
#' 2)\cr URL: \tab http://cran.r-project.org/package=VIM\cr }
#' 
#' @name VIM-package
#' @aliases VIM-package VIM
#' @docType package
#' @author Matthias Templ, Andreas Alfons, Alexander Kowarik, Bernd Prantner
#' 
#' Maintainer: Matthias Templ <templ@@tuwien.ac.at>
#' @references M. Templ, A. Alfons, P. Filzmoser (2012) Exploring incomplete
#' data using visualization tools.  \emph{Journal of Advances in Data Analysis
#' and Classification}, Online first. DOI: 10.1007/s11634-011-0102-y.
#' 
#' M. Templ, A. Kowarik, P. Filzmoser (2011) Iterative stepwise regression
#' imputation using standard and robust methods.  \emph{Journal of
#' Computational Statistics and Data Analysis}, Vol. 55, pp. 2793-2806.
#' @keywords package
NULL



setGeneric("plot")
setGeneric("print")