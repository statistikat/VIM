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
#' @importFrom ranger ranger importance
#' @useDynLib VIM
NULL


#' Breast cancer Wisconsin data set
#' 
#' Dataset containing the original Wisconsin breast cancer data.
#' 
#' 
#' @name bcancer
#' @docType data
#' @references  The data downloaded and conditioned for R from the UCI machine learning repository, 
#' see https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)
#' This breast cancer databases was obtained from the University of Wisconsin Hospitals, 
#' Madison from Dr. William H. Wolberg. If you publish results when using this database, 
#' then please include this information in your acknowledgements. 
#' Also, please cite one or more of: 
#' O. L. Mangasarian and W. H. Wolberg: "Cancer diagnosis via linear programming", 
#' SIAM News, Volume 23, Number 5, September 1990, pp 1 & 18. 
#' William H. Wolberg and O.L. Mangasarian: 
#' "Multisurface method of pattern separation for medical diagnosis applied to breast cytology", 
#' Proceedings of the National Academy of Sciences, U.S.A., Volume 87, December 1990, pp 9193-9196. 
#' O. L. Mangasarian, R. Setiono, and W.H. Wolberg: 
#' "Pattern recognition via linear programming: Theory and application to medical diagnosis", 
#' in: "Large-scale numerical optimization", Thomas F. Coleman and Yuying Li, editors, 
#' SIAM Publications, Philadelphia 1990, pp 22-30. 
#' K. P. Bennett & O. L. Mangasarian: 
#' "Robust linear programming discrimination of two linearly inseparable sets", 
#' Optimization Methods and Software 1, 1992, 23-34 (Gordon & Breach Science Publishers).
#' @keywords datasets
#' @format A data frame with 699 observations on the following 11 variables.
#' \describe{ 
#' \item{ID}{Sample ID} 
#' \item{clump_thickness}{as integer from 1 - 10} 
#' \item{uniformity_cellsize}{as integer from 1 - 10}
#' \item{uniformity_cellshape}{as integer from 1 - 10}
#' \item{adhesion}{as integer from 1 - 10}
#' \item{epithelial_cellsize}{as integer from 1 - 10}
#' \item{bare_nuclei}{as integer from 1 - 10, includes 16 missings}
#' \item{chromatin}{as integer from 1 - 10}
#' \item{normal_nucleoli}{as integer from 1 - 10}
#' \item{mitoses}{as integer from 1 - 10}
#' \item{class}{benign or malignant}
#' }
#' @examples
#' 
#' data(bcancer)
#' aggr(bcancer)
#' 
NULL



#' Brittleness index data set 
#' 
#' @description A plastic product is produced in three parallel reactors (TK104, TK105, or TK107). 
#' For each row in the dataset, we have the same batch of raw material that was split, and fed to the 3 reactors. 
#' These values are the brittleness index for the product produced in the reactor. A simulated data set.
#' 
#' @name brittleness
#' @docType data
#' @source \url{https://openmv.net/info/brittleness-index}
#' @keywords datasets
#' @format A data frame with 23 observations on the following 3 variables.
#' \describe{ 
#' \item{TK104}{Brittleness for batches of raw material in reactor 104} 
#' \item{TK105}{Brittleness for batches of raw material in reactor 105} 
#' \item{TK107}{Brittleness for batches of raw material in reactor 107}
#' }
#' @examples
#' 
#' data(brittleness)
#' aggr(brittleness)
#' 
NULL

#' Colic horse data set 
#' 
#' @description This is a modified version of the original training data set 
#' taken from the UCI repository, see reference. 
#' The modifications are only related to having appropriate levels for factor variables.   
#' This data set is about horse diseases where the task is to determine, 
#' if the lesion of the horse was surgical or not. 
#' 
#' @name colic
#' @docType data
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Horse+Colic}
#' Creators: Mary McLeish & Matt Cecile, Department of Computer Science, University of Guelph,
#' Guelph, Ontario, Canada N1G 2W1 
#' Donor: Will Taylor
#' @keywords datasets
#' @format A training data frame with 300 observations on the following 31 variables.
#' \describe{ 
#' \item{surgery}{yes or no} 
#' \item{age}{1 equals an adult horse, 2 is a horse younger than 6 months} 
#' \item{hospitalID}{ID}
#' \item{temp_rectal}{rectal temperature}
#' \item{pulse}{heart rate in beats per minute}
#' \item{respiratory_rate}{a normal rate is between 8 and 10}
#' \item{temp_extreme}{temperature of extremities}
#' \item{pulse_peripheral}{factor with four categories}
#' \item{capillayr_refill_time}{a clinical judgement. The longer the refill, the poorer the circulation. Possible values are
#' 1 = < 3 seconds and 2 = >= 3 seconds}
#' \item{pain}{a subjective judgement of the horse's pain level }
#' \item{peristalsis}{an indication of the activity in the horse's gut. 
#' As the gut becomes more distended or the horse becomes more toxic, the activity decreases }
#' \item{abdominal_distension}{An animal with abdominal distension is likely to be painful and have reduced gut motility. 
#' A horse with severe abdominal distension is likely to require surgery just tio relieve the pressure }
#' \item{nasogastric_tube}{This refers to any gas coming out of the tube. 
#' A large gas cap in the stomach is likely to give the horse discomfort }
#' \item{nasogastric_reflux}{posible values are 1 = none, 2 = > 1 liter, 3 = < 1 liter. 
#' The greater amount of reflux, the more likelihood that there is some 
#' serious obstruction to the fluid passage from the rest of the intestine }
#' \item{nasogastric_reflux_PH}{scale is from 0 to 14 with 7 being neutral. 
#' Normal values are in the 3 to 4 range }
#' \item{rectal_examination}{Rectal examination. Absent feces probably indicates an obstruction }
#' \item{abdomen }{abdomen. possible values 1 = normal, 2 = other, 3 = firm feces in the large intestine, 
#' 4 = distended small intestine, 5 = distended large intestine }
#' \item{cell_volume }{packed cell volume. normal range is 30 to 50. 
#' The level rises as the circulation becomes compromised or as the animal becomes dehydrated. }
#' \item{protein}{total protein. Normal values lie in the 6-7.5 (gms/dL) range. The higher the value the greater the dehydration }
#' \item{abdominocentesis_appearance}{Abdominocentesis appearance. 
#' A needle is put in the horse's abdomen and fluid is obtained from the abdominal cavity }
#' \item{abdomcentesis_protein}{abdomcentesis total protein. 
#' The higher the level of protein the more likely it is to have a compromised gut. Values are in gms/dL }
#' \item{outcome}{What eventually happened to the horse? }
#' \item{surgical_lesion}{retrospectively, was the problem (lesion) surgical?}
#' \item{lesion_type1}{type of lesion }
#' \item{lesion_type2}{type of lesion }
#' \item{lesion_type3}{type of lesion }
#' \item{cp_data}{}
#' \item{temp_extreme_ordered}{temperature of extremities (ordered)}
#' \item{mucous_membranes_col}{mucous membranes. A subjective measurement of colour }
#' \item{mucous_membranes_group}{different recodings of mucous membrances}
#' }
#' @examples
#' 
#' data(colic)
#' aggr(colic)
#' 
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

#' Subset of the collision data 
#' 
#' Subset of the collision data from December 20. to December 31. 2018 from NYCD.
#' 
#' Each record represents a collision in NYC by city, borough, precinct and cross street. 
#' 
#' 
#' @name collisions
#' @docType data
#' @source \url{https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95}
#' @keywords datasets
#' @examples
#' 
#' data(collisions)
#' aggr(collisions)
#' 
NULL


#' Indian Prime Diabetes Data
#' 
#' The datasets consists of several medical predictor variables and 
#' one target variable, Outcome. Predictor variables includes the number of pregnancies 
#' the patient has had, their BMI, insulin level, age, and so on.
#' 
#' This dataset is originally from the National Institute of Diabetes and 
#' Digestive and Kidney Diseases. The objective of the dataset is to 
#' diagnostically predict whether or not a patient has diabetes, based 
#' on certain diagnostic measurements included in the dataset. 
#' Several constraints were placed on the selection of these instances 
#' from a larger database. In particular, all patients here are females 
#' at least 21 years old of Pima Indian heritage.
#' 
#' 
#' @name diabetes
#' @docType data
#' @source \url{https://www.kaggle.com/uciml/pima-indians-diabetes-database/data}
#' @references Smith, J.W., Everhart, J.E., Dickson, W.C., Knowler, W.C., & Johannes, R.S. (1988). 
#' Using the ADAP learning algorithm to forecast the onset of diabetes mellitus. 
#' In Proceedings of the Symposium on Computer Applications and Medical Care (pp. 261--265). IEEE Computer Society Press.
#' @keywords datasets
#' @format A data frame with 768 observations on the following 9 variables.
#' \describe{ 
#' \item{Pregnancies}{Number of times pregnant} 
#' \item{Glucose}{Plasma glucose concentration a 2 hours in an oral glucose tolerance test} 
#' \item{BloodPressure}{Diastolic blood pressure (mm Hg)}
#' \item{SkinThickness}{Triceps skin fold thickness (mm)}
#' \item{Insulin}{2-Hour serum insulin (mu U/ml)}
#' \item{BMI}{Body mass index (weight in kg/(height in m)^2)}
#' \item{DiabetesPedigreeFunction}{Diabetes pedigree function}
#' \item{Age}{Age in years}
#' \item{Outcome}{Diabetes (yes or no)}
#' }
#' @examples
#' 
#' data(diabetes)
#' aggr(diabetes)
#' 
NULL


#' Food consumption
#' 
#' The relative consumption of certain food items in European and Scandinavian countries.
#' 
#' The numbers represent the percentage of the population consuming that food type.
#' 
#' @name food
#' @docType data
#' @source \url{https://openmv.net/info/food-consumption}
#' @keywords datasets
#' @format A data frame with 16 observations on the following 21 variables.
#' @examples
#' 
#' data(food)
#' str(food)
#' aggr(food)
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

#' Pulp lignin content
#' 
#' Pulp quality by lignin content remaining
#' 
#' Pulp quality is measured by the lignin content remaining in the pulp: 
#' the Kappa number. This data set is used to understand which variables 
#' in the process influence the Kappa number, and if it can be predicted 
#' accurately enough for an inferential sensor application.
#' Variables with a number at the end have been lagged by that 
#' number of hours to line up the data.
#' 
#' @name pulplignin
#' @docType data
#' @source \url{https://openmv.net/info/kamyr-digester}
#' @references K. Walkush and R.R. Gustafson. Application of feedforward neural networks and partial least 
#' squares regression for modelling Kappa number in a continuous Kamyr digester", 
#' Pulp and Paper Canada, 95, 1994, p T7-T13.
#' @keywords datasets
#' @format A data frame with 301 observations on the following 23 variables.
#' @examples
#' 
#' data(pulplignin)
#' str(pulplignin)
#' aggr(pulplignin)
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


#' Simulated toy data set for examples
#' 
#' A 2-dimensional data set with additional information.
#' 
#' 
#' @name toydataMiss
#' @docType data
#' @format data frame with 100 observations and 12 variables. The first two 
#' variables represent the fully observed data.
#' @keywords datasets
#' @examples
#' 
#' data(toydataMiss)
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


#' Wine tasting and price
#' 
#' Wine reviews from France, Switzerland, Austria and Germany.
#' 
#' The data was scraped from WineEnthusiast during the week of Nov 22th, 2017. 
#' The code for the scraper can be found at https://github.com/zackthoutt/wine-deep-learning 
#' This data set is slightly modified, i.e. only four countries are selected and 
#' broader categories on the variety have been added. 
#' 
#' @name wine
#' @docType data
#' @format A data frame with 9627 observations on the following 9 variables.
#' \describe{ 
#' \item{country}{country of origin} 
#' \item{points}{the number of points WineEnthusiast rated the wine on a scale of 1-100 
#' (though they say they only post reviews for wines that score >=80)} 
#' \item{price}{the cost for a bottle of the wine} 
#' \item{province}{the province or state that the wine is from} 
#' \item{taster_name}{name of the person who tasted and reviewed the wine} 
#' \item{taster_twitter_handle}{Twitter handle for the person who tasted ane reviewed the wine} 
#' \item{variety}{the type of grapes used to make the wine (ie pinot noir)} 
#' \item{winery}{the winery that made the wine} 
#' \item{variety_main}{broader category as variety} 
#' }
#' @source \url{https://www.kaggle.com/zynicide/wine-reviews}
#' @keywords datasets
#' @examples
#' 
#' data(wine)
#' str(wine)
#' aggr(wine)
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