# ---------------------------------------
# Author: Daniel Schopfhauser
#         Vienna University of Technology
# ---------------------------------------

regressionImp <- function(formula, family="AUTO", robust=FALSE, data) {
  UseMethod("regressionImp", data)
}

regressionImp.data.frame <- function(formula, family="AUTO", robust=FALSE, data) {
  regressionImp_work(formula, family, robust, data)
}

regressionImp.survey.design <- function(formula, family="AUTO", robust=FALSE, data) {
  data$variables <- regressionImp_work(formula, family, robust, data$variables)
  data$call <- sys.call(-1)
  data
}

regressionImp.default <- function(formula, family="AUTO", robust=FALSE, data) {
  regressionImp_work(formula, family, robust, as.data.frame(data))
}

regressionImp_work <- function(formula, family, robust, data){
  warning("Not yet implemented!")
  #please create a warning if there was nothing to impute
  #the gui checks for it
}
