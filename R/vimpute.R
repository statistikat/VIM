# model based imputation framework function
# generally idea function that incorporates a framework for model based imputation. 
# Options should be
# - sequential modelling
# - using PMM or not
# - bootstrap the model error
# - drawing the predicted value from the "posterior" distribution
# - model options: (robust) regression, ranger, XGBoost, some kind of transformer model
# - complex formula (with `formula`) for each variable possible. 


#' VIM Model Based Imputation framework
#' generally idea function that incorporates a framework for model based imputation. 
#' Options should be
#' - sequential modelling
#' - using PMM or not
#' - bootstrap the model error
#' - drawing the predicted value from the "posterior" distribution
#' - model options: (robust) regression, ranger, XGBoost, some kind of transformer model
#' - complex formula (with `formula`) for each variable possible. 
#'
#' @param data 
#' @param variable 
#' @param sequential 
#' @param modeluncertainty 
#' @param imputationuncertainty 
#' @param xvar 
#' @param method 
#' @param formula 
#' @param imp_var 
#' @param imp_suffix 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
vimpute <- function(data,
                    variable = colnames(data),
                    sequential = TRUE,
                    modeluncertainty = c("none", "bootstrap", "robustBootrap-stratified", "robustBootstrap-xyz", "BayesianBootstrap"),
                    # how to best deal with it that each method has own parameters?
                    imputationuncertainty = "PMM", #(choices: "PMM", "midastouch", "normal", "residual", "pmm_k=NULL,# if integer value use kNN on predicted values. Should be either of length one or length of number of variables?
                    xvar = colnames(data), # delete this?
                    method = c("lm", "regression","ranger","XGBoost","GPT"), # here I would use default methods for each kind of variable - as in mice - that one can override. Supported methods: "lm", "MM", "ranger", "XGBoost", "GPT", "gam", "robGam")
                    formula = NULL, # possibililty to override the indivual models. A named list (name of variable -> formula used, else default).
                    imp_var=FALSE,
                    imp_suffix="imp", 
                    verbose = FALSE){
  stop("this is not a real function yet")
}