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
#' @param variable variables to be imputed (default: all variables with missings)
#' @param sequential TRUE/FALSE if sequential modelling should be performed
#' @param nseq maximum number of iterations
#' @param model_uncertainty 
#' @param imputation_uncertainty 
#' @param method 
#' @param formula in a named list a specific model formula for the regression
#' model can be specified
#' @param imp_var TRUE/FALSE if a TRUE/FALSE variables for each imputed
#' @param imp_suffix suffix for the TRUE/FALSE variables showing the imputation
#' status
#' @param verbose 
#'
#' @return imputed data set
#' @export
#'
#' @examples
#' vimpute(sleep)
#' vimpute (sleep, formula = list(Span=Span~BodyWgt+BraunWgt:Danger))
vimpute <- function(data,
                    variable = colnames(data), # default all variables with missings
                    sequential = TRUE, nseq = 100,
                    model_uncertainty = c("none", "bootstrap", "robustBootrap-stratified", "robustBootstrap-xyz", "BayesianBootstrap"),
                    # how to best deal with it that each method has own parameters?
                    imputation_uncertainty = "none",#"PMM","PMM_k" #(choices: "PMM", "midastouch", "normal", "residual", "PMM_k",
                    #xvar = colnames(data), # delete this?
                    method = c("robust","regression","ranger","XGBoost","GPT"), # here I would use default methods for each kind of variable - as in mice - that one can override. Supported methods: "lm", "MM", "ranger", "XGBoost", "GPT", "gam", "robGam")
                    formula = NULL, # possibililty to override the indivual models. A named list (name of variable -> formula used, else default).
                    imp_var=FALSE,
                    imp_suffix="imp", 
                    verbose = FALSE){
  stopifnot(length(model_uncertainty)==1|
              length(model_uncertainty)==length(variable))
  stopifnot(length(imputation_uncertainty)==1|
              length(imputation_uncertainty)==length(variable))
  #numeric -> lm bzw. lmrob
  #factor 2-stufig -> glm(family="binomial"), glmrob(family="binomial")
  #pos. Integer -> family="poisson"
  message("this is not a real function yet")
  
#1a) If sequential=TRUE Impute all variables in 'variable' with initialise()
#1b) If sequential=FALSE Imputa all variables in variable with models trained on the observerd part 
  
#2a) reimpute all variables with the specified model

#3) If imputationuncertainty = PMM or PMM_k -> use kNN on predicted values.

#4) Generate FLAG variable if (imp_var=TRUE)
  
  
  output <- data
  return(output)
}