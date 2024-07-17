#' VIM Model Based Imputation framework
#' generally idea function that incorporates a framework for model based imputation. 
#' Options should be
#' - sequential modelling
#' - using PMM or not
#' - bootstrap the model error
#' - drawing the predicted value from the "posterior" distribution
#' - model options: (robust) regression, ranger, xgboost, some kind of transformer model
#' - complex formula (with `formula`) for each variable possible. 
#'
#' @param data data.frame or matrix
#' @param variable variables to be imputed (default: all variables with missings)
#' @param sequential TRUE/FALSE if sequential modelling should be performed
#' @param nseq maximum number of iterations
#' @param eps threshold for convergency
#' @param model_uncertainty 
#' @param imputation_uncertainty 
#' @param method 
#' @param robust `TRUE`/`FALSE` if robust regression should be used
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
                    variable = colnames(data)[apply(data,2,function(x) any (is.na(x)))], # default all variables with missings
                    sequential = TRUE, 
                    nseq = 100, # maximum number of iterations
                    eps = 5, # threshold for convergency
                    model_uncertainty = c("none", "bootstrap", "robustBootrap-stratified", "robustBootstrap-xyz", "BayesianBootstrap"),
                    # how to best deal with it that each method has own parameters?
                    imputation_uncertainty = "none",#"PMM","PMM_k" #(choices: "PMM", "midastouch", "normal", "residual", "PMM_k",
                    #xvar = colnames(data), # delete this?
                    method = c("regression","ranger","xgboost","GPT"), # here I would use default methods for each kind of variable - as in mice - that one can override. Supported methods: "lm", "MM", "ranger", "XGBoost", "GPT", "gam", "robGam")
                    robust = FALSE,
                    formula = NULL, # possibility to override the individual models. A named list (name of variable -> formula used, else default).
                    imp_var=FALSE,
                    imp_suffix="imp", 
                    verbose = FALSE){
  
 ' stopifnot(length(model_uncertainty)==1|
              length(model_uncertainty)==length(variable))
  stopifnot(length(imputation_uncertainty)==1|
              length(imputation_uncertainty)==length(variable))
  message("this is not a real function yet")'
  
#1a) If sequential=TRUE Impute all variables in 'variable' with initialise()
#1b) If sequential=FALSE Impute all variables in variable with models trained on the observed part 
  
  # eine Schleife darum, mit for i in 1:x mit
  # x=1 wenn sequential=FALSE und 
  # x=nseq wenn sequential=TRUE

  
#2a) re-impute all variables with the specified model

#3) If imputation_uncertainty = PMM or PMM_k -> use kNN on predicted values.

#4) Generate FLAG variable if (imp_var=TRUE)
  
  
  stopifnot(is.logical(robust))
  stopifnot(is.logical(imp_var))
  stopifnot(is.logical(verbose))
  
  if (!is.data.frame(data)) {
    if (is.matrix(data))
      data <- as.data.frame(data)
    else
      stop("data frame must be provided")
  }
  
  if(any(apply(data[,variable],2,function(x) any (is.na(x)))==FALSE)){
    
    message("The parameter `variable` includes variables with no missing values, this variables will be removed")
    variable <- variable[apply(data[,variable],2,function(x) any (is.na(x)))] 
    
    if(length(variable)==0){
      stop("Parameter `variable` is empty.")
    }
  }
  
  if(substr(imputation_uncertainty, 1, 4)=="PMM_"){ # check for PMM
    if(!nchar(imputation_uncertainty)==5 & substr(imputation_uncertainty, 1, 4)=="PMM_" & 
       as.numeric(substr(imputation_uncertainty,5,5)) %in% c(1:9)){
      stop(imputation_uncertainty, " is not a valid method")
    }
  }else if(!imputation_uncertainty %in% c("none", "midastouch", "normal", "residual")){
    stop(imputation_uncertainty, " is not a valid method")
  }
  
  if(!(is.null(formula) | is.list(formula))){
    stop("wrong imput for parameter formula")
  }
  
  if(!method %in% c("regression","ranger","xgboost","GPT")){
    stop(method, " is not a valid method")
  }
  
  if(!is.character(imp_suffix)){
    stop(imp_suffix, " is not a valid imp_suffix, needs to be a String")
  }
  
  
  # method - general code
  if (is.null(formula)){ # impute values for all columns with missing values (columns in parameter "variable")
    # create the formula
    rhs <- paste(colnames(data)[!colnames(data) %in% variable], collapse = ' + ') # extract all variables (names) with no missing values
    lhs <- variable
    formula <- as.formula(paste(paste(variable, collapse = '+'), "~", rhs))
  } else {
    formchar <- as.character(formula[[1]])
    rhs <- formchar[3] # gsub(" ", "", strsplit(formchar[3], "\\+")[[1]])
    lhs <- gsub(" ", "", strsplit(formchar[2], "\\+")[[1]])
    variable <- lhs # ???? überschreiben von Imput 
  }
  
  ##### sequential #####
  d <- 99999
  it <- 0
  previous_output <- data
  current_output <- data
  save_imp <- data.frame(matrix(ncol = length(lhs), nrow = nrow(data)))
  types <- rep(NA, length(lhs))
  
  while (d > eps && it < nseq) {
    it <- it + 1
    previous_output <- current_output
    
    for(i in 1:length(lhs)){ # if there are multiple imputed columns
      
      if(it == 1){
        data_comb <- data
      } else {
        data_comb <- rbind(previous_output,data[is.na(data[,names(data) %in% lhs[i]]),])
      }

      form <- as.formula(paste(lhs[i], "~", rhs))
        
      # predict missings
      pred_output <- switch(
        method,
        regression = VIM::regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = robust), # , ...
        ranger = VIM::rangerImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose), # , ...
        xgboost = VIM::xgboostImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose),
        # GPT = ,
      )
      
      # add (new) estimates to the current_output-dataset
      id_miss_comb <- which(is.na(data_comb[,names(data_comb) %in% lhs[i]]))
      id_miss_data <- which(is.na(data[,names(data) %in% lhs[i]]))  
      
      current_output[id_miss_data,lhs[i]] <- pred_output[id_miss_comb,lhs[i]]
      
      
      lhs_imp <- paste(lhs[i], "_", imp_suffix, sep="")
      
      if(imp_var && it == 1){ # if TRUE save column lhs_imp from first iteration
        save_imp[,i] <- pred_output[,lhs_imp]
        names(save_imp)[i] <- lhs_imp
      }
    }
    
    # if sequential == FALSE stop look after first iteration
    if(!sequential) break  
    
    # update d-value - different types of imputed variables (numeric, factor, ...) !!
    class1 <- function(x) class(x)[1]
    
    if(length(lhs)==1){
      types <- class1(data[,lhs])
    } else {
      types <- lapply(data[,lhs], class1)
    }
    
    
    getType <- Vectorize(vectorize.args = "a",
              FUN = function(a) {
                switch(as.character(a),
                  integer = "numeric",
                  numeric = "numeric",
                  mixed = "numeric",
                  binary = "bin",
                  logical = "bin",
                  nominal = "factor",
                  count = "count", 
                  ordered = "ordered",
                  stop("unsupported variable type"))}
    )
    types <- as.vector(getType(a = types))
    
    if(any(types %in% c("numeric","bin"))){
      col <- lhs[types %in% c("numeric","bin")] 
      
      diff_all <- (current_output[,col] - previous_output[,col])^2
      
      if(length(lhs)==1){
        d <- sum(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE)
      } else {
        d <- sum(colSums(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE))
      }
      
    }else if(any(types %in% "factor")){
      col <- lhs[types %in% "factor"]
      
      diff_all <- (as.factor(current_output[,col]) - as.factor(previous_output[,col]))^2
      
      if(length(lhs)==1){
        d <- sum(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE)
      } else {
        d <- sum(colSums(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE))
      }
      
    }else if(any(types %in% "count")){
      d <- d # ???
    }else{ # type "ordered"
      d <- d # ???
    } 
  }
  print(it)
  # if imp_var == TRUE add the *_imp columns to the dataset
  if(imp_var && imputation_uncertainty == "none"){
    current_output <- cbind(current_output, save_imp)
  }
  
  
  ##### imputation_uncertainty ##### 
  
  if(imputation_uncertainty=="none"){ # default "none" - so use directly the predicted values for imputation
    output <- current_output
    
  } else if(substr(imputation_uncertainty, 1, 3)=="PMM"){ 
    k <- substr(imputation_uncertainty, 5, 5)
    output <- data
    
    for(i in 1:length(lhs)){ # if there are multiple imputed columns
      # i=2
      dat <- current_output
      dat[,names(dat) %in% lhs[i]==TRUE] <- NA
      data_comb <- rbind(current_output[!is.na(current_output[,names(current_output) %in% lhs[i]]),],dat)
      
      form <- as.formula(paste(lhs[i], "~", rhs))
      
      dat_pmm <- switch(
        method,
        regression = VIM::regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = robust), # , ...
        ranger = VIM::rangerImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose), # , ...
        xgboost = VIM::xgboostImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose),
        # GPT = ,
      )
      
      # add estimates for all observations to original dataset as column Predict
      lhs_imp <- paste(lhs[i], "_", imp_suffix, sep="")
      
      part <- dat_pmm[dat_pmm[,(names(dat_pmm) %in% lhs_imp)]==TRUE, ]
      output$Predict <- part[,variable[i]]
      
      # apply the kNN function
      knnP <- VIM::kNN(output,k=k,dist_var="Predict", variable=variable[i], imp_suffix = imp_suffix)
      output$Predict <- NULL
      
      # add estimates to the current-output-dataset and output-dataset
      output[,lhs[i]] <- knnP[,lhs[i]]
      current_output[,lhs[i]] <- knnP[,lhs[i]]
      
      if(imp_var){ # if TRUE add also column lhs_imp
        output <- cbind(output, knnP[,lhs_imp])
        names(output)[ncol(output)] <- lhs_imp
      }
    }

  }else{
    message("methods midastouch, normal and residual currently not implemented")
  }
  return(output)
}



# for testing

data=VIM::sleep
# variable = colnames(data)[apply(data,2,function(x) any (is.na(x)))]
sequential = FALSE
nseq = 100
eps = 5
model_uncertainty = "none"
imputation_uncertainty = "PMM_3"
method = "regression"
robust = FALSE
formula =  NULL # list(SpanDream=Dream+NonD~BodyWgt+BrainWgt) # NULL 
imp_var=TRUE
imp_suffix="xxx"
verbose = FALSE

test <- vimpute(data=VIM::sleep, imputation_uncertainty = "none", method="regression", imp_var=TRUE, sequential = FALSE)

test1 <- vimpute(data=VIM::sleep, formula = list(Span=NonD~BodyWgt+BrainWgt), imputation_uncertainty = "none", 
                 method="regression", robust=TRUE, imp_var=TRUE, sequential = FALSE)

test2 <- vimpute(data=VIM::sleep, formula = list(Span=Dream+NonD~BodyWgt+BrainWgt), imputation_uncertainty = "PMM_3",
                 method="regression", imp_var=TRUE, sequential = FALSE)

test3 <- vimpute(data=VIM::sleep, imputation_uncertainty = "PMM_3", method="regression", imp_var=TRUE, sequential = FALSE)

test4 <- vimpute(data=VIM::sleep, imputation_uncertainty = "PMM_3", method="regression", imp_var=FALSE, sequential = FALSE)

test5 <- vimpute(data=VIM::sleep, imputation_uncertainty = "PMM_3", method="ranger", imp_var=TRUE, sequential = FALSE)

test6 <- vimpute(data=VIM::sleep, formula = list(Span=Dream+NonD~BodyWgt+BrainWgt), imputation_uncertainty = "none", 
                 method="regression", imp_var=TRUE, nseq=10, sequential = TRUE)

test7 <- vimpute(data=VIM::sleep, formula = list(Span=Dream+NonD~BodyWgt+BrainWgt), imputation_uncertainty = "PMM_3", 
                 method="regression", imp_var=TRUE, nseq=10, sequential = TRUE, imp_suffix="xxx")



###########


df <- iris
colnames(df) <- c("S.Length","S.Width","P.Length","P.Width","Species")
# randomly produce some missing values in the data
set.seed(1)
nbr_missing <- 50
y <- data.frame(row = sample(nrow(iris), size = nbr_missing, replace = TRUE),
                col = sample(ncol(iris), size = nbr_missing, replace = TRUE))
y<-y[!duplicated(y), ]
df[as.matrix(y)] <- NA

VIM::aggr(df)


test8 <- vimpute(data=df, formula = list(Span=S.Length~S.Width+P.Length), imputation_uncertainty = "none", 
                 method="regression", imp_var=FALSE, nseq=10, sequential = FALSE, imp_suffix="imp")

test9 <- vimpute(data=df, formula = list(Span=S.Length~S.Width+P.Length), imputation_uncertainty = "PMM_3", 
                 method="regression", imp_var=FALSE, nseq=10, sequential = FALSE, imp_suffix="imp")

test10 <- vimpute(data=df, formula = list(Span=S.Length~S.Width+P.Length), imputation_uncertainty = "none", 
                 method="regression", imp_var=FALSE, nseq=10, sequential = TRUE, imp_suffix="imp")


cbind(iris$Sepal.Length, test8$S.Length, test9$S.Length, test10$S.Length)


