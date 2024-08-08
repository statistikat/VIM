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
#' @param method methods of imputation which can be used, possible options are
#' "robust","regression","ranger","xgboost","GPT"
#' @param formula in a named list a specific model formula for the regression
#' model can be specified
#' @param imp_var TRUE/FALSE if a TRUE/FALSE variables for each imputed
#' @param imp_suffix suffix for the TRUE/FALSE variables showing the imputation
#' status
#' @param verbose Show the number of observations used for training
#'   and evaluating the RF-Model. This parameter is also passed down to
#'   [xgboost::xgboost()] / [ranger::ranger()] to show computation status.
#'
#' @return imputed data set
#' @export
#'
#' @examples
#' x <- vimpute(sleep, method="ranger", sequential = FALSE, imp_var=TRUE)
#' y <- vimpute(sleep, method="ranger", sequential = TRUE, imp_var=TRUE)
#' z <- vimpute(sleep, method="ranger", sequential = TRUE, imp_var=TRUE, imputation_uncertainty ="PMM_1")
#' vimpute (sleep, formula = list(Span=~BodyWgt+BraunWgt:Danger))


#1a) If sequential=TRUE Impute all variables in 'variable' with initialise()
#1b) If sequential=FALSE Impute all variables in variable with models trained on the observed part 
#2a) re-impute all variables with the specified model
#3) If imputation_uncertainty = PMM or PMM_k -> use kNN on predicted values.
#4) Generate FLAG variable if (imp_var=TRUE)


vimpute <- function(data,
                    variable = colnames(data)[apply(data,2,function(x) any (is.na(x)))], # default all variables with missings
                    sequential = TRUE, 
                    nseq = 100, 
                    eps = 5, 
                    model_uncertainty = "none", # c("none", "bootstrap", "robustBootrap-stratified", "robustBootstrap-xyz", "BayesianBootstrap"),
                    # how to best deal with it that each method has own parameters?
                    nboot = 25,
                    imputation_uncertainty = "none", # "PMM_k" #(choices: "midastouch", "normal", "residual", "PMM_k",
                    method = "robust",#c("robust","regression","ranger","xgboost"), # ,"GPT" 
                    formula = NULL, # possibility to override the individual models. A named list (name of variable -> formula used, else default).
                    imp_var=FALSE,
                    imp_suffix="imp", 
                    verbose = FALSE,
                    ...){
  
  ## parameter checks
  stopifnot(is.logical(imp_var))
  stopifnot(is.logical(verbose))
  stopifnot(is.logical(sequential))
  stopifnot(is.numeric(nseq) && nseq > 0)
  stopifnot(is.numeric(eps) && eps > 0)
  stopifnot(is.character(imp_suffix))
  
  if (!is.data.frame(data)) {
    if (is.matrix(data))
      data <- as.data.frame(data)
    else
      stop("A data frame must be provided.")
  }
  
  if(length(variable)==1){
    if(!any(is.na(data[,variable]))){
      stop(variable, " is not a valid input for the parameter 'variable', since this variable has no missing values.")
    }
  }else if(any(apply(data[,variable],2,function(x) any (is.na(x)))==FALSE)){
    message("The parameter `variable` includes variables with no missing values, this variables will be removed.")
    variable <- variable[apply(data[,variable],2,function(x) any (is.na(x)))]
  }
  
  if(!model_uncertainty %in% c("none","bootstrap")){ # , "robustBootrap-stratified", "robustBootstrap-xyz", "BayesianBootstrap"
    stop(model_uncertainty, " is not a valid input for the parameter 'model_uncertainty'.")
  }

  if(substr(imputation_uncertainty, 1, 4)=="PMM_"){ # check for PMM
    if(!nchar(imputation_uncertainty)==5 & substr(imputation_uncertainty, 1, 4)=="PMM_" & 
       as.numeric(substr(imputation_uncertainty,5,5)) %in% c(1:9)){
      stop(imputation_uncertainty, " is not a valid input for the parameter.")
    }
  }else if(!imputation_uncertainty %in% c("none")){ # , "midastouch", "normal", "residual"
    stop(imputation_uncertainty, " is not a valid input for the parameter 'imputation_uncertainty'.")
  }
  
  if(!method %in% c("robust","regression","ranger","xgboost")){ # ,"GPT"
    stop(method, " is not a valid input for the parameter 'method'.")
  }
  
  if(!(is.null(formula) | is.list(formula))){ 
    stop(formula, " is not a valid input for the parameter 'formula'.")
  }
  if(!is.null(formula) && any(substr(formula, 1, 1) != "~")){
    stop(formula, " has not the right form, correct example: formula = list(y=~x1+x2).")
  }
  if((!is.null(formula) && !any(names(formula) %in% names(data))) | 
     (!is.null(formula) && any(!(unlist(strsplit(as.character(sub('.', '', formula)), " \\+ ")) %in% names(data))))){ 
    stop(formula, " has not the right form, correct example: formula = list(y=~x1+x2).")
  }
  # regressionImp, rangerImp und xgboostImp can only handel formula with "+", not interaction terms, *, ^2, ...
  
  if(!is.character(imp_suffix)){
    stop(imp_suffix, "is not a valid input for the parameter 'imp_suffix', needs to be a character.")
  }
  
  
  ## method - general code
  
  current_output <- data
  lhs <- variable
  
  if(sequential==TRUE){
    # impute values for all columns in "variable"
    index_var <- grep(variable, colnames(data))
    current_output[,index_var] <- VIM:::initialise(x=data.frame(data[,index_var]), method="median")
  }
  
  
  ##### sequential #####
  d <- 99999
  it <- 0
  save_imp <- data.frame(matrix(ncol = length(lhs), nrow = nrow(data)))
  types <- rep(NA, length(lhs))
  
  while(d > eps && it < nseq){
    it <- it + 1
    
    for(i in 1:length(lhs)){ # if there are multiple imputed columns
      # i=1
      previous_output <- current_output
      
      # create formula for column lhs[i]
      if(is.null(formula)==FALSE && lhs[i] %in% names(formula)){
        form <- as.formula(paste(lhs[i], formula[lhs[i]]))
      }else{
        sum_miss <- colSums(is.na(data[which(is.na(data[,names(data) %in% lhs[i]])),colnames(data)[!colnames(data) %in% lhs[i]]]))==0
        rhs <- paste(names(sum_miss)[sum_miss==TRUE], collapse = ' + ')
        
        if(length(names(sum_miss)[sum_miss==TRUE])==0){
          rhs <- paste(names(sum_miss), collapse = ' + ')
        }
        form <- as.formula(paste(lhs[i], "~", rhs))
      }
      
      if(model_uncertainty == "bootstrap"){
        pred <- data.frame(matrix(NA,nrow=nboot,ncol=sum(is.na(data[,lhs[i]]))))
        data_missC <- data[is.na(data[,names(data) %in% lhs[i]]),]
        
        for(b in 1:nboot){
          # b=1
          boot_sample <- previous_output[sample(1:nrow(previous_output),nrow(previous_output),replace = TRUE),]
          data_comb <- rbind(boot_sample[!is.na(boot_sample[,lhs[i]]),], data_missC)
          
          id_miss_comb <- which(is.na(data_comb[,names(data_comb) %in% lhs[i]]))
          id_miss_data <- which(is.na(data[,names(data) %in% lhs[i]]))  
          
          # predict missings
          pred[b,] <- switch(
            method,
            regression = regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = FALSE, ...), 
            robust = regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = TRUE, ...), 
            ranger = rangerImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose, ...), 
            xgboost = xgboostImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose, ...),
            # GPT = ,
          )[id_miss_comb,lhs[i]]
        }
        
        current_output[id_miss_data,lhs[i]] <- as.vector(colSums(pred)/nboot)
        
      }else{ # "none"
        data_comb <- rbind(previous_output[!is.na(previous_output[,lhs[i]]),],data[is.na(data[,names(data) %in% lhs[i]]),])
        
        # predict missings
        pred_output <- switch(
          method,
          regression = regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = FALSE, ...), 
          robust = regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = TRUE, ...), 
          ranger = rangerImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose, ...), 
          xgboost = xgboostImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose, ...),
          # GPT = ,
        )
        
        # add (new) estimates to the current_output-dataset
        id_miss_comb <- which(is.na(data_comb[,names(data_comb) %in% lhs[i]]))
        id_miss_data <- which(is.na(data[,names(data) %in% lhs[i]]))  
        
        current_output[id_miss_data,lhs[i]] <- pred_output[id_miss_comb,lhs[i]]
      }
      
      # if imp_var == TRUE, save columns with TRUE/FALSE
      if(imp_var && it == 1){
        lhs_imp <- paste(lhs[i], "_", imp_suffix, sep="")
        save_imp[,i] <- is.na(data[,lhs[i]])
        names(save_imp)[i] <- lhs_imp
      }
    }
    
    # if sequential == FALSE stop after first iteration
    if(!sequential) break  
    
    # update d-value - different types of imputed variables (numeric, factor, ...) !!
    class1 <- function(x) class(x)[1]
    
    if(length(lhs)==1){
      types <- class1(data[,lhs])
    }else{
      types <- lapply(data[,lhs], class1)
    }
    
    getType <- Vectorize(vectorize.args = "a",
              FUN = function(a) {
                switch(as.character(a),
                  integer = "numeric",
                  numeric = "numeric",
                  double = "numeric",
                  logical = "bin",
                  mixed = "numeric",
                  binary = "bin",
                  nominal = "factor",
                  ordered = "factor",
                  stop("unsupported variable type"))}
    )
    types <- as.vector(getType(a = types))
    
    
    if(any(types %in% c("numeric","bin")) && it>1){
      col <- lhs[types %in% c("numeric","bin")] 
      diff_all <- (current_output[,col] - previous_output[,col])^2
      
      if(length(lhs)==1){
        d <- sum(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE)
      }else{
        d <- sum(colSums(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE))
      }
      
    }else if(any(types %in% "factor") && it>1){
      col <- lhs[types %in% "factor"]
      diff_all <- (as.factor(current_output[,col]) - as.factor(previous_output[,col]))^2
      
      if(length(lhs)==1){
        d <- sum(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE)
      }else{
        d <- sum(colSums(replace(diff_all, save_imp ==FALSE, NA),  na.rm = TRUE))
      }
    }
  }
  
  # if imp_var == TRUE add the *_imp columns to the dataset
  if(imp_var && imputation_uncertainty == "none"){
    current_output <- cbind(current_output, save_imp)
  }
  
  ##### imputation_uncertainty ##### 
  
  if(imputation_uncertainty=="none"){ # default "none" - so use directly the predicted values for imputation
    output <- current_output
    
  }else if(substr(imputation_uncertainty, 1, 3)=="PMM"){ 
    k <- substr(imputation_uncertainty, 5, 5)
    output <- data
    
    for(i in 1:length(lhs)){ # if there are multiple imputed columns
      # i=1
      dat <- current_output
      dat[,names(dat) %in% lhs[i]==TRUE] <- NA # set all values of the column lhs[i] to NA
      data_comb <- rbind(current_output[!is.na(current_output[,names(current_output) %in% lhs[i]]),],dat)
      
      # create formula for column lhs[i]
      if(is.null(formula)==FALSE && lhs[i] %in% names(formula)){
        form <- as.formula(paste(lhs[i], formula[lhs[i]]))
      }else{
        sum_miss <- colSums(is.na(data[which(is.na(data[,names(data) %in% lhs[i]])),colnames(data)[!colnames(data) %in% lhs[i]]]))==0
        rhs <- paste(names(sum_miss)[sum_miss==TRUE], collapse = ' + ')
        
        if(length(names(sum_miss)[sum_miss==TRUE])==0){
          rhs <- paste(names(sum_miss), collapse = ' + ')
        }
        form <- form <- as.formula(paste(lhs[i], "~", rhs))
      }
      
      dat_pmm <- switch(
        method,
        regression = regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = FALSE, ...),
        robust = regressionImp(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, robust = TRUE, ...),
        ranger = rangerImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose, ...),
        xgboost = xgboostImpute(formula = form, data = data_comb, imp_var = TRUE, imp_suffix = imp_suffix, verbose = verbose, ...),
        # GPT = ,
      )
      
      # add estimates for all observations to original dataset as column Predict
      lhs_imp <- paste(lhs[i], "_", imp_suffix, sep="")
      
      part <- dat_pmm[dat_pmm[,(names(dat_pmm) %in% lhs_imp)]==TRUE, ]
      output$Predict <- part[,variable[i]]
      
      # apply the kNN function
      knnP <- kNN(output,k=k,dist_var="Predict", variable=variable[i], imp_suffix = imp_suffix)
      output$Predict <- NULL
      
      # add estimates to the current-output-dataset and output-dataset
      output[,lhs[i]] <- knnP[,lhs[i]]
      current_output[,lhs[i]] <- knnP[,lhs[i]]
      
      if(imp_var){ # if TRUE add also column lhs_imp
        output <- cbind(output, knnP[,lhs_imp])
        names(output)[ncol(output)] <- lhs_imp
      }
    }
  }
  return(output)
}


if(0){
  # for testing
  
  data=VIM::sleep
  variable = c("Dream", "NonD", "Sleep") # colnames(data)[apply(data,2,function(x) any (is.na(x)))]
  sequential = FALSE
  nseq = 10
  eps = 5
  model_uncertainty = "bootstrap"
  imputation_uncertainty = "none"
  method = "regression"
  robust = TRUE
  formula = list(NonD=~BodyWgt+BrainWgt) #  NULL
  imp_var=FALSE
  imp_suffix="imp"
  verbose = FALSE
  nboot = 25
  
  
  test <- vimpute(data=VIM::sleep, method="regression", imp_var=TRUE, sequential = FALSE)
  test1 <- vimpute(data=VIM::sleep, formula = list(NonD=~BodyWgt+BrainWgt),
                   method="regression", robust=TRUE, imp_var=TRUE, sequential = FALSE) ## error
  test2 <- vimpute(data=VIM::sleep, formula = list(Dream=~BodyWgt+BrainWgt), imputation_uncertainty = "PMM_3",
                   method="regression", imp_var=TRUE, sequential = FALSE)
  test3 <- vimpute(data=VIM::sleep, imputation_uncertainty = "PMM_3", method="ranger", imp_var=TRUE, sequential = FALSE)
  test4 <- vimpute(data=VIM::sleep, imputation_uncertainty = "PMM_3", method="regression", imp_var=FALSE, sequential = FALSE)
  test5 <- vimpute(data=VIM::sleep, imputation_uncertainty = "PMM_3", method="ranger", imp_var=TRUE, sequential = FALSE)
  test6 <- vimpute(data=VIM::sleep, formula = list(Dream=~BodyWgt+BrainWgt), imputation_uncertainty = "none", 
                   method="regression", imp_var=TRUE, nseq=10, sequential = TRUE)
  test7 <- vimpute(data=VIM::sleep, formula = list(NonD=~BodyWgt+BrainWgt), imputation_uncertainty = "PMM_3", 
                   method="regression", imp_var=TRUE, nseq=10, sequential = TRUE, imp_suffix="xxx")
  test13 <- vimpute(data=VIM::sleep, formula = list(Dream=~Span+Gest), imputation_uncertainty = "none", 
                    method="regression", imp_var=FALSE, nseq=10, sequential = TRUE, imp_suffix="imp")
  test14 <- vimpute(data=VIM::sleep, variable = c("Dream", "NonD", "Sleep"), sequential = FALSE, model_uncertainty = "none",
                    imputation_uncertainty = "none", method="regression", formula = list(Dream=~BodyWgt+BrainWgt, NonD=~BodyWgt+BrainWgt),
                    imp_var=TRUE)
  
  # compare bootstrap vs. no bootstrap (all other option stay the same)
  testa <- vimpute(data=VIM::sleep, method="regression",formula = list(Dream=~BodyWgt+BrainWgt), variable = "Dream",
                   imp_var=TRUE, sequential = FALSE, model_uncertainty = "none")
  testb <- vimpute(data=VIM::sleep, method="regression",formula = list(Dream=~BodyWgt+BrainWgt), variable = "Dream",
                   imp_var=TRUE, sequential = FALSE, model_uncertainty = "bootstrap", nboot = 10)
  
  cbind(testa$Dream, testb$Dream)[is.na(VIM::sleep$Dream),]
  
  
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
  
  ##
  
  data=df
  variable = colnames(df)[apply(df,2,function(x) any (is.na(x)))]
  sequential = FALSE
  nseq = 100
  eps = 5
  model_uncertainty = "bootstrap"
  imputation_uncertainty = "none"
  method = "regression"
  robust = FALSE
  formula =  NULL # list(Dream=~Span+Gest) # NULL 
  imp_var=FALSE
  imp_suffix="imp"
  verbose = FALSE
  nboot = 25
  
  testa <- vimpute(data=df, method="regression",formula = list(S.Length=~S.Width+P.Length), variable = "S.Length",
                   imp_var=TRUE, sequential = FALSE, model_uncertainty = "none")
  testb <- vimpute(data=df, method="regression",formula = list(S.Length=~S.Width+P.Length), variable = "S.Length",
                   imp_var=TRUE, sequential = FALSE, model_uncertainty = "bootstrap", nboot = 10)
  
  cbind(round(testa$S.Length,1) - iris$Sepal.Length, round(testb$S.Length,1)- iris$Sepal.Length)
  
  
  test8 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"),
                   method="regression", imp_var=FALSE, sequential = TRUE, imp_suffix="imp")
  test9 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"), imputation_uncertainty = "PMM_3", 
                   method="regression", imp_var=FALSE, sequential = FALSE, imp_suffix="imp")
  test10 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"),
                    method="regression", imp_var=FALSE, nseq=10, sequential = TRUE, imp_suffix="imp")
  test11 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"), imputation_uncertainty = "PMM_3", 
                    method="regression", imp_var=FALSE, nseq=10, sequential = TRUE, imp_suffix="imp")
  
  cbind(iris$Sepal.Length - round(test8$S.Length,1), 
        iris$Sepal.Length - test9$S.Length, 
        iris$Sepal.Length - round(test10$S.Length,1),
        iris$Sepal.Length - round(test11$S.Length,1))
  
  test12 <- vimpute(data=df, imputation_uncertainty = "none", 
                    method="regression", imp_var=FALSE, sequential = FALSE, imp_suffix="imp")
  
  ??adf.test  
}


