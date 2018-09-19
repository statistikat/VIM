####Hotdeck in context of kNN-k Nearest Neighbour Imputation
#Author: Alexander Kowarik, Statistics Austria
## (k)NN-Imputation
#data - data.frame of the data with missing
#variable - vector of variablesnames to be imputed
#metric - method for distance computation of in function daisy(cluster), otherwise automatical selection
#k - number of neighbours used
#dist_var - list/vector of the variablenames used for distance computation
#weights - list/vector of the weights for the different dist variables
#numFun - function for evaluating the k NN (numerical variable)
#catFun - function for evaluating the k NN (categorical variable)
#makeNA - vector of values which should be imputed too e.g. 8,9 or 98,99 in SPSS-data sets
#NAcond - list of conditions for each variable to create NAs there (not yet implemented)
#donorcond - list of conditions for a donor e.g. "<=10000"
#TODO: Donors from cold deck



#' k-Nearest Neighbour Imputation
#' 
#' k-Nearest Neighbour Imputation based on a variation of the Gower Distance
#' for numerical, categorical, ordered and semi-continous variables.
#' 
#' The function sampleCat samples with probabilites corresponding to the
#' occurrence of the level in the NNs. The function maxCat chooses the level
#' with the most occurrences and random if the maximum is not unique. The
#' function gowerD is used by kNN to compute the distances for numerical,
#' factor ordered and semi-continous variables. 
#' 
#' @aliases kNN sampleCat maxCat gowerD
#' @param data data.frame or matrix
#' @param variable variables where missing values should be imputed
#' @param metric metric to be used for calculating the distances between
#' @param k number of Nearest Neighbours used
#' @param dist_var names or variables to be used for distance calculation
#' @param weights weights for the variables for distance calculation.
#' If \code{weights = "auto"} weights will be selected based on variable importance from random forest regression, using function \code{\link[ranger]{ranger}}.
#' Weights are calculated for each variable seperately.
#' @param numFun function for aggregating the k Nearest Neighbours in the case
#' of a numerical variable
#' @param catFun function for aggregating the k Nearest Neighbours in the case
#' of a categorical variable
#' @param makeNA list of length equal to the number of variables, with values, that should be converted to NA for each variable
#' @param NAcond list of length equal to the number of variables, with a condition for imputing a NA
#' @param impNA TRUE/FALSE whether NA should be imputed
#' @param donorcond condition for the donors e.g. ">5"
#' @param trace TRUE/FALSE if additional information about the imputation
#' process should be printed
#' @param imp_var TRUE/FALSE if a TRUE/FALSE variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix for the TRUE/FALSE variables showing the imputation
#' status
#' @param addRF TRUE/FALSE each variable will be modelled using random forest regression (\code{\link[ranger]{ranger}}) and used as additional distance variable.
#' @param onlyRF TRUE/FALSE if TRUE only additional distance variables created from random forest regression will be used as distance variables.
#' @param addRandom TRUE/FALSE if an additional random variable should be added
#' for distance calculation
#' @param mixed names of mixed variables
#' @param mixed.constant vector with length equal to the number of
#' semi-continuous variables specifying the point of the semi-continuous
#' distribution with non-zero probability
#' @param useImputedDist TRUE/FALSE if an imputed value should be used for distance calculation for imputing another variable.
#' Be aware that this results in a dependency on the ordering of the variables.
#' @param weightDist TRUE/FALSE if the distances of the k nearest neighbours should be used as weights in the
#' aggregation step
#' @return the imputed data set.
#' @author Alexander Kowarik, Statistik Austria
#' @references A. Kowarik, M. Templ (2016) Imputation with
#' R package VIM.  \emph{Journal of
#' Statistical Software}, 74(7), 1-16.
#' @keywords manip
#' @examples
#' 
#' data(sleep)
#' kNN(sleep)
#' library(laeken)
#' kNN(sleep, numFun = weightedMean, weightDist=TRUE)
#' 
#' @export kNN
#' @export sampleCat
#' @export maxCat
#' @export gowerD
#' @S3method kNN data.frame
#' @S3method kNN survey.design
#' @S3method kNN default
kNN <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                numFun = median, catFun=maxCat,
                makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                imp_var=TRUE,imp_suffix="imp", addRF=FALSE, onlyRF=FALSE,addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  UseMethod("kNN", data)
}
kNN.data.table <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
    numFun = median, catFun=maxCat,
    makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
    imp_var=TRUE,imp_suffix="imp", addRF=FALSE, onlyRF=FALSE, addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  kNN_work(copy(data), variable, metric, k, dist_var,weights, numFun, catFun,
      makeNA, NAcond, impNA, donorcond, mixed, mixed.constant, trace,
      imp_var, imp_suffix, addRF, onlyRF, addRandom,useImputedDist,weightDist)
}
kNN.data.frame <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                           numFun = median, catFun=maxCat,
                           makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                           imp_var=TRUE,imp_suffix="imp", addRF=FALSE, onlyRF=FALSE, addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  as.data.frame(kNN_work(as.data.table(data), variable, metric, k, dist_var,weights, numFun, catFun,
           makeNA, NAcond, impNA, donorcond, mixed, mixed.constant, trace,
           imp_var, imp_suffix, addRF, onlyRF, addRandom,useImputedDist,weightDist))
}

kNN.survey.design <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                              numFun = median, catFun=maxCat,
                              makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                              imp_var=TRUE,imp_suffix="imp", addRF=FALSE, onlyRF=FALSE, addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  data$variables <- kNN_work(data$variables, variable, metric, k, dist_var,weights, numFun, catFun,
           makeNA, NAcond, impNA, donorcond, mixed, mixed.constant, trace,
           imp_var, imp_suffix, addRF, onlyRF,addRandom,useImputedDist,weightDist)
  data$call <- sys.call(-1)
  data
}

kNN.default <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                        numFun = median, catFun=maxCat,
                        makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                        imp_var=TRUE,imp_suffix="imp", addRF=FALSE, onlyRF=FALSE, addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  kNN_work(as.data.table(data), variable, metric, k, dist_var,weights, numFun, catFun,
           makeNA, NAcond, impNA, donorcond, mixed, mixed.constant, trace,
           imp_var, imp_suffix, addRF, onlyRF, addRandom,useImputedDist,weightDist)
}
lengthL <- function(x){
  if(is.list(x)){
    return(sapply(x,length))
  }else{
    return(length(x))
  }
}
sampleCat <- function(x,weights = NULL){
  #sample with probabilites corresponding to there number in the NNs
  if(!is.factor(x))
    x <- as.factor(x)
  s <- summary(x)
  s <- s[s!=0]
  sample(names(s),1,prob=s)
}
maxCat <- function(x,weights = NULL){
  #choose cat with max prob, random if max is not unique
  if(!is.factor(x))
    x <- as.factor(x)
  s <- summary(x)
  s <- s[s!=0]
  if(sum(s>0)>1)
    s <- sample(s)
  names(s)[which.max(s)]
}
kNN_work <-
    function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
        numFun = median, catFun=maxCat,
        makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
        imp_var=TRUE,imp_suffix="imp",addRF=FALSE, onlyRF=FALSE ,addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE){
  #basic checks
  if(!is.null(mixed.constant)){
    if(length(mixed.constant)!=length(mixed))
      stop("length 'mixed.constant' and length 'mixed' differs")
  }
  startTime <- Sys.time()
  nvar <- length(variable)
  ndat <- nrow(data)
  #impNA==FALSE -> NAs should remain NAs (Routing NAs!?)
  indexNAs <- is.na(data)
  if(!is.null(makeNA)){
    if(length(makeNA)!=nvar)
      stop("The vector 'variable' must have the same length as the 'makeNA' list")
    else{
      for(i in 1:nvar){
        data[data[,sapply(.SD,function(x)x%in%makeNA[[i]])[,1],.SDcols=variable[i]],variable[i]:=NA]#,with=FALSE]  
      }
    }
    if(!impNA){
      indexNA2s <- is.na(data)&!indexNAs
    }else
      indexNA2s <- is.na(data)
  }else{
    indexNA2s <- is.na(data)
  }
  if(sum(indexNA2s)<=0){
    warning("Nothing to impute, because no NA are present (also after using makeNA)")
    invisible(data)
  }
  if(imp_var){
    imp_vars <- paste(variable,"_",imp_suffix,sep="")
    index_imp_vars <- which(!imp_vars%in%colnames(data))
    index_imp_vars2 <- which(imp_vars%in%colnames(data))
    if(length(index_imp_vars)>0){
      data[,imp_vars[index_imp_vars]:=FALSE]#,with=FALSE]
      for(i in index_imp_vars){
        data[indexNA2s[,variable[i]],imp_vars[i]:=TRUE]
          #if(!any(indexNA2s[,variable[i]]))
            #data<-data[,-which(names(data)==paste(variable[i],"_",imp_suffix,sep=""))]
      }
    }
    if(length(index_imp_vars2)>0){
      warning(paste("The following TRUE/FALSE imputation status variables will be updated:",
              paste(imp_vars[index_imp_vars2],collapse=" , ")))
      for(i in index_imp_vars2)
        data[,imp_vars[i]:=as.logical(data[,imp_vars[i]])]#,with=FALSE]
    }
  }
  for(v in variable){
    if(data[,sapply(.SD,function(x)all(is.na(x))),.SDcols=v]){
      warning(paste("All observations of",v,"are missing, therefore the variable will not be imputed!\n"))
      variable <- variable[variable!=v]  
    }
  }
  if(length(variable)==0){
    warning(paste("Nothing is imputed, because all variables to be imputed only contains missings."))
    return(data)
  }
  orders <- data[,sapply(.SD,is.ordered)]
  orders <- colnames(data)[orders]
  levOrders <- vector()
  if(length(orders)>0){
    levOrders <- data[,sapply(.SD,function(x)length(levels(x))),.SDcols=orders]
  }
  factors <- data[,sapply(.SD,function(x)is.factor(x)|is.character(x))]
  factors <- colnames(data)[factors]
  factors <- factors[!factors%in%orders]
  
  numerical <- data[,sapply(.SD,function(x)is.numeric(x)|is.integer(x))]
  numerical <- colnames(data)[numerical]
  numerical <- numerical[!numerical%in%mixed]
  if(trace){
    cat("Detected as categorical variable:\n")
    print(factors)
    cat("Detected as ordinal variable:\n")
    print(orders)
    cat("Detected as numerical variable:\n")
    print(numerical)  
  }

  ###Make an index for selecting donors
  INDEX <- 1:ndat
  ##START DISTANCE IMPUTATION
  ## if(is.null(metric))
  ##   metric <- c("euclidean", "manhattan", "gower")
  ## else if(!metric%in%c("euclidean", "manhattan", "gower"))
  ##   stop("metric is unknown")
  
  # add features using random forest (ranger)
  if(addRF){

    features_added <- c()
    dist_var_new <- list()
    weights_new <- list()
    # create data set without missings for regressors
    # seems to be most efficient way
    # can still be improved...?
    dataRF <- suppressWarnings(kNN(data[,unique(c(unlist(dist_var)
                                                  ,variable)),with=FALSE],imp_var = FALSE))
    
    for(i in 1:nvar){
      
      if(any(indexNA2s[,variable[i]])){
        if(is.list(dist_var)){
          dist_var_cur <- dist_var[[i]]
        }else{
          dist_var_cur <- dist_var
        }
        regressors <- dist_var_cur[dist_var_cur!=variable[i]]
        index.miss <- data[is.na(get(variable[i])),which=TRUE]

        data.mod <- dataRF[-c(index.miss),unique(c(dist_var_cur,variable[i])),with=FALSE]
        
        if(nrow(data.mod)==0){
          warning("cannot use random forest for ",variable[i],"\n too many missing values in the data")
          next;
        }
        ranger.formula <- as.formula(paste(variable[i],paste(regressors,collapse = "+"),sep="~"))
        
        ranger.mod <- ranger(ranger.formula,data=data.mod)
        
        new_feature <- c(paste0(variable[i],"randomForestFeature"))
        data[,c(new_feature):=predict(ranger.mod,data=dataRF)$predictions]

        features_added <- c(features_added,new_feature)
        
        if(variable[i]%in%mixed){
          mixed <- c(mixed,new_feature)
        }else if(variable[i]%in%numerical){
          numerical <- c(numerical,new_feature)
        }else if(variable[i]%in%orders){
          orders <- c(orders,new_feature)
        }else if(variable[i]%in%factors){
          factors <- c(factors,new_feature)
        }
        
        if(onlyRF){
          dist_var_new[[i]] <- c(new_feature)
        }else{
          dist_var_new[[i]] <- c(dist_var_cur,new_feature)
          if(!is.null(weights)&&weights[1]!="auto"){
            if(is.list(weights)){
              weights_new[[i]] <- c(weights[[i]],median(weights[[i]]))
            }else{
              weights_new[[i]] <- c(weights,median(weights))
            }
            
          }
        }
 
      }
    }
    rm(dataRF)
    # create sets for distance variables
    dist_var <- dist_var_new
    if(!is.null(weights)&&weights[1]!="auto"){
      weights <- weights_new
    }
  }else{
    if(onlyRF){
      onlyRF <- FALSE
      warning("The onlyRF is automatically set to FALSE, because addRF=FALSE.")
    }
    features_added <- NULL
  }
  # set weights vector
  if(is.null(weights)){
    if(is.list(dist_var)){
      weights <- lapply(dist_var,function(z){rep(1,length(z))})
    }else{
      weights <- rep(1,length(dist_var))
    }

  }else if(weights[1]=="auto"){
    # use random forest and importance values for automatic weighting
    # setup dist_var and weights as lists
    # for each model different weights
    weights_new <- list()
    dist_var_new <- list()

    for(i in 1:nvar){
      if(any(indexNA2s[,variable[i]])){
        
        if(is.list(dist_var)){
          regressors <- dist_var[[i]][dist_var!=variable[i]]
          data.mod <- na.omit(subset(data,select=unique(c(variable[i],dist_var[[i]]))))
        }else{
          regressors <- dist_var[dist_var!=variable[i]]
          data.mod <- na.omit(subset(data,select=unique(c(variable[i],dist_var))))
        }
        
        ranger.formula <- as.formula(paste(variable[i],paste(regressors,collapse = "+"),sep="~"))
        ranger.mod <- ranger(ranger.formula,data=data.mod,importance="impurity")
        dist_var_new[[i]] <- regressors
        weights_new[[i]] <- importance(ranger.mod)
        
      }
    }
    weights <- weights_new
    dist_var <- dist_var_new
    rm(weights_new,dist_var_new)
  }else if(any(lengthL(weights)!=lengthL(dist_var))){
    stop("length of weights must be equal the number of distance variables")
  }
  if(addRandom){
    numerical <- c(numerical, "RandomVariableForImputation")
    data[,"RandomVariableForImputation":=rnorm(ndat)]#,with=FALSE] 
    if(is.list(dist_var)){
      for(i in 1:length(dist_var)){
        dist_var[[i]] <- c(dist_var[[i]],"RandomVariableForImputation")
        weights[[i]] <- c(weights[[i]],min(weights[[i]])/(sum(weights[[i]])+1))
      }
    }else{
      dist_var <- c(dist_var,"RandomVariableForImputation")
      weights <- c(weights,min(weights)/(sum(weights)+1))
    }
  }

  dist_single <- function(don_dist_var,imp_dist_var,numericalX,factorsX,ordersX,mixedX,levOrdersX,
      don_index,imp_index,weightsx,k,mixed.constant,provideMins=TRUE){
    #gd <- distance(don_dist_var,imp_dist_var,weights=weightsx)
    if(is.null(mixed.constant))
      mixed.constant <- rep(0,length(mixedX))
    if(provideMins){
      gd <- gowerD(don_dist_var,imp_dist_var,weights=weightsx,numericalX,
          factorsX,ordersX,mixedX,levOrdersX,mixed.constant=mixed.constant,returnIndex=TRUE,
          nMin=as.integer(k),returnMin=TRUE);
      colnames(gd$mins) <- imp_index
      erg2 <- as.matrix(gd$mins)
    }else{
      gd <- gowerD(don_dist_var,imp_dist_var,weights=weightsx,numericalX,
          factorsX,ordersX,mixedX,levOrdersX,mixed.constant=mixed.constant,returnIndex=TRUE,
          nMin=as.integer(k));
      erg2 <- NA
    }
    colnames(gd$ind) <- imp_index
    gd$ind[,] <- don_index[gd$ind]
    erg <- as.matrix(gd$ind)
    
    if(k==1){
      erg <- t(erg)
      erg2 <- t(erg2)
    }
    list(erg,erg2)
  }
  for(j in 1:nvar){
    
    if(any(indexNA2s[,variable[j]])){
      if(is.list(dist_var)){
        if(!is.list(weights))
          stop("if dist_var is a list weights must be a list")
        dist_varx <- dist_var[[j]]
        weightsx <- weights[[j]]
      }else{
        dist_varx <- dist_var[dist_var!=variable[j]]
        weightsx <- weights[dist_var%in%dist_varx]
      }
      if(!is.null(donorcond)){
        cmd <- paste0("TF <- data[,sapply(.SD,function(x)!is.na(x)&x",donorcond[[j]],"),.SDcols=variable[j]][,1]")
        eval(parse(text=cmd))
        don_dist_var <- data[TF,dist_varx,with=FALSE]
        don_index <- INDEX[TF]
      }else{
        TF <- data[,sapply(.SD,function(x)!is.na(x)),.SDcols=variable[j]][,1]
        don_dist_var <- data[TF,dist_varx,with=FALSE]
        don_index <- INDEX[TF]
      }
      TF_imp <- indexNA2s[,variable[j]]
      imp_dist_var <- data[TF_imp,dist_varx,with=FALSE]
      imp_index <- INDEX[TF_imp]
      
      #
      if(!useImputedDist&&any(dist_varx%in%variable)){
        for(dvar in dist_varx[dist_varx%in%variable]){
          ## setting the value for original missing variables to NA
          don_dist_var[indexNA2s[TF,dvar],c(dvar):=NA]#,with=FALSE]
          imp_dist_var[indexNA2s[TF_imp,dvar],c(dvar):=NA]#,with=FALSE]
        }
      }
      
      numericalX <-numerical[numerical%in%dist_varx]
      factorsX <-factors[factors%in%dist_varx]
      ordersX <-orders[orders%in%dist_varx]
      levOrdersX <- levOrders[orders%in%dist_varx]
      #print(levOrdersX)
      mixedX <-mixed[mixed%in%dist_varx]
      #dist_single provide the rows of the k nearest neighbours and the corresponding distances
      mindi <- dist_single(as.data.frame(don_dist_var),as.data.frame(imp_dist_var),numericalX,factorsX,ordersX,mixedX,levOrdersX,
          don_index,imp_index,weightsx,k,mixed.constant,provideMins=weightDist)
      getI <- function(x)data[x,variable[j],with=FALSE]
      if(trace)
        cat(sum(indexNA2s[,variable[j]]),"items of","variable:",variable[j]," imputed\n")
      #Fetching the actual values of the kNNs for the indices provided by dist_single
      getI <- function(x)data[x,variable[j],with=FALSE]
      kNNs <- do.call("cbind",apply(mindi[[1]],2,getI)) 
      if(k==1){
        kNNs <- t(kNNs)
      }
      
      if(weightDist&k>1){
        if(length(factors)<length(variable)&!"weights"%in%names(as.list(args(numFun)))){
          warning("There is no explicit 'weights' argument in your numeric aggregation function.")
        }
        if(length(factors)>0&&!"weights"%in%names(as.list(args(catFun)))){
          warning("There is no explicit 'weights' argument in your categorical aggregation function.")
        }
        #1-dist because dist is between 0 and 1
        mindi[[2]] <- 1-mindi[[2]]
        ### warning if there is no argument named weights
        if(variable[j]%in%factors)
          data[indexNA2s[,variable[j]],variable[j]] <- sapply(1:ncol(kNNs),function(x)do.call("catFun",list(unlist(kNNs[,x,with=FALSE]),mindi[[2]][,x])))
        else if(is.integer(data[,variable[j]])){
          data[indexNA2s[,variable[j]],variable[j]] <- round(sapply(1:ncol(kNNs),function(x)do.call("numFun",list(unlist(kNNs[,x,with=FALSE]),mindi[[2]][,x]))))
        }else
          data[indexNA2s[,variable[j]],variable[j]] <- sapply(1:ncol(kNNs),function(x)do.call("numFun",list(unlist(kNNs[,x,with=FALSE]),mindi[[2]][,x])))
      }else{
        if(variable[j]%in%factors)
          data[indexNA2s[,variable[j]],variable[j]] <- apply(kNNs,2,catFun)
        else if(is.integer(data[,variable[j]])){
          data[indexNA2s[,variable[j]],variable[j]] <- round(apply(kNNs,2,numFun))
        }else
          data[indexNA2s[,variable[j]],variable[j]] <- apply(kNNs,2,numFun)  
      }
      
    }else{
      if(trace)
        cat("0 items of","variable:",variable[j]," imputed\n")
    }
    
  }
  if(trace){
    print(difftime(Sys.time(),startTime))
  }
  if(addRandom){
    RandomVariableForImputation <- NULL # for satisfying CRAN check
    data <- data[,RandomVariableForImputation:=NULL]
  }
  if(!is.null(features_added)){
    data[,c(features_added):=NULL]
  }
  data
}
