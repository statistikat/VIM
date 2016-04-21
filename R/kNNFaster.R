####Hotdeck in context of kNN-k Nearest Neighbour Imputation
#Author: Alexander Kowarik, Statistics Austria
## (k)NN-Imputation
## (k)NN-Imputation, donors from cold deck
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
#' factor ordered and semi-continous variables. The function which.minN is used
#' by kNN.
#' 
#' @aliases kNN sampleCat maxCat gowerD which.minN
#' @param data data.frame or matrix
#' @param variable variables where missing values should be imputed
#' @param metric metric to be used for calculating the distances between
#' @param k number of Nearest Neighbours used
#' @param dist_var names or variables to be used for distance calculation
#' @param weights weights for the variables for distance calculation
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
#' @keywords manip
#' @examples
#' 
#' data(sleep)
#' kNN(sleep)
#' kNN(sleep, numFun = weightedMean, weightDist=TRUE)
#' 
#' @export kNN
#' @export sampleCat
#' @export maxCat
#' @export gowerD
#' @export which.minN
#' @S3method kNN data.frame
#' @S3method kNN survey.design
#' @S3method kNN default
kNN <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                numFun = median, catFun=maxCat,
                makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                imp_var=TRUE,imp_suffix="imp",addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  UseMethod("kNN", data)
}

kNN.data.frame <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                           numFun = median, catFun=maxCat,
                           makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                           imp_var=TRUE,imp_suffix="imp",addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  kNN_work(data, variable, metric, k, dist_var,weights, numFun, catFun,
           makeNA, NAcond, impNA, donorcond, mixed, mixed.constant, trace,
           imp_var, imp_suffix, addRandom,useImputedDist,weightDist)
}

kNN.survey.design <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                              numFun = median, catFun=maxCat,
                              makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                              imp_var=TRUE,imp_suffix="imp",addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  data$variables <- kNN_work(data$variables, variable, metric, k, dist_var,weights, numFun, catFun,
           makeNA, NAcond, impNA, donorcond, mixed, mixed.constant, trace,
           imp_var, imp_suffix, addRandom,useImputedDist,weightDist)
  data$call <- sys.call(-1)
  data
}

kNN.default <- function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
                        numFun = median, catFun=maxCat,
                        makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
                        imp_var=TRUE,imp_suffix="imp",addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE) {
  kNN_work(as.data.frame(data), variable, metric, k, dist_var,weights, numFun, catFun,
           makeNA, NAcond, impNA, donorcond, mixed, mixed.constant, trace,
           imp_var, imp_suffix, addRandom,useImputedDist,weightDist)
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
which.minN <- function(x,n){
  n <- min(n,length(x))
  out <- vector()
  for(i in 1:n){
    out[i] <- names(x)[which.min(x)]
    x[which.min(x)] <- Inf
  }
  as.numeric(out)
}
minN <- function(x,n){
  n <- min(n,length(x))
  out <- vector()
  for(i in 1:n){
    out[i] <- min(x)
    x[which.min(x)] <- Inf
  }
  as.numeric(out)
}
kNN_work <-
    function(data, variable=colnames(data), metric=NULL, k=5, dist_var=colnames(data),weights=NULL,
        numFun = median, catFun=maxCat,
        makeNA=NULL,NAcond=NULL, impNA=TRUE, donorcond=NULL,mixed=vector(),mixed.constant=NULL,trace=FALSE,
        imp_var=TRUE,imp_suffix="imp",addRandom=FALSE,useImputedDist=TRUE,weightDist=FALSE){
  #basic checks
  if(!is.null(mixed.constant)){
    if(length(mixed.constant)!=length(mixed))
      stop("length 'mixed.constant' and length 'mixed' differs")
  }
  startTime <- Sys.time()
  nvar <- length(variable)
  ndat <- nrow(data)
  for(v in variable){
    if(all(is.na(data[,v]))){
      warning(paste("All observations of",v,"are missing, therefore the variable will not be imputed!\n"))
      variable <- variable[variable!=v]  
    }
  }
  if(!is.data.frame(data)&&!is.matrix(data))
    stop("supplied data should be a dataframe or matrix")
  
  if(is.matrix(data))
    data <- as.data.frame(data)
  #impNA==FALSE -> NAs should remain NAs (Routing NAs!?)
  indexNAs <- is.na(data)
  if(!is.null(makeNA)){
    if(length(makeNA)!=nvar)
      stop("The vector 'variable' must have the same length as the 'makeNA' list")
    else{
      for(i in 1:nvar){
        data[data[,variable[i]]%in%makeNA[[i]],variable[i]] <- NA 
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
      data[,imp_vars[index_imp_vars]] <- FALSE
      for(i in index_imp_vars){
        data[indexNA2s[,variable[i]],imp_vars[i]] <- TRUE
          #if(!any(indexNA2s[,variable[i]]))
            #data<-data[,-which(names(data)==paste(variable[i],"_",imp_suffix,sep=""))]
      }
    }
    if(length(index_imp_vars2)>0){
      warning(paste("The following TRUE/FALSE imputation status variables will be updated:",
              paste(imp_vars[index_imp_vars2],collapse=" , ")))
      for(i in index_imp_vars2)
        data[,imp_vars[i]] <- as.logical(data[,imp_vars[i]])
    }
  }
  orders <- vector()
  for(i in 1:ncol(data)){
    orders <- c(orders,is.ordered(data[,i]))
  }
  orders <- colnames(data)[orders]
  levOrders <- vector()
  if(length(orders)>0){
    for(i in 1:length(orders)){
      levOrders[i] <- length(levels(data[,orders[i]]))
    }
  }
  factors <- vector()
  for(i in 1:ncol(data)){
    factors <- c(factors,is.factor(data[,i])|is.character(data[,i]))
  }
  factors <- colnames(data)[factors]
  factors <- factors[!factors%in%orders]
  numerical <- vector()
  for(i in 1:ncol(data)){
    numerical <- c(numerical,is.numeric(data[,i])|is.integer(data[,i]))
  }
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
  if(is.null(weights)){
    weights <- rep(1,length(dist_var))
  }else if(length(weights)!=length(dist_var)){
    stop("length of weights must be equal the number of distance variables")
  }
  if(addRandom){
    numerical <- c(numerical, "RandomVariableForImputation")
    data[,"RandomVariableForImputation"] <- rnorm(ndat)
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
    gd <- gowerD(don_dist_var,imp_dist_var,weights=weightsx,numericalX,factorsX,ordersX,mixedX,levOrdersX,mixed.constant=mixed.constant);
    rownames(gd) <- don_index
    colnames(gd) <- imp_index
    which.minNk <- function(x)1
    minNk <- function(x)1
    cmd <- paste("which.minNk <- function(x)which.minN(x,",k,")",sep="")
    eval(parse(text=cmd))
    mindi <- apply(gd,2,which.minNk)
    erg <- as.matrix(mindi)
    if(provideMins){
      cmd <- paste("minNk <- function(x)minN(x,",k,")",sep="")
      eval(parse(text=cmd))
      mindist <- apply(gd,2,minNk)
      erg2 <- as.matrix(mindist)      
    }else{
      erg2 <- NA
    }
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
        cmd <- paste("TF <- !is.na(data[,variable[j]])&data[,variable[j]]",donorcond[[j]],sep="")
        eval(parse(text=cmd))
        don_dist_var <- data[TF,dist_varx,drop=FALSE]
        don_index <- INDEX[TF]
      }else{
        TF <- !is.na(data[,variable[j]])
        don_dist_var <- data[TF,dist_varx,drop=FALSE]
        don_index <- INDEX[TF]
      }
      TF_imp <- indexNA2s[,variable[j]]
      imp_dist_var <- data[TF_imp,dist_varx,drop=FALSE]
      imp_index <- INDEX[TF_imp]
      
      #
      if(!useImputedDist&&any(dist_varx%in%variable)){
        for(dvar in dist_varx[dist_varx%in%variable]){
          ## setting the value for original missing variables to NA
          don_dist_var[indexNA2s[TF,dvar],dvar] <- NA
          imp_dist_var[indexNA2s[TF_imp,dvar],dvar] <- NA
        }
      }
      
      numericalX <-numerical[numerical%in%dist_varx]
      factorsX <-factors[factors%in%dist_varx]
      ordersX <-orders[orders%in%dist_varx]
      levOrdersX <- levOrders[orders%in%dist_varx]
      #print(levOrdersX)
      mixedX <-mixed[mixed%in%dist_varx]
      #dist_single provide the rows of the k nearest neighbours and the corresponding distances
      mindi <- dist_single(don_dist_var,imp_dist_var,numericalX,factorsX,ordersX,mixedX,levOrdersX,
          don_index,imp_index,weightsx,k,mixed.constant,provideMins=weightDist)
      getI <- function(x)data[x,variable[j]]
      if(trace)
        cat(sum(indexNA2s[,variable[j]]),"items of","variable:",variable[j]," imputed\n")
      #Fetching the actual values of the kNNs for the indices provided by dist_single
      kNNs <- as.matrix(apply(mindi[[1]],2,getI))
      if(k==1){
        kNNs <- matrix(kNNs,nrow=1)
      }
      
      if(weightDist){
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
          data[indexNA2s[,variable[j]],variable[j]] <- sapply(1:ncol(kNNs),function(x)do.call("catFun",list(kNNs[,x],mindi[[2]][,x])))
        else if(is.integer(data[,variable[j]])){
          data[indexNA2s[,variable[j]],variable[j]] <- round(sapply(1:ncol(kNNs),function(x)do.call("numFun",list(kNNs[,x],mindi[[2]][,x]))))
        }else
          data[indexNA2s[,variable[j]],variable[j]] <- sapply(1:ncol(kNNs),function(x)do.call("numFun",list(kNNs[,x],mindi[[2]][,x])))
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
  print(difftime(Sys.time(),startTime))  
  if(addRandom)
    data <- data[,-which(names(data)=="RandomVariableForImputation")]
  data
}
