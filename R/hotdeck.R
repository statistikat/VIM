####Classical hotdeck methods
#Author: Alexander Kowarik, Statistics Austria
## Sequential hot(cold)deck
## Random (within domain) hot(cold)deck
## Cold deck is not implemented yet
#data - data.frame of the data with missing
#variable - vector of variablesnames to be imputed
#ord_var - list of vectors of variablesnames to be used to order the dataset
#domain_var - vector of variablesnames to be used as domains
#makeNA - vector of values which should be imputed too e.g. 8,9 or 98,99 in SPSS-data sets
#NAcond - list of conditions for each variable to create NAs there (not yet implemented)
#donorcond - list of conditions for a donor e.g. "<=10000"
#TODO: Donors from cold deck



#' Hot-Deck Imputation
#' 
#' Implementation of the popular Sequential, Random (within a domain) hot-deck
#' algorithm for imputation.
#' 
#' 
#' @param data data.frame or matrix
#' @param variable variables where missing values should be imputed
#' @param ord_var variables for sorting the data set before imputation
#' @param domain_var variables for building domains and impute within these
#' domains
#' @param makeNA list of length equal to the number of variables, with values, that should be converted to NA for each variable
#' @param NAcond list of length equal to the number of variables, with a condition for imputing a NA
#' @param impNA TRUE/FALSE whether NA should be imputed
#' @param donorcond list of length equal to the number of variables, with a donorcond condition for the donors e.g. ">5"
#' @param imp_var TRUE/FALSE if a TRUE/FALSE variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix for the TRUE/FALSE variables showing the imputation
#' status
#' @return the imputed data set.
#' @author Alexander Kowarik
#' @keywords manip
#' @examples
#' 
#' data(sleep)
#' sleepI <- hotdeck(sleep)
#' sleepI2 <- hotdeck(sleep,ord_var="BodyWgt",domain_var="Pred")
#' 
#' set.seed(132)
#' nRows <- 1e3
#' # Generate a data set with nRows rows and several variables
#' x<-data.frame(x=rnorm(nRows),y=rnorm(nRows),z=sample(LETTERS,nRows,replace=TRUE),
#'     d1=sample(LETTERS[1:3],nRows,replace=TRUE),d2=sample(LETTERS[1:2],nRows,replace=TRUE),
#'     o1=rnorm(nRows),o2=rnorm(nRows),o3=rnorm(100))
#' origX <- x
#' x[sample(1:nRows,nRows/10),1] <- NA
#' x[sample(1:nRows,nRows/10),2] <- NA
#' x[sample(1:nRows,nRows/10),3] <- NA
#' x[sample(1:nRows,nRows/10),4] <- NA
#' xImp <- hotdeck(x,ord_var = c("o1","o2","o3"),domain_var="d2")
#' 
#' 
#' @export hotdeck
#' @S3method hotdeck data.frame
#' @S3method hotdeck survey.design
#' @S3method hotdeck default
hotdeck <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  UseMethod("hotdeck", data)
}

hotdeck.data.frame <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  hotdeck_work(data, variable, ord_var, domain_var, makeNA, NAcond, impNA, donorcond,
      imp_var, imp_suffix)
}

hotdeck.survey.design <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  data$variables <- hotdeck_work(data$variables, variable, ord_var, domain_var, makeNA, NAcond, impNA, donorcond,
      imp_var, imp_suffix)
  data$call <- sys.call(-1)
  data
}

hotdeck.default <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  hotdeck_work(as.data.frame(data), variable, ord_var, domain_var, makeNA, NAcond, impNA, donorcond,
      imp_var, imp_suffix)
}

#hotdeck_work <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
#    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
#    imp_var=TRUE,imp_suffix="imp"){
#  #basic checks
#  if(is.null(variable)){
#    variable <- colnames(data)
#    if(!is.null(ord_var))
#      variable <- variable[-match(ord_var,variable)]
#    if(!is.null(domain_var))
#      variable <- variable[-match(domain_var,variable)]    
#  }
#  nvar <- length(variable)
#  ndat <- nrow(data)
#  data$UNIQUEIDx <- 1:ndat
#  if(!is.data.frame(data)&&!is.matrix(data))
#    stop("supplied data should be a dataframe or matrix")
#  if(!impNA&&is.null(NAcond)){
#    warning("Nothing to impute, because NA were not imputed and no NA condition is given")
#    invisible(data)
#  }
#  if(!is.null(domain_var))
#    if(any(is.na(data[,domain_var])))
#      stop("Domain variables should not include missings, otherwise you have to impute them first")
#  if(is.matrix(data))
#    data <- as.data.frame(data)
#  
#  #impNA==FALSE -> NAs should remain NAs (Routing NAs!?)
#  if(!impNA)
#    data[is.na(data)] <- "THISISanNASTRINGthatshouldnotbeimputedbytheroutine"
#  
#  if(!is.null(makeNA)){
#    if(length(makeNA)!=nvar)
#      stop("The list 'variable' must have the same length as the 'makeNA' list")
#    else{
#      for(i in 1:nvar){
#        data[data[,variable[i]]%in%makeNA[[i]],variable[i]] <- NA 
#      }
#    }
#  }
#  if(!any(is.na(data))){
#    warning("Nothing to impute, because no NA are present (also after using makeNA)")
#    invisible(data)
#  }
#  if(imp_var){
#    imp_vars <- paste(variable,"_",imp_suffix,sep="")
#    index_imp_vars <- which(!imp_vars%in%colnames(data))
#    index_imp_vars2 <- which(imp_vars%in%colnames(data))
#    if(length(index_imp_vars)>0){
#      data[,imp_vars[index_imp_vars]] <- FALSE
#      #for(i in index_imp_vars){
#      #data[indexNA2s[,variable[i]],imp_vars[i]] <- TRUE
#      #if(!any(indexNA2s[,variable[i]]))
#      #  data<-data[,-which(names(data)==paste(variable[i],"_",imp_suffix,sep=""))]
#      #}
#    }
#    if(length(index_imp_vars2)>0){
#      warning(paste("The following TRUE/FALSE imputation status variables will be updated:",
#              paste(imp_vars[index_imp_vars2],collapse=" , ")))
#      for(i in index_imp_vars2)
#        data[,imp_vars[i]] <- as.logical(data[,imp_vars[i]])
#    }
#  }
#  ##Split the data into domains
#  if(!is.null(domain_var)){
#    newindex <- apply(data[,domain_var,drop=FALSE], 1, paste, collapse=":")
#    datas <- split(data,newindex)      
#  }else
#    datas <- list(data)
#  for(j in 1:nvar){#impute each variable
#    for(d in 1:length(datas)){#impute in each domain
#      datax <- datas[[d]]
#      ndat <- nrow(datax)
#      ##Order the data set
#      if(!is.null(ord_var)){
#        if(is.list(ord_var))
#          ord_var1 <- ord_var[[j]]
#        else
#          ord_var1 <- ord_var
#        cmd <- "ord <- order("
#        for(i in 1:length(ord_var1)){
#          cmd <- paste(cmd,"datax[,ord_var1[",i,"]],",sep="")
#        }
#        cmd <- paste(cmd,"na.last=TRUE)",sep="")
#        eval(parse(text=cmd))
#      }else{
#        ord <- sample(1:ndat,ndat)
#      }
#      datax <- datax[ord,]
#      donorISgood <- FALSE
#      if(nrow(datax)>1){
#        while(!donorISgood){
#          donor <- sample(na.omit(datax[,variable[[j]]]),1)
#          if(!is.null(donorcond)){
#            if(is.list(donorcond))
#              donorcondX <- donorcond[[j]]
#            else
#              donorcondX <- donorcond
#            TFdon <- vector()
#            cmd <- paste("TFdon <- donor",donorcondX,sep="")
#            eval(parse(text=cmd))
#            if(TFdon)
#              donorISgood <- TRUE
#          }else
#            donorISgood <- TRUE
#        }
#      }
#      for(i in 1:ndat){
#        if(is.na(datax[i,variable[j]])){
#          datax[i,variable[j]] <- donor
#        }else{
#          donorX <- datax[i,variable[j]] 
#          if(!is.null(donorcond)){
#            if(is.list(donorcond))
#              donorcondX <- donorcond[[j]]
#            else
#              donorcondX <- donorcond
#            cmd <- paste("TFdon <- donor",donorcondX,sep="")
#          }else
#            cmd <- paste("TFdon <- TRUE",sep="")
#          eval(parse(text=cmd))
#          if(TFdon)
#            donor <- donorX
#        }
#      }        
#      datax -> datas[[d]] 
#    }
#  }
#  
#  datax <- data.frame()
#  for(d in 1:length(datas)){
#    datax <- rbind(datax,datas[[d]])
#  }
#  datax <- datax[order(datax$UNIQUEIDx),-which(names(datax)=="UNIQUEIDx")] 
#  if(any(is.na(datax)))
#    warning("Some NAs remained, maybe due to a too restrictive domain building!?")
#  if(!impNA)
#    datax[datax=="THISISanNASTRINGthatshouldnotbeimputedbytheroutine"] <- NA
#  datax
#}

hotdeck_work <- function(x , variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp"
    ){
  OriginalSortingVariable <- impvar <- NULL #empty init
  if(is.null(variable)){
    variable <- colnames(x)
    variable<-variable[!variable%in%c(ord_var,domain_var)]
  }
  if(!is.null(makeNA)){
    if(!is.list(makeNA)||!length(makeNA)==length(variable))
      stop("makeNA is not defined correctly")
  }
  classx <- class(x)
  VariableSorting <- colnames(x)
  x$OriginalSortingVariable <- 1:nrow(x)
  x <- data.table(x)
  if(is.null(variable)){
    variable  <- colnames(x)[apply(is.na(x),2,any)]
  }
  if(!is.null(NAcond))
    warning("NAcond is not implemented yet and will be ignored.")
  ## xx should be a data.table and ord_var the name of variables to sort
  imputeHD <- function(xx,variableX,varTypeX,imp_varX,imp_suffixX,
      impNAX,makeNAX){
    #xxx <<- copy(xx)
    #variableX <<- variableX
    #varTypeX <<- varTypeX
    #imp_varX <<- imp_varX
    #imp_suffixX <<- imp_suffixX
    OriginalSortingVariable <- weirdandlongname <- UniqueIdForImputation <- NULL#empty init
    J <- function()NULL#empty init
    xx$UniqueIdForImputation <- 1:nrow(xx) 
    for(v in variableX){
      
      if(!impNAX){
        setkeyv(xx,v)
        if(is.null(makeNAX))
          stop("If impNA=FALSE a list of values to be imputed must be provided.")
        ## NAs should not be imputed
        if(varTypeX[v]%in%c("numeric","integer")){
          NAs <- xx[J(NA_real_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
        }else{
          NAs <- xx[J(NA_character_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
        }
        xxna <- xx[NAs]
        xx <- xx[-NAs]
        xx$UniqueIdForImputation <- 1:nrow(xx)
      }
      
      if(!is.null(makeNAX)){
       # eval(parse(text="xx[xx>1]"))
        setnames(xx,v,"weirdandlongname")
        xx[weirdandlongname%in%makeNAX[[match(v,variableX)]],weirdandlongname:=NA]
        setnames(xx,"weirdandlongname",v)
      }
      setkeyv(xx,v)
      if(varTypeX[v]%in%c("numeric","integer")){
        impPart <- xx[J(NA_real_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
      }else{
        impPart <- xx[J(NA_character_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
      }
      
      if(length(impPart)>0){
        if(imp_varX){
          impvarname <- paste(v,"_",imp_suffixX,sep="")
          xx[UniqueIdForImputation%in%impPart,impvarname:=TRUE,with=FALSE]
        }
        impDon <- impPart-1
        impDon[impDon<1] <- impPart[impDon<1]+1
        setkey(xx,UniqueIdForImputation)
        Don <- data.frame(xx[impDon,v,with=FALSE])[,1]
        TFindex <- is.na(Don)
        TF <- any(TFindex)
        if(TF){
          add <- 2
          while(TF){
            impDon[TFindex] <- impPart[TFindex]-add
            impDon[TFindex][impDon[TFindex]<1] <- impPart[TFindex][impDon[TFindex]<1]+add
            impDon2 <- impDon[TFindex]
            Don[TFindex] <- data.frame(xx[impDon2,v,with=FALSE])[,1]
            TFindex[TFindex] <- is.na(Don[TFindex])
            TF <- any(TFindex)
            if(add>50){
              TF <- FALSE
              Don[TFindex]<-1
            }
            add <- add +1
          }
        }
        xx[impPart,v] <- Don
      }
      if(!impNAX)
        xx <- rbindlist(list(xx,xxna))
    }
    xx[,UniqueIdForImputation:=NULL]
    setkey(xx,OriginalSortingVariable)
    return(xx)
  }
  varType <- sapply(x,class)[variable]
  if(imp_var){
    for(v in variable){
      x[,impvar:=FALSE]
      impvarname <- paste(v,"_",imp_suffix,sep="")
      setnames(x,"impvar",impvarname)
      VariableSorting <- c(VariableSorting,impvarname)
    }
  }
  # If no ord_var is defined, a random ordered will be used
  if(is.null(ord_var)){
    RandomVariableForImputationWithHotdeck <- NULL # Init for CRAN check
    x[,RandomVariableForImputationWithHotdeck:=runif(nrow(x))]
    ord_var <- "RandomVariableForImputationWithHotdeck"
  }
  setkeyv(x,ord_var)
  # if no domain_var is defined, the imputeHD function is automatically called on the
  # whole data set
  x <- x[,imputeHD(.SD,variableX=variable,varTypeX=varType,
    imp_varX=imp_var,imp_suffixX=imp_suffix,impNAX=impNA,makeNAX=makeNA), by = domain_var]
  if(ord_var=="RandomVariableForImputationWithHotdeck"){
    x[,RandomVariableForImputationWithHotdeck:=NULL]
    ord_var <- NULL
  }
  
  setkey(x,OriginalSortingVariable)
  x[,OriginalSortingVariable:=NULL]
  if(all(classx!="data.table"))
    return(data.frame(x)[,VariableSorting])
  return(x[,VariableSorting,with=FALSE])
}
#require(data.table)
#setwd("/Users/alex")
#Rprof("profile1.out")
###TEST
#set.seed(132)
#nRows <- 1e6
#x<-data.frame(x=rnorm(nRows),y=rnorm(nRows),z=sample(LETTERS,nRows,rep=T),
#    d1=sample(LETTERS[1:3],nRows,rep=T),d2=sample(LETTERS[1:2],nRows,rep=T),o1=rnorm(nRows),o2=rnorm(nRows),o3=rnorm(100))
#origX <- x
#x[sample(1:nRows,nRows/10),1] <- NA
#x[sample(1:nRows,nRows/10),2] <- NA
#x[sample(1:nRows,nRows/10),3] <- NA
#x[sample(1:nRows,nRows/10),4] <- NA
##
#xImp <- hotdeck_work2(x,ord_var = c("o1","o2","o3"),domain_var="d2")
#Rprof(NULL)
#summaryRprof("profile1.out")
#xImp1 <- hotdeck(x,ord_var = c("o1","o2","o3"),domain_var="d2")
#identical(xImp,xImp1)
#
#
#for(v in colnames(xImp)){
#  print(v)
#  print(identical(xImp[,v],xImp1[,v]))
#}
#  
#  
#  
#require(microbenchmark)
#res <- microbenchmark(xImp <- hd(x,ord_var = c("o1","o2","o3"),domain_var="d2"),times=10)


