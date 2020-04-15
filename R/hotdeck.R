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
#' @note If the sequential hotdeck does not lead to a suitable,
#' a random donor in the group will be used.
#' @references A. Kowarik, M. Templ (2016) Imputation with
#' R package VIM.  *Journal of
#' Statistical Software*, 74(7), 1-16.
#' @keywords manip
#' @family imputation methods
#' @examples
#' 
#' data(sleep)
#' sleepI <- hotdeck(sleep)
#' sleepI2 <- hotdeck(sleep,ord_var="BodyWgt",domain_var="Pred")
#' 
#' # Usage of donorcond in a simple example
#' sleepI3 <- hotdeck(sleep,variable=c("NonD","Dream","Sleep","Span","Gest"),
#' ord_var="BodyWgt",domain_var="Pred",
#' donorcond = list(">4","<17",">1.5","%between%c(8,13)",">5"))
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
hotdeck <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  UseMethod("hotdeck", data)
}

#' @rdname hotdeck
#' @export

hotdeck.data.frame <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  hotdeck_work(data, variable, ord_var, domain_var, makeNA, NAcond, impNA, donorcond,
      imp_var, imp_suffix)
}

#' @rdname hotdeck
#' @export

hotdeck.survey.design <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  data$variables <- hotdeck_work(data$variables, variable, ord_var, domain_var, makeNA, NAcond, impNA, donorcond,
      imp_var, imp_suffix)
  data$call <- sys.call(-1)
  data
}

#' @rdname hotdeck
#' @export

hotdeck.default <- function(data, variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp") {
  hotdeck_work(as.data.frame(data), variable, ord_var, domain_var, makeNA, NAcond, impNA, donorcond,
      imp_var, imp_suffix)
}

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
      stop("makeNA is not defined correctly. \n It should be a list of length equal to the length of the argument 'variable'.")
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
    prevKey <- key(xx)
    #xxb <- copy(xx)
    #xx <- copy(xxb)
    for(v in variableX){
      
      if(!impNAX){
        setkeyv(xx,v)
        if(is.null(makeNAX))
          stop("If impNA=FALSE a list of values to be imputed must be provided.")
        ## NAs should not be imputed
        if(varTypeX[v]%in%c("numeric","integer")){
          NAs <- xx[J(NA_real_),.I,nomatch=FALSE]# get the Index of the NAS
        }else{
          NAs <- xx[J(NA_character_),.I,nomatch=FALSE]# get the Index of the NAS
        }
        #NAs hold the index of observations with NAs in the current variable
        if(length(NAs)>0){
          xxna <- xx[NAs] # move observation to a temp data set
          xx <- xx[-NAs]  # just keep the non NA obs
        }else{#if no NA xx is unchanged and xxna is just an empty data.table
          xxna <- data.table()
        }
        setkeyv(xx,prevKey)
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
        setnames(xx,v,"VariableWhichIsCurrentlyImputed")
        impPart <- xx[J(NA_real_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
        setnames(xx,"VariableWhichIsCurrentlyImputed",v)
      }else{
        setnames(xx,v,"VariableWhichIsCurrentlyImputed")
        impPart <- xx[J(NA_character_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
        setnames(xx,"VariableWhichIsCurrentlyImputed",v)
      }
      
      if((length(impPart)>0)&&(length(impPart)<nrow(xx))){
        if(imp_varX){
          impvarname <- paste(v,"_",imp_suffixX,sep="")
          xx[UniqueIdForImputation%in%impPart,c(impvarname):=TRUE]
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
              # remaining missing values will be set to a random value from the group
              
              Don[TFindex] <- Don[!TFindex][sample(sum(!TFindex),1)]
              if(!identical(ord_var, "RandomVariableForImputationWithHotdeck")){
                warning(paste("For variable",v,"the ordering is ignored for at least one imputation."))
              }
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
  classWithoutLabelled <- function(x){
  cl <- class(x)
  return(cl[cl!="labelled"])
  }
  varType <- sapply(x,classWithoutLabelled)[variable]
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
    nrowXforRunif <- nrow(x)
    x[,RandomVariableForImputationWithHotdeck:=runif(nrowXforRunif)]
    ord_var <- "RandomVariableForImputationWithHotdeck"
  }
  setkeyv(x,ord_var)
  # if no domain_var is defined, the imputeHD function is automatically called on the
  # whole data set
  x <- x[,imputeHD(.SD,variableX=variable,varTypeX=varType,
    imp_varX=imp_var,imp_suffixX=imp_suffix,impNAX=impNA,makeNAX=makeNA), by = domain_var]
  if(any(ord_var=="RandomVariableForImputationWithHotdeck")){
    x[,RandomVariableForImputationWithHotdeck:=NULL]
    ord_var <- NULL
  }
  
  setkey(x,OriginalSortingVariable)
  x[,OriginalSortingVariable:=NULL]
  if(all(classx!="data.table"))
    return(as.data.frame(x)[,VariableSorting,drop=FALSE])
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


