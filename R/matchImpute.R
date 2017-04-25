#' Fast matching/imputation based on categorical variable
#' 
#' Suitable donors are searched based on matching of the categorical variables.
#' The variables are dropped in reversed order, so that the last element of
#' 'match_var' is dropped first and the first element of the vector is dropped last.
#' 
#' The method works by sampling values from the suitable donors.
#' 
#' @param data data.frame, data.table, survey object or matrix
#' @param variable variables to be imputed
#' @param match_var variables used for matching
#' @param imp_var TRUE/FALSE if a TRUE/FALSE variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix for the TRUE/FALSE variables showing the imputation
#' status
#' @return the imputed data set.
#' @author Johannes Gussenbauer, Alexander Kowarik
#' @seealso \code{\link[VIM]{hotdeck}}
#' @keywords manip
#' @examples
#' 
#' data(sleep,package="VIM")
#' imp_data <- matchImpute(sleep,variable=c("NonD","Dream","Sleep","Span","Gest"),
#'   match_var=c("Exp","Danger"))
#' 
#' data(testdata,package="VIM")
#' imp_testdata1 <- matchImpute(testdata$wna,match_var=c("c1","c2","b1","b2"))
#'
#' dt <- data.table(testdata$wna)
#' imp_testdata2 <- matchImpute(dt,match_var=c("c1","c2","b1","b2"))
#' 
#' 
#' @export matchImpute
#' @S3method matchImpute data.frame
#' @S3method matchImpute survey.design
#' @S3method matchImpute default
#

# working function
primitive.impute <- function(x){
  x.na <- is.na(x)
  if(all(!x.na)|all(x.na)){
    return(x)
  }
  # if(all(x.na)){
  #   warning("no donors present in subsample")
  #   return(x)
  # }
  n.imp <- sum(x.na)
  if(length(x[!x.na])>1){
    x[x.na] <- sample(x[!x.na],n.imp,replace=TRUE)
  }else{
    x[x.na] <- x[!x.na]
  }
  return(x)
}
matchImpute <- function(data,variable=colnames(data)[!colnames(data)%in%match_var],match_var, imp_var=TRUE,
    imp_suffix="imp") {
  UseMethod("matchImpute", data)
}

matchImpute.data.frame <- function(data,variable=colnames(data)[!colnames(data)%in%match_var],match_var, imp_var=TRUE,
    imp_suffix="imp") {
  as.data.frame(matchImpute.default(data.table(data), variable, match_var, imp_var, imp_suffix))
}

matchImpute.data.table <- function(data,variable=colnames(data)[!colnames(data)%in%match_var],match_var, imp_var=TRUE,
    imp_suffix="imp") {
  matchImpute.default(copy(data), variable, match_var, imp_var, imp_suffix)
}
# main function
# imp_var can only be a single collumn (yet)
matchImpute.default <- function(data,variable=colnames(data)[!colnames(data)%in%match_var],match_var, imp_var=TRUE,
    imp_suffix="imp"){
  na_present <- data[,sum(sapply(lapply(.SD,is.na),sum)),.SDcols=variable]
  
  if(imp_var){
    data[,paste(variable,imp_suffix,sep="_"):=lapply(.SD,is.na),.SDcols=variable]
  }
  
  count_missings <- matrix(c(1,na_present),nrow=1,ncol=2)
  colnames(count_missings) <- c("STEP","NA_PRESENT")
  
  i <- length(match_var)
  j <- 1
  while(na_present>0){
    
    if(i>0){
      data[,c(variable):=lapply(.SD,primitive.impute),by=c(match_var[1:i]),.SDcols=variable]
    }else{
      data[,c(variable):=lapply(.SD,primitive.impute),.SDcols=variable]
    }
    na_present <- data[,sum(sapply(lapply(.SD,is.na),sum)),.SDcols=variable]
    i <- i-1
    
    j <- j+1
    count_missings <- rbind(count_missings,c(j,na_present))
  }
  attr(data,"count_missings") <- count_missings
  return(data)
  
}




#nRows <- 1e9
#x<-data.table(x=rnorm(nRows),y=rnorm(nRows),z=sample(LETTERS,nRows,rep=T),
#    d1=sample(LETTERS[1:3],nRows,rep=T),d2=sample(LETTERS[1:2],nRows,rep=T),
#    d3=sample(LETTERS[1:3],nRows,rep=T),d4=sample(LETTERS[1:2],nRows,rep=T))
#origX <- x
#x[sample(1:nRows,nRows/10),1] <- NA
#x[sample(1:nRows,nRows/10),2] <- NA
#x[sample(1:nRows,nRows/10),3] <- NA
#x[sample(1:nRows,nRows/10),4] <- NA
#system.time(ximp <- matchImpute(x,match_var=c("d1","d2","d3","d4")))
