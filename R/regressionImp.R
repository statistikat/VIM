# ---------------------------------------
# Author: Alexander Kowarik
# ---------------------------------------



#' Iterative robust model-based imputation (IRMI)
#' 
#' In each step of the iteration, one variable is used as a response variable
#' and the remaining variables serve as the regressors.
#' 
#' "lm" is used for family "normal" and glm for all other families.
#' (Robust=TRUE: lmrob, glmrob)
#' 
#' @param formula model formula to impute one variable
#' @param data A data.frame or survey object containing the data
#' @param family family argument for "glm" ("AUTO" tries to choose
#' automatically, only really tested option!!!)
#' @param robust TRUE/FALSE if robust regression should be used
#' @param mod_cat TRUE/FALSE if TRUE for categorical variables the level with
#' the highest prediction probability is selected, otherwise it is sampled
#' according to the probabilities.
#' @param imp_var TRUE/FALSE if a TRUE/FALSE variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix used for TF imputation variables
#' @return the imputed data set.
#' @author Alexander Kowarik
#' @keywords manip
#' @examples
#' 
#' data(sleep)
#' sleepImp1 <- regressionImp(Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#' sleepImp2 <- regressionImp(Sleep+Gest+Span+Dream+NonD~BodyWgt+BrainWgt,data=sleep)
#' sleepImp3 <- regressionImp(Sleep+Gest+Span+Dream+NonD~BodyWgt+BrainWgt,data=sleep,robust=TRUE)
#' 
#' data(testdata)
#' imp_testdata1 <- regressionImp(c1+c2~x1+x2,data=testdata$wna)
#' imp_testdata2 <- regressionImp(x1+b1+c1+c2~x2,data=testdata$wna)
#' imp_testdata3 <- regressionImp(x1+b1+c1+c2~x2,data=testdata$wna,robust=TRUE)
#' 
#' @export regressionImp
#' @S3method regressionImp data.frame
#' @S3method regressionImp survey.design
#' @S3method regressionImp default
regressionImp <- function(formula,data,family="AUTO", robust=FALSE,imp_var = TRUE,
    imp_suffix = "imp",mod_cat=FALSE) {
  UseMethod("regressionImp", data)
}

regressionImp.data.frame <- function(formula,data,family="AUTO", robust=FALSE,imp_var = TRUE,
    imp_suffix = "imp",mod_cat=FALSE) {
  regressionImp_work(formula=formula, data=data,family=family, robust=robust,
      imp_var=imp_var,imp_suffix=imp_suffix,mod_cat=mod_cat)
}

regressionImp.survey.design <- function(formula, data, family, robust,imp_var = TRUE,
    imp_suffix = "imp",mod_cat=FALSE) {
  data$variables <- regressionImp_work(formula=formula, data=data$variables,family=family,
      robust=robust,imp_var=imp_var,imp_suffix=imp_suffix,mod_cat=mod_cat)
  data$call <- sys.call(-1)
  data
}

regressionImp.default <- function(formula, data, family="AUTO", robust=FALSE,imp_var = TRUE,
    imp_suffix = "imp",mod_cat=FALSE) {
  regressionImp_work(formula=formula, data=as.data.frame(data),family=family,
      robust=robust,imp_var=imp_var,imp_suffix=imp_suffix,mod_cat=mod_cat)
}

regressionImp_work <- function(formula, family, robust, data,imp_var,imp_suffix,mod_cat){
  
  formchar <- as.character(formula)
  lhs <- gsub(" ","",strsplit(formchar[2],"\\+")[[1]])
  rhs <- formchar[3]
  rhs2 <- gsub(" ","",strsplit(formchar[3],"\\+")[[1]])
  TFna2 <- apply(data[,c(rhs2),drop=FALSE],1,function(x)!any(is.na(x)))
  for(lhsV in lhs){
    form <- as.formula(paste(lhsV,"~",rhs))
    if(!any(is.na(data[,lhsV]))) warning(paste("No missings in","lhsV",". Nothing to impute\n"))
    if(class(family)!="function"){
      if(family=="AUTO"){
        if("numeric"%in%class(data[,lhsV])){
          nLev <- 0
          if(robust)
            mod <- lmrob(form,data=data)
          else
            mod <- lm(form,data=data)
        }else if("factor"%in%class(data[,lhsV])){
          nLev <- length(levels(data[,lhsV]))
          if(nLev==2){
            fam <- binomial
            if(robust)
              mod <- glmrob(form,data=data,family=fam)
            else
              mod <- glm(form,data=data,family=fam)
          }else{
            fam <- "multinomial"
            TFna <- TFna2&!is.na(data[,lhsV])
            mod <- glmnet(model.matrix(form,data[TFna,]),data[TFna,lhsV],family=fam)
          }
        }
        if(imp_var){
          data$NEWIMPTFVARIABLE <- is.na(data[,lhsV])
          colnames(data)[ncol(data)] <- paste(lhsV,"_",imp_suffix,sep="")
        }
        TFna1 <- is.na(data[,lhsV])
        TFna3 <- TFna1&TFna2
        tmp <- data[TFna3,]
        tmp[,lhsV] <- 1
        if(nLev>2){
          modcv <- cv.glmnet(model.matrix(form,data[TFna,]),data[TFna,lhsV],family="multinomial")
          pre <- predict(mod,newx=model.matrix(form,tmp),type="response",s=modcv$lambda.1se)[,,1]
          if(mod_cat){
            pre <- levels(data[,lhsV])[apply(pre,1,which.max)]
          }else{
            pre <- levels(data[,lhsV])[apply(pre,1,function(x)sample(1:length(x),1,prob=x))]
          }
        }else if(nLev==2){
          if(mod_cat){
            pre <- predict(mod,newdata=tmp,type="response")
            pre <- levels(data[,lhsV])[as.numeric(pre>.5)+1]
          }else{
            pre <- predict(mod,newdata=tmp,type="response")
            pre <- levels(data[,lhsV])[sapply(pre,function(x)sample(1:2,1,prob=c(1-x,x)))]
          }
          
        }else{
          pre <- predict(mod,newdata=tmp)
        }
        if(sum(TFna1)>sum(TFna3))
          warning(paste("There still missing values in variable",lhsV,". Probably due to missing values in the regressors."))
        data[TFna3,lhsV] <- pre
        
      }
    }else{
      TFna1 <- is.na(data[,lhsV])
      TFna3 <- TFna1&TFna2
      tmp <- data[TFna3,]
      tmp[,lhsV] <- 1
      if(robust)
        mod <- glmrob(form,data=data,family=family)
      else
        mod <- glm(form,data=data,family=family)
      pre <- predict(mod,newdata=tmp,type="response")
      data[TFna3,lhsV] <- pre
    }
    
  }
  invisible(data)
}
