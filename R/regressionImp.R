# ---------------------------------------
# Author: Alexander Kowarik
# ---------------------------------------

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
