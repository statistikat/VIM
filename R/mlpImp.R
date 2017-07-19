#' Deep Learning Imputation for Categorical Variables
#' 
#' mxnet is used 
#' 
#' only works for character and factor variables
#' 
#' @aliases mlpImp
#' @param data data.frame, data.table, survey object or matrix
#' @param variable variables to be imputed
#' @param modelVariables variables used for modelling
#' @param imp_var TRUE/FALSE if a TRUE/FALSE variables for each imputed
#' variable should be created show the imputation status
#' @param imp_suffix suffix for the TRUE/FALSE variables showing the imputation
#' status
#' @return the imputed data set.
#' @author  Alexander Kowarik
#' @examples
#' 
#' data(sleep,package="VIM")
#' d2 <- sleep#kNN(sleep,imp_var = FALSE)
#' d2$Pred <- as.character(d2$Pred)
#' d2$Exp <- as.factor(d2$Exp)
#' d2$Danger <- as.factor(d2$Danger)
#' d2[1:10,c("Pred","Danger")] <- NA
#' di <- mlpImp(d2)
#' 
#' 
#' @export
mlpImp <- function(data,variable=colnames(data),modelVariables=colnames(data), 
                   imp_var = TRUE, imp_suffix = "imp"){
  if(!require(mxnet)){
    warning("The package 'mxnet' is not available, therefore nothing will be imputed.")
  }else{
    for(v in variable){
      if(!any(is.na(data[,v]))){
      }else if(!(is.factor(data[,v])|is.character(data[,v]))){
        #TODO: better type checking
        warning(paste("Variable",v,"will not be imputed, because it seems not be a categorical variable."))
      }else{
        modelVariablesTmp <- modelVariables[modelVariables!=v]
        train.ind <- which(!is.na(data[,v]))
        modelVariablesTmp <- na.omit(sapply(modelVariablesTmp,function(x)ifelse(any(is.na(data[-train.ind,x])),NA,x)))
        modelVariablesTmp <- modelVariablesTmp[apply(data[train.ind,c(modelVariablesTmp)],2,function(x)!any(is.na(x)))]
        train.x = data.matrix(data[train.ind,modelVariablesTmp])
        fac <- as.factor(data[train.ind, v])
        train.y = as.numeric(fac)
        test.x = data.matrix(data[-train.ind,modelVariablesTmp])
        model <- mx.mlp(train.x, train.y, hidden_node=10, out_node=length(levels(fac)), out_activation="softmax",
                        num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                        eval.metric=mx.metric.accuracy)
        test.y <- predict(model, test.x)
        data[-train.ind,v] <- levels(fac)[max.col(t(test.y))]
      }
    }
  }
  return(data)
}



