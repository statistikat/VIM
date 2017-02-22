#' Iterative robust model-based imputation (IRMI)
#' 
#' In each step of the iteration, one variable is used as a response variable
#' and the remaining variables serve as the regressors.
#' 
#' The method works sequentially and iterative. The method can deal with a
#' mixture of continuous, semi-continuous, ordinal and nominal variables
#' including outliers.
#' 
#' A full description of the method will be uploaded soon in form of a package
#' vignette.
#' 
#' @param x data.frame or matrix
#' @param eps threshold for convergency
#' @param maxit maximum number of iterations
#' @param mixed column index of the semi-continuous variables
#' @param mixed.constant vector with length equal to the number of
#' semi-continuous variables specifying the point of the semi-continuous
#' distribution with non-zero probability
#' @param count column index of count variables
#' @param step a stepwise model selection is applied when the parameter is set
#' to TRUE
#' @param robust if TRUE, robust regression methods will be applied
#' @param takeAll takes information of (initialised) missings in the response
#' as well for regression imputation.
#' @param noise irmi has the option to add a random error term to the imputed
#' values, this creates the possibility for multiple imputation. The error term
#' has mean 0 and variance corresponding to the variance of the regression
#' residuals.
#' @param noise.factor amount of noise.
#' @param force if TRUE, the algorithm tries to find a solution in any case,
#' possible by using different robust methods automatically.
#' @param robMethod regression method when the response is continuous.
#' @param force.mixed if TRUE, the algorithm tries to find a solution in any
#' case, possible by using different robust methods automatically.
#' @param addMixedFactors if TRUE add additional factor variable for each mixed variable as X variable in the regression
#' @param modelFormulas a named list with the name of variables for the  rhs of the formulas, which must contain a rhs formula for each variable with missing values, it should look like list(y1=c("x1","x2"),y2=c("x1","x3"))
#' 
#' if factor variables for the mixed variables should be created for the
#' regression models
#' @param mi number of multiple imputations.
#' @param trace Additional information about the iterations when trace equals
#' TRUE.
#' @param init.method Method for initialization of missing values (kNN or
#' median)
#' @param multinom.method Method for estimating the multinomial models
#' (current default and only available method is multinom)
#' @return the imputed data set.
#' @author Matthias Templ, Alexander Kowarik
#' @seealso \code{\link[mi]{mi}}
#' @references M. Templ, A. Kowarik, P. Filzmoser (2011) Iterative stepwise
#' regression imputation using standard and robust methods.  \emph{Journal of
#' Computational Statistics and Data Analysis}, Vol. 55, pp. 2793-2806.
#' @references A. Kowarik, M. Templ (2016) Imputation with
#' R package VIM.  \emph{Journal of
#' Statistical Software}, 74(7), 1-16.
#' @keywords manip
#' @examples
#' 
#' data(sleep)
#' irmi(sleep)
#' 
#' data(testdata)
#' imp_testdata1 <- irmi(testdata$wna,mixed=testdata$mixed)
#' 
#' # mixed.constant != 0 (-10)
#' testdata$wna$m1[testdata$wna$m1==0] <- -10
#' testdata$wna$m2 <- log(testdata$wna$m2+0.001)
#' imp_testdata2 <- irmi(testdata$wna,mixed=testdata$mixed,mixed.constant=c(-10,log(0.001)))
#' imp_testdata2$m2 <- exp(imp_testdata2$m2)-0.001
#' 
#' #example with fixed formulas for the variables with missing
#' form=list(
#' NonD=c("BodyWgt","BrainWgt"),
#' Dream=c("BodyWgt","BrainWgt"),
#' Sleep=c("BrainWgt"),
#' Span=c("BodyWgt"),
#' Gest=c("BodyWgt","BrainWgt")
#' )
#' irmi(sleep,modelFormulas=form,trace=TRUE)
#' 
#' # Example with ordered variable
#' td <- testdata$wna
#' td$c1 <- as.ordered(td$c1)
#' irmi(td)
#' 
#' @export irmi
#' @S3method irmi data.frame
#' @S3method irmi survey.design
#' @S3method irmi default
irmi <- function(x, eps=5, maxit=100, mixed=NULL,mixed.constant=NULL, count=NULL, step=FALSE, 
    robust=FALSE, takeAll=TRUE,
    noise=TRUE, noise.factor=1, force=FALSE,
    robMethod="MM", force.mixed=TRUE, mi=1, 
    addMixedFactors=FALSE, trace=FALSE,init.method="kNN",modelFormulas=NULL,multinom.method="multinom") {
  UseMethod("irmi", x)
}

irmi.data.frame <- function(x, eps=5, maxit=100, mixed=NULL,mixed.constant=NULL, count=NULL, step=FALSE, 
    robust=FALSE, takeAll=TRUE,
    noise=TRUE, noise.factor=1, force=FALSE,
    robMethod="MM", force.mixed=TRUE, mi=1, 
    addMixedFactors=FALSE, trace=FALSE,init.method="kNN",modelFormulas=NULL,multinom.method="multinom") {
  irmi_work(x, eps, maxit, mixed, mixed.constant, count, step, 
      robust, takeAll, noise, noise.factor, force,
      robMethod, force.mixed, mi, addMixedFactors, 
      trace,init.method,modelFormulas=modelFormulas,multinom.method=multinom.method)
}

irmi.survey.design <- function(x, eps=5, maxit=100, mixed=NULL,mixed.constant=NULL, count=NULL, step=FALSE, 
    robust=FALSE, takeAll=TRUE,
    noise=TRUE, noise.factor=1, force=FALSE,
    robMethod="MM", force.mixed=TRUE, mi=1, 
    addMixedFactors=FALSE, trace=FALSE,init.method="kNN",modelFormulas=NULL,multinom.method="multinom") {
  x$variables <- irmi_work(x$variables, eps, maxit, mixed, mixed.constant, count, step, 
      robust, takeAll, noise, noise.factor, force,
      robMethod, force.mixed, mi, addMixedFactors, 
      trace,init.method,modelFormulas=modelFormulas,multinom.method=multinom.method)
  x$call <- sys.call(-1)
  x
}

irmi.default <- function(x, eps=5, maxit=100, mixed=NULL,mixed.constant=NULL, count=NULL, step=FALSE, 
    robust=FALSE, takeAll=TRUE,
    noise=TRUE, noise.factor=1, force=FALSE,
    robMethod="MM", force.mixed=TRUE, mi=1, 
    addMixedFactors=FALSE, trace=FALSE,init.method="kNN",modelFormulas=NULL,multinom.method="multinom") {
  irmi_work(as.data.frame(x), eps, maxit, mixed, mixed.constant, count, step, 
      robust, takeAll, noise, noise.factor, force,
      robMethod, force.mixed, mi, addMixedFactors, 
      trace,init.method,modelFormulas=modelFormulas,multinom.method=multinom.method)
}

`irmi_work` <- function(x, eps=5, maxit=100, mixed=NULL,mixed.constant=NULL, count=NULL, step=FALSE, 
    robust=FALSE, takeAll=TRUE,
    noise=TRUE, noise.factor=1, force=FALSE,
    robMethod="MM", force.mixed=TRUE, mi=1, 
    addMixedFactors=FALSE, trace=FALSE,init.method="kNN",modelFormulas=NULL,multinom.method="multinom"){
#Authors: Alexander Kowarik and Matthias Templ, Statistics Austria, GPL 2 or newer, version: 15. Nov. 2012
  #object mixed conversion into the right format (vector of variable names of type mixed)
#TODO: Data sets with variables "y" might fail
  if(trace){
    cat("Method for multinomial models:",multinom.method,"\n")
  }
  if(!is.data.frame(x)){
    if(is.matrix(x))
      x <- as.data.frame(x)
    else
      stop("data frame must be provided")
  }
  if(!is.null(mixed.constant)&&!is.null(mixed)){
    if(length(mixed)!=length(mixed.constant))
      stop("The length of 'mixed' and 'mixed.constant' differ.")
  }
  if(!is.null(mixed)){
    if(!is.character(mixed)){
      if(is.logical(mixed)){
        if(length(mixed)!=length(colnames(x)))
          stop("the mixed parameter is not defined correct.")
        mixed <- colnames(x)[mixed]
      } else if(is.numeric(mixed)){
        if(max(mixed)>length(colnames(x)))
          stop("the mixed parameter is not defined correct.")
        mixed <- colnames(x)[mixed]
      }      
    }else if(!all(mixed%in%colnames(x))){
      stop("Not all mixed variables are found in the colnames of the input dataset.")
    }
  }
  if(!is.null(count)){
    if(!is.character(count)){
      if(is.logical(count)){
        if(length(count)!=length(colnames(x)))
          stop("the count parameter is not defined correct.")
        count <- colnames(x)[count]
      }else if(is.numeric(count)){
        if(max(count)>length(colnames(x)))
          stop("the count parameter is not defined correct.")
        count <- colnames(x)[count]
      }      
    }else if(!all(count%in%colnames(x))){
      stop("Not all count variables are found in the colnames of the input dataset.")
    }
  }
  class1 <- function(x) class(x)[1] 
  types <- lapply(x,class1)
#  if(any(types=="ordered")){
#    for(i in which(types=="ordered")){
#      msg <- paste(names(x)[i]," is defined as ordered,but irmi cannot deal with ordered variables
#              at the moment, therefore the ordered attribute is set to FALSE \n",sep="")
#      cat(msg)
#      x[,i] <- factor(x[,i],ordered=FALSE)
#      types[i] <- "factor"
#    }
#  }
  types[colnames(x)%in%mixed] <- "mixed"
  types[colnames(x)%in%count] <- "count"
  
  attributes(types)$names <-NULL
  types <- unlist(types)
  if(any(types=="character")){
    chrInd <- which(types=="character")
    warning("At least one character variable is converted into a factor")
    for(ind in chrInd){
      x[,ind] <- as.factor(x[,ind])
      types[ind] <- "factor"
    }
  }
  
  #determine factor type: dichotomous or polytomous
  #detect problematic factors
  indFac <- which(types == "factor")
  for(ind in indFac){
    #get number of levels
    fac_nlevels = nlevels(x[[ind]])
    if (fac_nlevels == 2)
      types[ind] <- "binary"
    else if (fac_nlevels > 2)
      types[ind] <- "nominal"
    else stop(sprintf("factor with less than 2 levels detected! - `%s`", names(x)[ind]))
  }
  missingSummary <- cbind(types,apply(x,2,function(x)sum(is.na(x))))
  colnames(missingSummary) <- c("type","#missing")
  
  
#				 save(x, file="xtest.RData")				 
  N <- n <- dim(x)[1]
  P <- dim(x)[2]
  ## error management:
  if(dim(x)[2] < 2) stop("Less than 2 variables included in x.") 
  if(step&&robust)
    stop("robust stepwise is not yet implemented")
  if(!any(is.na(x))) cat("No missings in x. Nothing to impute")
  if(any(apply(x, 1, function(x) all(is.na(x))))) stop("Unit non-responses included in x.")
  ## mixed into logical vector:
  if(!is.logical(mixed) & !is.null(mixed)){
    ind <- rep(FALSE, P)
    ind[mixed] <- TRUE
    mixedlog <- ind
  } else mixedlog <- mixed
  if(!is.character(mixed)){
    mixed <- colnames(x)[mixed]
  } 
  if(!is.character(count)){
    count <- colnames(x)[count]
  }
#  if(!is.null(mixed) && length(mixed) != P) stop(paste("Length of mixed must either be NULL or", P))
  ## count into logical vector:
  #if(!is.logical(count) & !is.null(count)){
  #  ind <- rep(FALSE, P)
  #  ind[which(colnames(x) == count)] <- TRUE
  #  countlog <- ind
  #}  else countlog <- count
#  if(is.null(mixed)) mixed <- rep(FALSE, P)
#  if(is.null(count)) count <- rep(FALSE, P)
#  if(!is.null(count) && length(count) != P) stop(paste("Length of mixed must either be NULL or", P)) 
#if(any(countlog == mixedlog) && countlog == TRUE) stop(paste("you declined variable", which(countlog==mixedlog && countlog==TRUE), "to be both, count and mixed"))
  if(length(Inter(list(count,mixed)))>0) stop(paste("you declined a variable to be both, count and mixed"))
  #for(i in which(countlog)){
  #  class(x[,i]) <- c("count", "numeric")	  
  # }
  
  
  
  ## check for factors in x
  factors <- vector()
  for(i in 1:ncol(x)){
    factors <- c(factors,is.factor(x[,i]))
  }
  
  
  ## Recode the levels of a factor to 1:number of levels
  if(any(factors)){
    factors <- colnames(x)[factors]
    origLevels <- list()
    for(f in 1:length(factors)){
      origLevels[[f]] <- levels(x[,factors[f]])
      levels(x[,factors[f]]) <- 0:(length(origLevels[[f]])-1)
    }
  } else factors <- character(0)
  
  VarswithNA <-vector()
  
  ## index for missingness
  w2  <- is.na(x)
  
  ## variables that include missings
  for(i in seq(P)){
    if(anyNA(x[,i]))
      VarswithNA <- c(VarswithNA,i)    
  }
  ## count runden, da MIttelwertimputation in initialise:
  ndigitsCount <- apply(x[,types=="count", drop=FALSE], 2, 
      function(x){
        x <- as.character(x)
        max(unlist(lapply(strsplit(x, "\\."), function(x) ifelse(length(x) > 1, nchar(strsplit(x, "\\.")[2]), 0))))
      })
  
  ## initialisiere
  #for( j in 1:ncol(x) ) {
  #print(paste("HIER:", j))
  x <- initialise(x,mixed=mixed,method=init.method,mixed.constant=mixed.constant)  
  #}
  
  ## round count variables:
  j <- 0
  for( i in which(types=="count")){
    j <- j+1
    x[,i] <- round(x[,i], ndigitsCount[j])
  } 
  
  if(trace) print(head(x))
  mixedTF <- FALSE
  mixedConstant <- 0
  ### outer loop
  d <- 99999
  it <- 0
  while(d > eps && it < maxit){
    it = it + 1
    if(trace)
      cat("Iteration",it,"\n")
    xSave <- x
    ## inner loop
    for(i in VarswithNA){
      if(trace){
        print(paste("inner loop:",i))
        if(Sys.info()[1] == "Windows") flush.console()
      }
      yPart <- x[, i, drop=FALSE]
      wy <- which(w2[,i])
      xPart <- x[, -i, drop=FALSE]
      
      ## --- Start Additonal xvars for mixed vars
      if(!is.null(mixed)&&addMixedFactors){
        if(any(names(xPart)%in%mixed)){
          mixedIndex <- which(names(xPart)%in%mixed)
          for(ii in 1:length(mixedIndex)){
            namenew <- paste(names(xPart)[mixedIndex[ii]],"ADDMIXED",sep="")
            if(is.null(mixed.constant))
              xPart[,namenew] <- as.numeric(xPart[,mixedIndex[ii]]==0)
            else
              xPart[,namenew] <- as.numeric(xPart[,mixedIndex[ii]]==mixed.constant[ii])
          }          
        }
      } ## end additional xvars for mixed vars ---
      if(!takeAll){
        dataForReg <- data.frame(cbind(yPart[-wy,], xPart[-wy,])) ## part, wo in y keine missings
      } else{
        dataForReg <- data.frame(cbind(yPart, xPart))
      }
      if(!is.null(mixed)){
        if(names(x)[i] %in% mixed){
          mixedTF <- TRUE
          if(is.null(mixed.constant)){
            mixedConstant <- 0
          }else{
            mixedConstant <- mixed.constant[which(mixed==names(x)[i])]
          }
        } else{
          mixedTF <- FALSE
        }
      }
      colnames(dataForReg)[1] <- "y"
      new.dat <- data.frame(cbind(rep(1,length(wy)), xPart[wy,,drop=FALSE])) 
      
      #print(attributes(dataForReg$y)$cn)
      
      if(trace){
        print(types[[i]])
      }
      if( types[i]=="integer"||types[i]=="numeric" || types[i] =="mixed"){ ## todo: ausserhalb der Schleife!!
        meth = "numeric" 
      } else if( types[i]=="binary" ){ 
        meth = "bin" 
      } else if( types[i]=="nominal" ){ 
        meth = "factor" 
      } else if( types[i]=="count"){
        meth = "count"
      }else if( types[i]=="ordered" ){ 
        meth = "ordered" 
      }
      
      ## replace initialised missings:
      if(length(wy) > 0){ 
        #idataForReg <<- dataForReg
        #indata <<- new.dat[,-1,drop=FALSE]
        #imeth <<- meth
        #ii <<- i
        #iindex <<- wy
        #imixedTF<<- mixedTF
        #ifactors <<- factors
        #istep <<- step
        #irobust <<- robust
        #inoise <<- FALSE
        #itypes <<- types
        #debug(getM)
        if(trace)
          print(meth)
        #print(lapply(dataForReg, class))
        #if(i==10) stop("ZUR KONTROLLE i=10")
        if(!is.null(modelFormulas)){
          TFform <- names(modelFormulas)==colnames(x)[i]
          if(any(TFform))
            activeFormula <- modelFormulas[[which(TFform)]]
          else
            activeFormula <- names(dataForReg)[names(dataForReg)!="y"]
        }else
          activeFormula <- names(dataForReg)[names(dataForReg)!="y"]
        if(trace){
          print(paste("formula used:",paste(colnames(x)[i],"~",paste(activeFormula,collapse="+"))))
          if(Sys.info()[1] == "Windows") flush.console()
        }
        x[wy,i] <- getM(xReg=dataForReg, ndata=new.dat[,-1,drop=FALSE], type=meth, 
            index=wy, mixedTF=mixedTF,mixedConstant=mixedConstant, factors=factors, step=step, 
            robust=robust, noise=FALSE, force=force, robMethod,form=activeFormula,multinom.method=multinom.method)
        #if(!testdigits(x$x5)) stop()
      }	
    }  ## end inner loop
    d <- 0
    if(any(types%in%c("numeric","mixed")))
      d <- sum((xSave[,types%in%c("numeric","mixed")] - x[,types%in%c("numeric","mixed")])^2, na.rm=TRUE)  #todo: Faktoren anders behandeln.
    if(any(!types%in%c("numeric","mixed")))
      d <- d + sum(xSave[,!types%in%c("numeric","mixed")]!=x[,!types%in%c("numeric","mixed")])
    flush.console()
    if(trace){
      print(paste("it =",it,",  Wert =",d))
      print(paste("eps", eps))
      print(paste("test:", d > eps))
    }
  } ## end outer loop
  
  
  if( it > 1 ){ 
    d <- 0
    if(any(types%in%c("numeric","mixed")))
      d <- sum((xSave[,types%in%c("numeric","mixed")] - x[,types%in%c("numeric","mixed")])^2, na.rm=TRUE)  #todo: Faktoren anders behandeln.
    if(any(!types%in%c("numeric","mixed")))
      d <- d + sum(xSave[,!types%in%c("numeric","mixed")]!=x[,!types%in%c("numeric","mixed")])
    if(trace){
      if( it < maxit ){ 
        print(paste(d, "<", eps, "= eps")); print(paste("      --> finished after", it, "iterations"))
      } else if (it == maxit){
        print("not converged...");print(paste(d, "<", eps, "= eps"))
      }
    }	
  }
  ### Add NOISE:
  ### A last run with building the model and adding noise...
  if(noise && mi==1){
    for(i in seq(P)){
      flush.console()
      yPart <- x[, i, drop=FALSE]
      wy <- which(w2[,i])
      xPart <- x[, -i, drop=FALSE]
      if(!takeAll){
        dataForReg <- data.frame(cbind(yPart[-wy,], xPart[-wy,])) ## part, wo in y keine missings
      }else{
        dataForReg <- data.frame(cbind(yPart, xPart))
      }
      if(!is.null(mixed)){
        if(names(x)[i] %in% mixed){
          mixedTF <- TRUE
          if(is.null(mixed.constant)){
            mixedConstant <- 0
          }else{
            mixedConstant <- mixed.constant[which(mixed==names(x)[i])]
          }
        }else{
          mixedTF <- FALSE
        }
      }
      colnames(dataForReg)[1] <- "y"
      new.dat <- data.frame(cbind(rep(1,length(wy)), xPart[wy,,drop=FALSE])) 
      if( types[i]=="numeric" || types[i] =="mixed"){ ## todo: ausserhalb der Schleife!!
        meth = "numeric" 
      } else if( types[i]=="binary" ){ 
        meth = "bin" 
      } else if( types[i]=="nominal" ){ 
        meth = "factor" 
      } else if( types[i]=="count"){
        meth = "count"
      } else if( types[i]=="ordered"){
        meth = "ordered"
      }
      if(!is.null(modelFormulas)){
        TFform <- names(modelFormulas)==colnames(x)[i]
        if(any(TFform))
          activeFormula <- modelFormulas[[which(TFform)]]
        else
          activeFormula <- names(dataForReg)[names(dataForReg)!="y"]
      }else
        activeFormula <- names(dataForReg)[names(dataForReg)!="y"]
      if(length(wy) > 0) x[wy,i] <- getM(xReg=dataForReg, ndata=new.dat[,-1,drop=FALSE], 
            type=meth, index=wy,mixedTF=mixedTF,mixedConstant=mixedConstant,factors=factors,
            step=step,robust=robust,noise=TRUE,noise.factor=noise.factor,force=force,robMethod,form=activeFormula,multinom.method=multinom.method)
    }
  }
  ## End NOISE
  #if(!testdigits(x$x5)) stop("s121212121212asasa\n")
  ## Begin multiple imputation
  if(mi>1&&!noise){
    cat("Noise option is set automatically to TRUE")
    noise <- TRUE
  }
  if(mi>1){
    mimp <- list()
    xSave1 <- x 
    for(m in 1:mi){
      for(i in seq(P)){
        flush.console()
        yPart <- x[, i, drop=FALSE]
        wy <- which(w2[,i])
        xPart <- x[, -i, drop=FALSE]
        if(!takeAll){
          dataForReg <- data.frame(cbind(yPart[-wy,], xPart[-wy,])) ## part, wo in y keine missings
        }else{
          dataForReg <- data.frame(cbind(yPart, xPart))
        }
        if(!is.null(mixed)){
          if(names(x)[i] %in% mixed){
            mixedTF <- TRUE
            if(is.null(mixed.constant))
              mixedConstant <- 0
            else
              mixedConstant <- mixed.constant[which(mixed==names(x)[i])]
          }else{
            mixedTF <- FALSE
          }
        }
        colnames(dataForReg)[1] <- "y"
        new.dat <- data.frame(cbind(rep(1,length(wy)), xPart[wy,,drop=FALSE])) 
        if( class(dataForReg$y) == "numeric" ) meth = "numeric" else if( class(dataForReg$y) == "factor" & length(levels(dataForReg$y))==2) meth = "bin" else meth = "factor"
        ## replace initialised missings:
        if(!is.null(modelFormulas)){
          TFform <- names(modelFormulas)==colnames(x)[i]
          if(any(TFform))
            activeFormula <- modelFormulas[[which(TFform)]]
          else
            activeFormula <- names(dataForReg)[names(dataForReg)!="y"]
        }else
          activeFormula <- names(dataForReg)[names(dataForReg)!="y"]
        if(length(wy) > 0) x[wy,i] <- getM(xReg=dataForReg, ndata=new.dat[,-1,drop=FALSE], type=meth, index=wy,mixedTF=mixedTF,mixedConstant=mixedConstant,
              factors=factors,step=step,robust=robust,noise=TRUE,
              noise.factor=noise.factor,force=force,robMethod,form=activeFormula,multinom.method=multinom.method)
      }
      mimp[[m]] <- x
      x <- xSave1
    }
    x <- mimp
  }
  ##  End Multiple Imputation
  
  ## Recode factors to their original coding
  if(length(factors)>0){
    for(f in 1:length(factors)){
#		cat("vorher\n")
#		print(str(x))
      
#		print(origLevels[[f]])
      if(mi>1){
        for(mii in 1:mi)
          levels(x[[mii]][,factors[f]]) <- origLevels[[f]]
      }else{
        levels(x[,factors[f]]) <- origLevels[[f]]
      }
      
#	  cat("nachher\n")
    }
  }
  if(trace){
    cat("Imputation performed on the following data set:\n")
    print(missingSummary)
  }
  invisible(x)
  
}


### utility functions
anyNA <- function(X) any(is.na(X))
Unit <- function(A) UseMethod("Unit")
Unit.list <- function(A){ # Units a list of vectors into one vector
  a<-vector()
  for(i in 1:length(A)){
    a <- c(a,A[[i]])
  }
  levels(as.factor(a))
}
Inter <- function(A) UseMethod("Inter")
Inter.list <- function(A){ # common entries from a list of vectors
  a<-Unit(A)
  TF <- rep(TRUE,length(a))
  for(i in 1:length(a)){
    for(j in 1:length(A)){
      TF[i] <- TF[i] && a[i] %in% A[[j]]
    }
  }
  levels(as.factor(a[TF]))
}



#' Initialization of missing values
#' 
#' Rough estimation of missing values in a vector according to its type.
#' 
#' Missing values are imputed with the mean for vectors of class
#' \code{"numeric"}, with the median for vectors of class \code{"integer"}, and
#' with the mode for vectors of class \code{"factor"}.  Hence, \code{x} should
#' be prepared in the following way: assign class \code{"numeric"} to numeric
#' vectors, assign class \code{"integer"} to ordinal vectors, and assign class
#' \code{"factor"} to nominal or binary vectors.
#' 
#' @param x a vector.
#' @param mixed a character vector containing the names of variables of type
#' mixed (semi-continous).
#' @param method Method used for Initialization (median or kNN)
#' @param mixed.constant vector with length equal to the number of
#' semi-continuous variables specifying the point of the semi-continuous
#' distribution with non-zero probability
#' @return the initialized vector.
#' @note The function is used internally by some imputation algorithms.
#' @author Matthias Templ, modifications by Andreas Alfons
#' @keywords manip
#' @export initialise
`initialise` <- function(x,mixed,method="kNN",mixed.constant=NULL){
  if(method=="median"){
    for( j in 1:ncol(x) ) {
      xx <- x[,j]
      if(class(xx) == "numeric") {xx <- as.vector(impute(as.matrix(xx), "median"))}
      if(class(xx) == "integer") {xx <- as.vector(impute(as.matrix(xx), "median"))}
      if(class(xx) == "factor")  {xx <- as.character(xx)
        #if(class(x)[2] == "count") {x <-as.vector(impute(as.matrix(x), "mean"))} ### hier Fehler #TODO: verbessern
        xx[which(is.na(xx))] <-  names(which.max(table(xx)))
        xx <- as.factor(xx)}
      x[,j] <- xx
    }
  }else{
    x <- invisible(kNN(x,imp_var=FALSE,mixed=mixed,mixed.constant=mixed.constant))
  }
  return(x)                         
}

## switch function to automatically select methods
getM <- function(xReg, ndata, type, index,mixedTF,mixedConstant,factors,step,robust,noise,noise.factor=1,force=FALSE, robMethod="MM",form=NULL,multinom.method="mnlogit") {
  switch(type,
      numeric = useLM(xReg, ndata, index,mixedTF,mixedConstant,factors,step,robust,noise,noise.factor,force,robMethod,form=form),
      factor  = useMN(xReg, ndata, index,factors,step,robust,form=form,multinom.method=multinom.method),
      bin     = useB(xReg, ndata, index,factors,step,robust,form=form),
      count   = useGLMcount(xReg, ndata, index, factors, step, robust,form=form),
      ordered  = useOrd(xReg, ndata, index,factors,step,robust,form=form),
  )
}

### LM+GLM --- useLM start
useLM <- function(xReg,  ndata, wy, mixedTF,mixedConstant, factors, step, robust, noise, noise.factor, force, robMethod,form){
  n <- nrow(xReg)
  factors <- Inter(list(colnames(xReg),factors))
  ## for semicontinuous variables
  if(mixedTF){
    delFactors <- vector()
    if(length(factors)>0){
      for(f in 1:length(factors)){
        if(any(summary(xReg[,factors[f]])==0)){
          xReg <- xReg[,-which(colnames(xReg)==factors[f])]
          ndata <- ndata[,-which(colnames(ndata)==factors[f])]
          delFactors <- c(delFactors,factors[f])
        }
      }
    }
    xReg1 <- xReg
    xReg1$y[xReg$y==mixedConstant] <- 0
    xReg1$y[xReg$y!=mixedConstant] <- 1
    form <- form[form%in%names(xReg1)]
    if(class(form)!="formula")
      form <- as.formula(paste("y ~",paste(form,collapse="+")))
    else
      form <- y~.
    if(!robust)
      glm.bin <- glm(form , data=xReg1, family="binomial")
    else{
      glm.bin <- glm(form , data=xReg1, family="binomial")  
    }
#     if VGAM will be chosen instead of multinom:	  
#	  op <- options() #Alles auskommentiert, weil VGAM draussen!
#	  options(show.error.messages=FALSE)
#	  try(detach(package:VGAM))
#	  options(op)
    if(step)
      glm.bin <- stepAIC(glm.bin,trace=-1)
    ## imputation
    imp <- predict(glm.bin, newdata=ndata, type="response")
    imp[imp < 0.5] <- 0
    imp[imp >= 0.5] <- 1
    xReg <- xReg[xReg$y != mixedConstant,]
    factors2 <- factors[!factors%in%delFactors]
    if(length(factors2) > 0){
      for(f in 1:length(factors2)){
        if(any(summary(xReg[,factors2[f]])==0)){
          xReg <- xReg[,-which(colnames(xReg)==factors2[f])]
          ndata <- ndata[,-which(colnames(ndata)==factors2[f])]
        }
      }
    }
    ## for continuous variables:  
  } else{
    if(length(factors)>0){
      delFactors <- vector()
      for(f in 1:length(factors)){
        if(any(summary(xReg[,factors[f]])==0)){
          xReg <- xReg[,-which(colnames(xReg)==factors[f])]
          ndata <- ndata[,-which(colnames(ndata)==factors[f])]
          delFactors <- c(delFactors,factors[f])
        }
      }
    }
    imp <- rep(1,nrow(ndata))
  }
  ##Two-Step
  if(class(form)!="formula"){
    form <- form[form%in%names(xReg)]
    if(length(form)>0)
      form <- as.formula(paste("y ~",paste(form,collapse="+")))
    else
      form <- y~.
  }else{
    formVars <- all.vars(form)[-1]
    if(any(!formVars%in%colnames(xReg))){
      formVars <- formVars[formVars%in%colnames(xReg)]
      form <- as.formula(paste("y ~",paste(formVars,collapse="+")))
    }
  }
  if(!robust){
    glm.num <- glm(form, data=xReg, family="gaussian")
    #cat("not ROBUST!!!!!!!!\n")
  } else{
    if(exists("glm.num"))
      rm(glm.num)
    if(force){
      try(glm.num <- rlm(form , data=xReg,method="MM"),silent=TRUE)
      if(!exists("glm.num")){
        try(glm.num <- lmrob(form , data=xReg),silent=TRUE)
        if(!exists("glm.num")){
          glm.num <- rlm(form , data=xReg,method="M")
          if(!exists("glm.num")){
            glm.num <- glm(form, data=xReg, family="gaussian")
          }
        }
      }
    } else{
      if(robMethod=="lmrob"){
        glm.num <- lmrob(form , data=xReg)
      }else if(robMethod=="lqs"){
        glm.num <- lqs(form , data=xReg)
      }else{
        glm.num <- rlm(form , data=xReg,method=robMethod)
      }
    } 
  }
#  op <- options()#Alles auskommentiert, weil VGAM draussen
#  options(show.error.messages=FALSE)
#  try(detach(package:VGAM))
#  options(op)
  if(step){
    glm.num <- stepAIC(glm.num,trace=-1)
  }
  
  if(noise){
    if(!robust){
      consistencyFactor <- sqrt((nrow(ndata[imp==1,,drop=FALSE])/n + 1))#*n/(n+1)
      nout <- nrow(ndata[imp==1,,drop=FALSE])
      p.glm.num <- predict(glm.num, newdata=ndata[imp==1,,drop=FALSE],se.fit=TRUE)
      if(is.nan(p.glm.num$residual.scale)){
        warning("The residual scale could not be computed, probably due to a rank deficient model. It is set to 1\n")
        p.glm.num$residual.scale <- 1
      }
      imp2 <- p.glm.num$fit+noise.factor*rnorm(length(p.glm.num$fit),0,p.glm.num$residual.scale*consistencyFactor)
    } else{
      nout <- nrow(ndata[imp==1,,drop=FALSE])
      consistencyFactor <- sqrt((nrow(ndata[imp==1,,drop=FALSE])/n + 1))#*(n)/(n+1))
      p.glm.num <- predict(glm.num, newdata=ndata[imp==1,,drop=FALSE])
      if(is.nan(glm.num$s)){
        warning("The residual scale could not be computed, probably due to a rank deficient model. It is set to 1\n")
        glm.num$s <- 1
      }
      imp2 <- p.glm.num + noise.factor*rnorm(length(p.glm.num),0,glm.num$s*consistencyFactor)
    }
  } else
    imp2 <- predict(glm.num, newdata=ndata[imp==1,,drop=FALSE])
  imp3 <- imp
  imp3[imp==0] <- mixedConstant
  imp3[imp==1] <- imp2
  return(imp3)
#		library(VGAM, warn.conflicts = FALSE, verbose=FALSE)
# -end useLM-
}

## count data as response
useGLMcount <- function(xReg,  ndata, wy, factors, step, robust,form){
  factors <- Inter(list(colnames(xReg),factors))
  if(length(factors)>0){
    for(f in 1:length(factors)){
      if(any(summary(xReg[,factors[f]])==0)){
        xReg <- xReg[,-which(colnames(xReg)==factors[f])]
        ndata <- ndata[,-which(colnames(ndata)==factors[f])]
      }
    }
  }
  form <- form[form%in%names(xReg)]
  if(length(form)>0)
    form <- as.formula(paste("y ~",paste(form,collapse="+")))
  else
    form <- y~.
  if(robust){
    #glmc <- glm(y~ ., data=xReg, family=poisson)
    glmc <- glmrob(form, data=xReg, family=poisson)
    glmc$rank<-ncol(xReg)
    #glmc$coef <- glmcR$coef
  } else {
    glmc <- glm(form, data=xReg, family=poisson)
  }
  if(step & robust) stop("both step and robust equals TRUE not provided")
  if(step){
    glmc <- stepAIC(glmc, trace=-1)
  }
  imp2 <- round(predict(glmc, newdata=ndata,type="response"))
  #iin[[length(iin)+1]]<<-imp2
  return(imp2)
}

# categorical response
useMN <- function(xReg, ndata,  wy, factors, step, robust,form,multinom.method){
  factors <- Inter(list(colnames(xReg),factors))
  if(length(factors)>0){
    for(f in 1:length(factors)){
      if(any(summary(xReg[,factors[f]])==0)){
        xReg <- xReg[,-which(colnames(xReg)==factors[f])]
        ndata <- ndata[,-which(colnames(ndata)==factors[f])]
      }
    }
  }
  form <- form[form%in%names(xReg)]
  if(length(form)>0)
    form <- as.formula(paste("y ~",paste(form,collapse="+")))
  else
    form <- y~.
  if(multinom.method=="multinom"){
    co <- capture.output(multimod <- multinom(form, data=xReg,summ=2,maxit=50,trace=FALSE))
    if(step){
      multimod <- stepAIC(multimod,xReg)
    }
    imp <- predict(multimod, newdata=ndata)
  }else{
    stop("multinom is the only implemented method at the moment!\n") 
  }
  return(imp)
}


# ordered response
useOrd <- function(xReg, ndata,  wy, factors, step, robust,form){
  factors <- Inter(list(colnames(xReg),factors))
  if(length(factors)>0){
    for(f in 1:length(factors)){
      if(any(summary(xReg[,factors[f]])==0)){
        xReg <- xReg[,-which(colnames(xReg)==factors[f])]
        ndata <- ndata[,-which(colnames(ndata)==factors[f])]
      }
    }
  }
  form <- form[form%in%names(xReg)]
  if(length(form)>0)
    form <- as.formula(paste("y ~",paste(form,collapse="+")))
  else
    form <- y~.
  co <- capture.output(multimod <- polr(form, data=xReg))
  if(step){
    multimod <- stepAIC(multimod,xReg)
  }
  imp <- predict(multimod, newdata=ndata)
  return(imp)
}

# binary response
useB <- function(xReg,  ndata, wy,factors,step,robust,form){
  factors <- Inter(list(colnames(xReg),factors))
  #TODO: Faktoren mit 2 Levels und nicht Levels 0 1, funktionieren NICHT!!!!
  if(length(factors)>0){
    for(f in 1:length(factors)){
      if(any(summary(xReg[,factors[f]])==0)){
        xReg <- xReg[,-which(colnames(xReg)==factors[f])]
        ndata <- ndata[,-which(colnames(ndata)==factors[f])]
      }
    }
  }
  form <- form[form%in%names(xReg)]
  if(length(form)>0)
    form <- as.formula(paste("y ~",paste(form,collapse="+")))
  else
    form <- y~.
  if(!robust)
    glm.bin <- glm(form , data=xReg, family="binomial")
  else{
#		glm.bin <- BYlogreg(x0=xReg[,-1], xReg[,1]) ## BYlogreg kann niemals funken
    glm.bin <- glm(form , data=xReg, family="binomial")		
#      if(exists("glm.bin"))
#        rm(glm.bin)
#      try(glm.bin <- glmrob(y ~ . , data=xReg, family="binomial"),silent=TRUE)
#      if(exists("glm.bin"))
#        glm.bin$rank <- ncol(xReg)
#      else
#        glm.bin <- glm(y ~ . , data=xReg, family="binomial")
    
  }
#	op <- options() # Alles auskommentiert, weil VGAM draussen
#	options(show.error.messages=FALSE)
#	try(detach(package:VGAM))
#	options(op)
  if(step)
    glm.bin <- stepAIC(glm.bin,trace=-1)
  imp <- predict(glm.bin, newdata=ndata, type="response")
  imp[imp < 0.5] <- 0
  imp[imp >= 0.5] <- 1
#    library(VGAM, warn.conflicts = FALSE, verbose=FALSE)
  return(imp)
}