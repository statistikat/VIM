# Frozen hotdeck implementation from VIM 7.5.1 before the donor lookup optimization.
hotdeck <- function(data , variable=NULL, ord_var=NULL,domain_var=NULL,
    makeNA=NULL,NAcond=NULL,impNA=TRUE,donorcond=NULL,
    imp_var=TRUE,imp_suffix="imp"
    ){
  check_data(data)
  if(!is.null(variable)&&!is.null(ord_var)){
    if(length(intersect(ord_var,variable))>0){
      stop(paste0(intersect(ord_var,variable),collapse=", "),
           " should not be in the parameters ord_var and variable.
           Since this can lead to unforeseen results and errors.")
    }
  }
  OriginalSortingVariable <- impvar <- NULL #empty init
  if(is.null(variable)){
    variable <- colnames(data)
    variable<-variable[!variable%in%c(ord_var,domain_var)]
  }
  if(!is.null(makeNA)){
    if(!is.list(makeNA)||!length(makeNA)==length(variable))
      stop("makeNA is not defined correctly. \n It should be a list of length equal to the length of the argument 'variable'.")
  }
  classx <- class(data)
  VariableSorting <- colnames(data)
  data$OriginalSortingVariable <- 1:nrow(data)
  data <- data.table(data)
  if(is.null(variable)){
    variable  <- colnames(data)[apply(is.na(data),2,any)]
  }
  if(!is.null(NAcond))
    warning("NAcond is not implemented yet and will be ignored.")

  classWithoutLabelled <- function(x){
    cl <- class(x)
    return(cl[cl!="labelled"])
  }
  varType <- sapply(data,classWithoutLabelled)[variable]
  if(imp_var){
    for(v in variable){
      data[,impvar:=FALSE]
      impvarname <- paste(v,"_",imp_suffix,sep="")
      setnames(data,"impvar",impvarname)
      VariableSorting <- c(VariableSorting,impvarname)
    }
  }
  # If no ord_var is defined, a random ordered will be used
  if(is.null(ord_var)){
    RandomVariableForImputationWithHotdeck <- NULL # Init for CRAN check
    nrowXforRunif <- nrow(data)
    data[,RandomVariableForImputationWithHotdeck:=runif(nrowXforRunif)]
    ord_var <- "RandomVariableForImputationWithHotdeck"
  }
  setkeyv(data,ord_var)
  # if no domain_var is defined, the imputeHD function is automatically called on the
  # whole data set
  data <- data[,imputeHD(.SD,variableX=variable,varTypeX=varType,
    imp_varX=imp_var,imp_suffixX=imp_suffix,impNAX=impNA,makeNAX=makeNA,
    ord_varX = ord_var, donorcond = donorcond), by = domain_var]
  if(any(ord_var=="RandomVariableForImputationWithHotdeck")){
    data[,RandomVariableForImputationWithHotdeck:=NULL]
    ord_var <- NULL
  }

  setkey(data,OriginalSortingVariable)
  data[,OriginalSortingVariable:=NULL]
  if(all(classx!="data.table"))
    return(as.data.frame(data)[,VariableSorting,drop=FALSE])
  return(data[,VariableSorting,with=FALSE])
}


## xx should be a data.table and ord_var the name of variables to sort
imputeHD <- function(xx,variableX,varTypeX,imp_varX,imp_suffixX,
                     impNAX,makeNAX, ord_varX, donorcond){
  donor_applicable <- OriginalSortingVariable <- weirdandlongname <- UniqueIdForImputation <- NULL#empty init
  J <- function()NULL#empty init
  xx$UniqueIdForImputation <- 1:nrow(xx)
  prevKey <- key(xx)
  for(v in variableX){
    xx[, donor_applicable := !is.na(xx[[v]])]
    if (!is.null(donorcond)) {
      if(!is.null(donorcond[[match(v,variableX)]])){
        condition_string <- paste0("xx[[v]]", donorcond[[match(v,variableX)]],collapse="&")
        TF <- eval(parse(text=condition_string))
        xx[, donor_applicable := donor_applicable & TF]
      }
    }
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
      impPart <- xx[J(NA_real_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
    }else{
      impPart <- xx[J(NA_character_),UniqueIdForImputation,nomatch=FALSE]#$UniqueIdForImputation
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

      TFindex <- xx[impDon, !donor_applicable]
      TF <- any(TFindex)
      if(TF){
        add <- 2
        while(TF){
          impDon[TFindex] <- impPart[TFindex]-add
          if(any(impDon[TFindex][impDon[TFindex]<1]<=-nrow(xx))){
            impDon[TFindex][impDon[TFindex]<1][impDon[TFindex][impDon[TFindex]<1]<=-nrow(xx)] <-
              - impDon[TFindex][impDon[TFindex]<1][impDon[TFindex][impDon[TFindex]<1]<=-nrow(xx)]
          }
          impDon[TFindex][impDon[TFindex]<1] <- impPart[TFindex][impDon[TFindex]<1]-add+nrow(xx)
          impDon2 <- impDon[TFindex]
          Don[TFindex] <- data.frame(xx[impDon2,v,with=FALSE])[,1]
          TFindex[TFindex] <- xx[impDon2, !donor_applicable]
          TF <- any(TFindex)
          if(add>min(50, nrow(xx))){
            TF <- FALSE

            # remaining missing values will be set to a random value from the group
            if(length(Don)>0){
              if(sum(!TFindex)==0){
                stop("No donor available for ", v, " in a certain group.")
              }
              Don[TFindex] <- Don[!TFindex][sample(sum(!TFindex),1)]
            }

            if(!identical(ord_varX, "RandomVariableForImputationWithHotdeck")){
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
