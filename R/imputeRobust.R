#' @title Robust imputation
#' @description Multiple imputation using classical and robust methods 
#' accounting for model and imputation uncertainty. 
#' @param form Model formulas as a list.
#' @param data Data set to impute
#' @param boot Accounting for model uncertainty with a classical bootstrap, 
#' Default: TRUE
#' @param robustboot Accounting for model uncertainty with robust bootstrap 
#' methods, Default: 'stratified'
#' @param method Imputation method, Default: 'MM'
#' @param takeAll Missing values are intialized when TRUE, Default: TRUE
#' @param alpha Relative size of good data points. Used for the robust
#' bootstrap methods, Default: 0.75
#' @param uncert Imputation uncertainty method, Default: 'pmm'
#' @param familiy Not supported and ignored. Foreseen for future versions, Default: 'Gaussian'
#' @param value_back Only observations with imputed values as return object (ymiss),
#'  or the whole data set, Default: 'all'
#' @return Imputed data set.
#' @details Complex formulas can be provided for each variable in 
#' your data set. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[VIM]{initialise}}
#'  \code{\link[robustbase]{lmrob}}
#'  \code{\link[mgcv]{gam}}
#'  \code{\link[pdist]{pdist}}
#' @rdname imputeRobust
#' @export 
#' @importFrom VIM initialise
#' @importFrom robustbase lmrob
#' @importFrom mgcv gam
#' @importFrom pdist pdist
imputeRobust <- function(form, 
                         data, 
                         boot = TRUE,
                         robustboot = "stratified",
                         method = "MM",
                         takeAll = TRUE,
                         alpha = 0.75, 
                         uncert = "pmm", 
                         family = "Gaussian", 
                         value_back = "all"){
  supportedMethods <- c("lts", "lm", "gam", "gamRob", "MM", "robGAM", "robGam")
  if(method == "gamRob" | method == "robGAM" | method == "robGam"){
    method <- "gamRob"
  }
  if(method == "gam" | method == "gamRob"){
    if(inherits(form, "formula")){
      y <- all.vars(form)
    } else{
      y <- all.vars(form)[1]      
    }
    ynam <- y[1]
  } else{
    mf <- model.frame(form, data = data)
    y <- model.extract(mf, "response")
    ynam <- all.vars(form)[1]   
  }
  x_vars <- all.vars(form[-1]) 
  rn <- rownames(data)
  n <- nrow(data)
#  ynam <- as.character(form[2])

  missindex <- is.na(data[, ynam])
  m <- 1 # currently not further considered
  
  # initialize if takeAll
  if(takeAll){
    data <- VIM::initialise(data, mixed = NULL)
  }
  
  if(method == "lts"){
    mod <- ltsReg(form, data = data)
    if(boot){
      if(robustboot == "quantile"){
        boot_idx <- sample(x = mod$best, size = n, replace = TRUE)
      } else {
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE)       
      }
      boot_dat <- data[boot_idx, ]
      mod <- lm(form, data = boot_dat)
    } else {
      mod <- lm(form, data = data[mod$best, ])  
    }
    sdev <- sd(resid(mod))
  } else if(method == "lm"){
    mod <- lm(form, data = data)
    if(boot){
      if(robustboot == "quantile"){
        idx <- which(resid(mod) < quantile(resid(mod), alpha))
        boot_idx <- sample(x = idx, size = n, replace = TRUE)
      } else{
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE)       
      }
      boot_dat <- data[boot_idx, ]
      mod <- lm(form, data = boot_dat)
    }
    sdev <- sd(resid(mod))
  } else if(method == "MM"){
    mod <- robustbase::lmrob(form, data = data) 
    if(boot){
      if(robustboot == "quantile"){
        boot_idx <- sample(1:n, size = n, replace = TRUE, prob = mod$rweights)       
      } else {
        boot_idx <- sample(1:n, size = n, replace = TRUE)           
      }
      boot_dat <- data[boot_idx, ]
      mod <- robustbase::lmrob(form, data = boot_dat)     
    }
    sdev <- summary(mod)$scale
  } else if(method == "gam"){
    mod <- mgcv::gam(form, data = data)
    if(boot){
      divide <- resid(mod) < quantile(resid(mod), alpha)
      idx <- which(divide)
      if(robustboot == "quantile"){
        boot_idx <- sample(x = idx, size = n, replace = TRUE)       
      } else if(robustboot == "residual"){
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE, 
                           prob = max(abs(resid(mod))) - abs(resid(mod)))           
      } else if(robustboot == "psi"){
        # r <- tukeyPsi1(resid(mod)/mad(resid(mod)))
        u <- resid(mod) / mad(resid(mod))
        w <- ifelse(abs(u) > 4.685, 0, (1-(u^2)/4.685^2)^2)
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE, 
                           prob = max(abs(w)) - abs(w))           
      } else if(robustboot == "stratified"){
        boot_idx <- sample(x = idx, size = round(n * alpha), replace = TRUE)
        boot_nidx <- sample(x = which(!divide), size = round(n * (1 - alpha)), replace = TRUE)
        boot_idx <- c(boot_idx, boot_nidx)
        } else{
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE)         
      }
      boot_dat <- data[boot_idx, ]
      mod <<-  mgcv::gam(form, data = boot_dat)  
    }
    sdev <- summary(mod)$scale
  } else if(method == "gamRob"){
    tmp <- robGAM(form, data = data, fraction = alpha)
    mod <- tmp$mod
    goodPoints <- tmp$subset_good
    #divide <- resid(mod) < quantile(resid(mod), alpha)
    badPoints <- tmp$subset_bad
    if(boot){
      idx <- goodPoints #which(resid(mod) < quantile(resid(mod), alpha))
      if(robustboot == "quantile" | robustboot == "bacon"){
        boot_idx <- sample(x = idx, size = n, replace = TRUE)       
      } else if(robustboot  == "residual"){ # makes no sense
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE, 
                           prob = max(abs(resid(mod))) - abs(resid(mod)))           
      } else if(robustboot  == "psi"){ # makes no sense
        u <- resid(mod) / mad(resid(mod))
        w <- ifelse(abs(u) > 4.685, 0, (1-(u^2)/4.685^2)^2)
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE, 
                           prob = max(abs(w)) - abs(w))           
      } else if(robustboot == "stratified"){
        boot_idx <- sample(x = goodPoints, size = round(n * alpha), replace = TRUE)
        boot_nidx <- sample(x = badPoints, size = round(n * (1 - alpha)), replace = TRUE)
        boot_idx <- c(boot_idx, boot_nidx)
      } else{
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE)         
      }
      boot_dat <- data[boot_idx, ]
      mod <-  robGAM(form, data = boot_dat, fraction = 0.75)$mod  
    }
    sdev <- summary(mod)$scale
  } else {
    cat("----\n")
    stop(paste("Method not supported, \n   choose one of", paste(supportedMethods, collapse = ", "), "\n----\n"))
  }
  
  
  # beta <- coef(mod)
  # V <- vcov(mod)
  # Cv <- chol(V)
  
  # nus <- rnorm(m * length(beta))
  # beta_sims <- beta + 
  #   t(Cv) %*% 
  #   matrix(nus, nrow = length(beta), ncol = m)
  # covar_sim <- predict(mod, newdata = boot_dat, type = "lpmatrix")
  pred <-  predict(mod, newdata = data)
#  pred <-  predict(mod)
  # linpred_sim <- covar_sim %*% beta_sims
  # invlink <- function(x) x
  # exp_val_sim <- invlink(linpred_sim)
  if(uncert == "normalerror"){
    ymiss <- pred[missindex] + rnorm(n = sum(missindex), 
                                     mean = 0,#exp_val_sim, 
                                     sd = sdev)
  }
  if(uncert == "resid"){
      ymiss <- pred[missindex] + sample(resid(mod), size = sum(missindex)) 
  }
  if(uncert == "wresid"){
    ymiss <- rep(NA, sum(missindex))
    cnt <- 0
    if(length(resid(mod)) < n){
      resi <- rep(resid(mod), length.out = n)
    } else{
      resi <- resid(mod)
    }
    for(i in (1:n)[missindex]){
      cnt <- cnt + 1
      d <- pdist::pdist(as.matrix(data[i, x_vars, drop = FALSE]), 
                      as.matrix(data[, x_vars, drop = FALSE]))
      d <- 1 - d@dist / max(d@dist)
      d <- sqrt(d)
      p <- pred[i]
      ymiss[cnt] <- pred[i] + 
        sample(resi, 
        size = 1, prob = d)     
    }
  }
  if(uncert == "pmm"){
    selectDonor <- function(y, val){
      donors <- numeric(length(val))
      for(i in 1:length(val)){
        # donors[i] <- sample(y[sort(abs(val - y), index.return  = TRUE)$ix 
        #                       %in% 1:5], 1)
        #donors[i] <- sample(y[sort(dist(c(val[i], y))[1:sum(missindex)], index.return  = TRUE)$ix 
        #                      %in% 1:5], 1)
        #donors[i] <- sample(y[sort(pdist::pdist(matrix(val[i], ncol = 1), matrix(y, ncol = 1))@dist,
        #                          index.return  = TRUE)$ix %in% 1:5], 1)
        donors[i] <- sample(y[order(pdist::pdist(matrix(val[i], ncol = 1), matrix(y, ncol = 1))@dist)[1:5]], 1)        
        
      }
      donors
    }
    residuals <- selectDonor(na.omit(data[, ynam]), val = pred[missindex]) - pred[missindex] 
    ymiss <- pred[missindex] + residuals  
    
  }

  if(value_back == "ymiss"){
    return(ymiss)
  } else{
    rownames(data) <- rn
    data[missindex, ynam] <- ymiss
    return(data) 
  }
}

getOrigName <- function(tname){
  yname <- stringr::str_extract(string = tname,
                                pattern = "(?<=\\().*(?=\\))")
  if(is.na(yname)){
    return(tname)
  } else {
    return(yname)
  }
}

getTransformation <- function(tname){
  yname <- stringr::str_extract(string = tname,
                       pattern = "(?<=\\().*(?=\\))")
  if(is.na(yname)){
    return(NULL)
  } else {
    transformation <- sub("\\(\\)", "", sub(paste0(yname, "*"), "", tname))
    return(transformation)    
  }
}

backtransform <- function(tname, pred){
  # todo: add correction term
  transformation <- getTransformation(tname)
  if(!is.null(transformation)){
    if(transformation == "log"){
      pred <- exp(pred)
    }
    if(transformation == "log10"){
      pred <- 10^pred
    }
    if(transformation == "sqrt"){
      pred <- pred^2
    }
  } 
  return(pred)
}