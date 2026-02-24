#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param formulas PARAM_DESCRIPTION, Default: vector("list", ncol(data))
#' @param data PARAM_DESCRIPTION
#' @param boot PARAM_DESCRIPTION, Default: TRUE
#' @param robustboot PARAM_DESCRIPTION, Default: TRUE
#' @param method PARAM_DESCRIPTION, Default: 'lts'
#' @param multinom.method PARAM_DESCRIPTION, Default: 'multinom'
#' @param takeAll PARAM_DESCRIPTION, Default: TRUE
#' @param eps PARAM_DESCRIPTION, Default: 0.5
#' @param maxit PARAM_DESCRIPTION, Default: 4
#' @param alpha PARAM_DESCRIPTION, Default: 0.5
#' @param uncert PARAM_DESCRIPTION, Default: 'pmm'
#' @param familiy PARAM_DESCRIPTION, Default: 'Gaussian'
#' @param value_back PARAM_DESCRIPTION, Default: 'matrix'
#' @param trace PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[VIM]{initialise}}
#'  \code{\link[robCompositions]{outCoDa}}
#' @rdname imputeRobustChain
#' @export 
#' @importFrom VIM initialise
imputeRobustChain <- function(formulas = vector("list", ncol(data)), 
                              data, 
                              boot = TRUE,
                              robustboot = TRUE,
                              method = "lts",
                              multinom.method = "multinom",
                              takeAll = TRUE,
                              eps = 0.5,
                              maxit = 4,
                              alpha = 0.5, 
                              uncert = "pmm", 
                              familiy = "Gaussian", 
                              value_back = "matrix", 
                              trace = FALSE){
  supportedMethods <- c("lts", "lm", "gam", "MM")
  #mf <- model.frame(form, data = data)
  #y <- model.extract(mf, "response")
  #x_vars <- all.vars(form[-1]) 
  #y_var <- all.vars(form[1])
  rn <- rownames(data)
  n <- nrow(data)
  p <- ncol(data)
  #ynam <- as.character(form[2])
  #my <- is.na(data[, ynam])
  M <- is.na(data)
  m <- 1 # currently not further considered
  
  class1 <- function(x) class(x)[1]
  types <- lapply(data, class1)
  
  attributes(types)$names <- NULL
  types <- unlist(types)
  if (any(types == "character")) {
    chr_ind <- which(types == "character")
    warning("At least one character variable is converted into a factor")
    for (ind in chr_ind) {
      data[, ind] <- as.factor(data[, ind])
      types[ind] <- "factor"
    }
  }
  
  #determine factor type: dichotomous or polytomous
  #detect problematic factors
  ind_fac <- which(types == "factor")
  for (ind in ind_fac) {
    #get number of levels
    fac_nlevels <- nlevels(data[[ind]])
    if (fac_nlevels < 2)
      stop(sprintf("factor with less than 2 levels detected! - `%s`", names(data)[ind]))
    types[ind] = ifelse(fac_nlevels == 2, "binary", "nominal")
  }
  ind_ord <- which(types == "ordered")
  for (ind in ind_ord) {
    #get number of levels
    fac_nlevels <- nlevels(data[[ind]])
    if (fac_nlevels == 2)
      types[ind] <- "binary"
  }
  
  vars_miss <- apply(data, 2, function(x) any(is.na(x)))
  
  # initialize if takeAll
  if(takeAll){
    data <- VIM::initialise(data, mixed = NULL)
  }
  if(length(ind_fac) > 0){
    outCoDa <- function(x, quantile=0.975, method="robust", 
                        alpha = 0.5, coda = TRUE){
      ## Funktion from the same author from package robCompositions
      if(dim(x)[2] < 2) stop("need data with at least 2 variables")
      if(alpha < 0.5 | alpha > 1) stop("allowed values for h are between 0.5 and 1")
      covEst <- function(x, type) {
        standard <- function(x){
          list(mean=colMeans(x, na.rm=TRUE), varmat=cov(x))  
        }
        robust <- function(x){
          v <- robustbase::covMcd(x, alpha = alpha)
          list(mean=v$center, varmat=v$cov)
        }
        robustHD <- function(x){
          v <- robustbase::covOGK(x, sigmamu = robustbase::s_Qn)
          list(mean=v$center, varmat=v$cov)
        }
        switch(type,
               standard = standard(x),
               robust = robust(x),
               robustHD = robustHD(x))
      }
      if(!is.logical(coda) & !is.function(coda)){
        stop("coda must be logical or function")
      }
      if(!is.logical(coda)){
        x <- coda(x)
      }	else if (coda){ 
        x <- pivotCoord(x)
      }  
      cv <- covEst(x, "robust")
      cvc <- covEst(x, "standard")
      dM <- sqrt(mahalanobis(x, center=cv$mean, cov=cv$varmat))
      dMc <- sqrt(mahalanobis(x, center=cvc$mean, cov=cvc$varmat))
      #	limit <- sqrt(qchisq(p=quantile, df=ncol(x)-1))
      limit <- sqrt(qchisq(p=quantile, df=ncol(x)))	
      res <- list(mahalDist = dM, limit = limit, 
                  outlierIndex = dM > limit, method=method, 
                  om2 = dMc > limit,
                  m2=dMc, coda = coda)
      class(res) <- "outCoDa"
      invisible(res)
    }
    out_index <- outCoDa(data[, types %in% c("integer", "numeric", "count")])$outlierIndex
  } else{
    out_index <- NULL
  }
  
  ## outer loop
  criteria <- 999999
  iterations <- 0
  while(criteria > eps & iterations < maxit){
    iterations <- iterations + 1
    if (trace) {
      if (Sys.info()[1] == "Windows") flush.console()
      message("----------------------")
      message(paste("start of iteration =", iterations))
    }
    data_previous <- data
    ## inner loop
    for(i in (1:p)[vars_miss]){
      if (trace) {
        message(paste("impute variable:", i, "(", colnames(data)[i], ")"))
        if (Sys.info()[1] == "Windows") flush.console()
      }
      
      meth <- switch(
        types[i],
        integer = "numeric",
        numeric = "numeric",
        # mixed = "numeric",
        binary = "bin",
        nominal = "factor",
        count = "count",
        ordered = "ordered",
        logical = "bin",
        stop("unsupported variable type for column ", i)
      )
      
      ## replace initialised missings:
      if (is.null(formulas[[i]])) {
        xvars <- colnames(data)[-i]
        formulas[[i]] <- formula(paste0(colnames(data)[i], "~", 
                                        paste(xvars, collapse = "+")))
      }
      if (trace) {
        message(paste("formula:", format(formulas[[i]])))
        if (Sys.info()[1] == "Windows") flush.console()
      }
      if (trace){
        if(meth == "numeric"){
          message(paste(meth, method))           
        } else{
          message(meth)
        }
      }
      data[M[, i], i] <- imp(
        form = formulas[[i]], 
        data, 
        type = types[i],
        method = method,
        multinom.method = multinom.method,
        index = M[, i], #mixed_tf = mixed_tf, mixed_constant = mixed_constant,
        factors = factors, #step = step, robust = robust, noise = FALSE,
        boot = boot,
        robustboot = robustboot,
        uncert = uncert,
        outIndex = out_index
        #force = force, robMethod, form = active_formula,
        #multinom.method = multinom.method
      )
      #if(!testdigits(x$x5)) stop()
      
    }
    if (any(types %in% c("numeric", "mixed")))
      d <- sum( (data_previous[, types %in% "numeric"] -
                   data[, types %in% "numeric"]) ^ 2,
                na.rm = TRUE) 
    if (any(!types %in% c("numeric")))
      d <- d + 
      sum(data_previous[, !types %in% c("numeric")] != 
            data[, !types %in% c("numeric")])
  }
  if (trace) {
    if (Sys.info()[1] == "Windows") flush.console()
    message(paste("final value of criteria =", d))
    message("----------------------")
  }
  return(data)
}

## switch function to automatically select methods
imp <- function(form, data, type, method, multinom.method, index, factors, boot, robustboot, uncert, outIndex) {
  switch(
    type,
    numeric = useRobustNumeric(form, data, method, index, factors, boot, robustboot, uncert),
    nominal =   useRobustMN(form, data, index, factors, boot, robustboot, uncert, 
                            multinom.method = multinom.method, outIndex),
    bin = useLogistic(x_reg, ndata, index, factors, step, robust, form = form),
    count   = useGLMpoisson(x_reg, ndata, index, factors, step, robust,
                            form = form)#,
    #ordered  = useOrd(x_reg, ndata, index, factors, step, robust, form = form),
  )
}


# categorical response
useRobustMN <- function(form, data, index, factors, boot, robustboot, uncert, multinom.method, outIndex){
  
  n <- nrow(data)
  mf <- model.frame(form, data = data)
  y <- model.extract(mf, "response")
  x_vars <- all.vars(form[-1]) 
  y_var <- all.vars(form[1])
  ynam <- as.character(form[2])
  
  # factors <- Inter(list(colnames(x_reg), factors))
  # if (length(factors) > 0) {
  #   for (f in 1:length(factors)) {
  #     if (any(summary(x_reg[, factors[f]]) == 0)) {
  #       x_reg <- x_reg[, -which(colnames(x_reg) == factors[f])]
  #       ndata <- ndata[, -which(colnames(ndata) == factors[f])]
  #     }
  #   }
  # }
  # form <- form[form %in% names(x_reg)]
  if (multinom.method == "multinom") {
    # browser()
    co <- capture.output(multimod <- nnet::multinom(
      form, data = data, summ = 2, maxit = 50, trace = FALSE, MaxNWts = 50000))
    if(boot){
      if(robustboot){
        boot_idx <- sample(x = which(outIndex), size = n, replace = TRUE)
      } else {
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE)       
      }
      boot_dat <- data[boot_idx, ]
      mod <- lm(form, data = boot_dat)
    } else {
      mod <- lm(form, data = data[mod$best, ])  
    }
    # impnew <- predict(multimod, newdata = data[index, ])
    prob_predictions <- predict(multimod, newdata = data[index, ], type = "probs")
    impnew <- apply(prob_predictions, 1, function(x) sample(names(x), 1, prob = x))
  } else if(multinom.method == "enetLTS"){
    stop("enetLTS not implemented yet.")
    multimod <- enetLTS::enetLTS(xx = mf[, 2:ncol(mf)], yy = mf[, 1])
  } else if(multinom.method == "robmixglm"){
    stop("robmixglm not implemented yet")
    multimod <- robmixglm::robmixglm(form, data = data, family = "poisson")
  }
  return(impnew)
}


useRobustNumeric <- function(form, data, method, index, factors, boot, robustboot, uncert) 
  #            mixed_tf, mixed_constant, factors, step,
  # robust, noise, noise.factor, force, robMethod, form) 
{
  n <- nrow(data)
  mf <- model.frame(form, data = data)
  y <- model.extract(mf, "response")
  x_vars <- all.vars(form[-1]) 
  y_var <- all.vars(form[1])
  ynam <- as.character(form[2])
  
  if(method == "lts"){
    mod <- ltsReg(form, data = data)
    if(boot){
      if(robustboot){
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
      if(robustboot){
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
      if(robustboot){
        boot_idx <- sample(1:n, size = n, replace = TRUE, prob = mod$rweights)       
      } else {
        boot_idx <- sample(1:n, size = n, replace = TRUE)           
      }
      boot_dat <- data[boot_idx, ]
      mod <- robustbase::lmrob(form, data = boot_dat)     
    }
    sdev <- summary(mod)$scale
  } else if(method == "gam"){
    if (requireNamespace("mgcv", quietly = TRUE)) {
      mgcv::gam(form, data = data)
    } else {
      stop("Package 'mgcv' is required for this function.")
    }
    if(boot){
      idx <- which(resid(mod) < quantile(resid(mod), 0.5))
      if(robustboot){
        boot_idx <- sample(x = idx, size = n, replace = TRUE)       
      } else{
        boot_idx <- sample(x = 1:n, size = n, replace = TRUE)         
      }
      boot_dat <- data[boot_idx, ]
      if (requireNamespace("mgcv", quietly = TRUE)) {
        mgcv::gam(form, data = boot_dat)  
      } else {
        stop("Package 'mgcv' is required for this function.")
      }
    }
    sdev <- summary(mod)$scale
  } else {
    cat("----\n")
    stop(paste("Method not supported, \n   choose one of", paste(supportedMethods, collapse = ", "), "\n----\n"))
  }
  
  
  # beta <- coef(mod)
  # V <- vcov(mod)
  # Cv <- chol(V)
  # 
  # nus <- rnorm(m * length(beta))
  # beta_sims <- beta + 
  #   t(Cv) %*% 
  #   matrix(nus, nrow = length(beta), ncol = m)
  # # covar_sim <- predict(mod, newdata = boot_dat, type = "lpmatrix")
  pred <-  predict(mod, newdata = data)
  # linpred_sim <- covar_sim %*% beta_sims
  # invlink <- function(x) x
  # exp_val_sim <- invlink(linpred_sim)
  if(uncert == "normalerror"){
    ymiss <- pred[index] + rnorm(n = sum(index), 
                                 mean = 0,#exp_val_sim, 
                                 sd = sdev)
  }
  if(uncert == "resid"){
    ymiss <- pred[index] + sample(resid(mod), size = sum(index))   
  }
  if(uncert == "wresid"){
    ymiss <- rep(NA, sum(index))
    cnt <- 0
    for(i in (1:n)[index]){
      cnt <- cnt + 1
      # d <- pdist::pdist(as.matrix(data[i, x_vars, drop = FALSE]), 
      #                   as.matrix(data[, x_vars, drop = FALSE]))
      mod_matrix <- as.matrix(model.matrix(mf, data = data))
      my_distance_function <- function(x, y) {
        if (!requireNamespace("pdist", quietly = TRUE)) {
          stop("Package 'pdist' must be installed to use this function.")
        }
        d <- pdist::pdist(mod_matrix[i, ,drop = FALSE], 
                                 mod_matrix) 
      }
      d <- 1 - d@dist / max(d@dist)
      d <- sqrt(d)
      r <- resid(mod)
      p <- pred[i]
      ymiss[cnt] <- pred[i] + 
        sample(resid(mod), 
               size = 1, prob = d)     
    }
  }
  if(uncert == "pmm"){
    selectDonor <- function(y, val){
      donors <- numeric(length(val))
      for(i in 1:length(val)){
        # donors[i] <- sample(y[sort(abs(val - y), index.return  = TRUE)$ix 
        #                       %in% 1:5], 1)
        #donors[i] <- sample(y[sort(dist(c(val[i], y))[1:sum(index)], index.return  = TRUE)$ix 
        #                      %in% 1:5], 1)
        #donors[i] <- sample(y[sort(pdist::pdist(matrix(val[i], ncol = 1), matrix(y, ncol = 1))@dist,
        #                          index.return  = TRUE)$ix %in% 1:5], 1)
        my_distance_function <- function(x, y) {
          if (!requireNamespace("pdist", quietly = TRUE)) {
            stop("Package 'pdist' must be installed to use this function.")
          }
          donors[i] <- sample(y[order(pdist::pdist(matrix(val[i], ncol = 1), matrix(y, ncol = 1))@dist)[1:5]], 1)        
          
        }

      }
      donors 
    }
    residuals <- selectDonor(na.omit(y), val = pred[index]) - pred[index] 
    ymiss <- pred[index] + residuals  
    
  }
  value_back <- "ymiss" # currently the only option
  
  # backtransform to original scale
  trans <- getTransformation(ynam)
  yname_orig <- getOrigName(ynam)
  data[index, yname_orig] <- ymiss
  ymiss <- backtransform(ynam, ymiss) 
  
  if(value_back == "ymiss"){
    return(ymiss)
  } else{
    rownames(data) <- rn
    return(data) 
  }
}

