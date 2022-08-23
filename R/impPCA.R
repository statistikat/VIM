#' Iterative EM PCA imputation
#'
#' Greedy algorithm for EM-PCA including robust methods
#'
#' @param x data.frame or matrix
#' @param method `"classical"` or `"mcd"` (robust estimation)
#' @param eps threshold for convergence
#' @param m number of multiple imputations (only if parameter `boot` equals `TRUE`)
#' @param k number of principal components for reconstruction of `x`
#' @param maxit maximum number of iterations
#' @param boot residual bootstrap (if `TRUE`)
#' @param verbose TRUE/FALSE if additional information about the imputation
#' process should be printed
#' @return the imputed data set. If `boot = FALSE` this is a data.frame. 
#' If `boot = TRUE` this is a list where each list element contains a data.frame. 
#' @author Matthias Templ
#' @references Serneels, Sven and Verdonck, Tim (2008). 
#' Principal component analysis for data containing outliers and missing elements. 
#' Computational Statistics and Data Analysis, Elsevier, vol. 52(3), pages 1712-1727
#' @keywords manip
#' @family imputation methods
#' @examples
#'
#' data(Animals, package = "MASS")
#' Animals$brain[19] <- Animals$brain[19] + 0.01
#' Animals <- log(Animals)
#' colnames(Animals) <- c("log(body)", "log(brain)")
#' Animals_na <- Animals
#' probs <- abs(Animals$`log(body)`^2)
#' probs <- rep(0.5, nrow(Animals))
#' probs[c(6,16,26)] <- 0
#' set.seed(1234)
#' Animals_na[sample(1:nrow(Animals), 10, prob = probs), "log(brain)"] <- NA
#' w <- is.na(Animals_na$`log(brain)`)
#' impPCA(Animals_na)
#' impPCA(Animals_na, method = "mcd")
#' impPCA(Animals_na, boot = TRUE, m = 10)
#' impPCA(Animals_na, method = "mcd", boot = TRUE)[[1]]
#' plot(`log(brain)` ~ `log(body)`, data = Animals, type = "n", ylab = "", xlab="")
#' mtext(text = "impPCA robust", side = 3)
#' points(Animals$`log(body)`[!w], Animals$`log(brain)`[!w])
#' points(Animals$`log(body)`[w], Animals$`log(brain)`[w], col = "grey", pch = 17)
#' imputed <- impPCA(Animals_na, method = "mcd", boot = TRUE)[[1]]
#' colnames(imputed) <- c("log(body)", "log(brain)")
#' points(imputed$`log(body)`[w], imputed$`log(brain)`[w], col = "red", pch = 20, cex = 1.4)
#' segments(x0 = Animals$`log(body)`[w], x1 = imputed$`log(body)`[w], y0 = Animals$`log(brain)`[w],
#' y1 = imputed$`log(brain)`[w], lty = 2, col = "grey")
#' legend("topleft", legend = c("non-missings", "set to missing", "imputed values"),
#' pch = c(1,17,20), col = c("black","grey","red"), cex = 0.7)
#' mape <- round(100* 1/sum(is.na(Animals_na$`log(brain)`)) * sum(abs((Animals$`log(brain)` -
#' imputed$`log(brain)`) / Animals$`log(brain)`)), 2)
#' s2 <- var(Animals$`log(brain)`)
#' nrmse <- round(sqrt(1/sum(is.na(Animals_na$`log(brain)`)) * sum(abs((Animals$`log(brain)` -
#' imputed$`log(brain)`) / s2))), 2)
#' text(x = 8, y = 1.5, labels = paste("MAPE =", mape))
#' text(x = 8, y = 0.5, labels = paste("NRMSE =", nrmse))
#'
#' @export
impPCA <- function(x, method = "classical", m = 1, eps = 0.5, 
                   k = ncol(x) - 1, maxit = 100, boot = FALSE,
                   verbose = TRUE){
  ### x ... Matrix or data.frame with missings
  ### method ... mcd, classical, gridMAD
  ### eps ... convergence criteria
  ### all.obs ... TRUE, if the whole observation should be replaced with the pca-estimate
  ###
  ### Matthias Templ
  ### original from 10.09.2006
  ### adapted 31.10.2006
  ### Imputation nach Sven
  ### Statistics Austria
  
  # classx <- class(x)
  classes <- lapply(x, class)
  if(!all(classes %in% c("numeric", "integer"))) stop("only numeric variables allowed")
  
  xorig <- x
  indexMiss <- is.na(x)
  
  #P <- dim(x)[2]
  
  ## erste Schaetzung mit Median:
  #w <- which(is.na(x), arr.ind=TRUE)
  # cm <- colMeans(x, na.rm=TRUE)  ## fuers Ruecktransf.
  # cmed <- apply(x, 2, median, na.rm = TRUE)
  # csd <- apply(x, 2, sd, na.rm = TRUE)   ## fuers Ruecktransf.
  # csdrob <- apply(x, 2, mad, na.rm = TRUE)   ## fuers Ruecktransf.  
  # xorig <- x
  # x <- apply(x, 2, function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))

  ## centering and initialisierung
  # if(method == "classical"){
    cm <- colMeans(x, na.rm=TRUE)  ## fuers Ruecktransf.
    csd <- apply(x, 2, sd, na.rm = TRUE)   ## fuers Ruecktransf.
    x <- apply(x, 2, function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
    # initialise missing values
    for(j in 1:ncol(x)){
      x[indexMiss[,j], j] <- mean(x[, j], na.rm = TRUE) 
    }
  # } else if(method == "mcd"){
  #   cmed <- apply(x, 2, median, na.rm = TRUE)
  #   csdrob <- apply(x, 2, mad, na.rm = TRUE)   ## fuers Ruecktransf.
  #   x <- apply(x, 2, function(x) (x - median(na.omit(x))/mad(na.omit(x))))
  #   # initialise missing values
  #   for(j in 1:ncol(x)){
  #     x[indexMiss[,j], j] <- median(na.omit(x[, j]))
  #   }
  # }
  
  ### PCA, Iteration:
  d <- 1000000
  it <- 0
  
  if(!boot){

    while(d > eps & it <= maxit){
      it <- it + 1
      if( method == "mcd" ){ 
        xMcd <- robustbase::covMcd(x)
        p <- princomp(x, covmat=xMcd) 
      }
      if( method == "classical" ){ 
        p <- princomp(x) 
      }
      #if( method == "gridMAD" ){ p <- PCAgrid(x, method="mad", k=ncol(x)) }
      xneu <- p$sco[,1:k] %*% t(p$load[,1:k])  #(p$load[,1:P])   
      ##+rep(1,dim(x)[1])%*%t(xMcd$center)  # p-dim???
      d <- sum(abs(x[indexMiss] - xneu[indexMiss]))    ## Konvergenzkriterium
      x[indexMiss] <- xneu[indexMiss]
    }
    
    
    ### Ruecktrans:
    
    # if(method == "classical"){
      for( i in 1: dim(x)[2] ){
        x[,i] <- (x[,i] * csd[i]) + cm[i]
      }
    # } else {
    #   for( i in 1: dim(x)[2] ){
    #     x[,i] <- (x[,i] * csdrob[i]) + cmed[i]
    #   }
    # }
    if(verbose) message("\nIterations: ", it)
    return(data.frame(x))
  } else { # boot
    # create bootstrap samples
    
    if( method == "mcd" ){ 
      xMcd <- robustbase::covMcd(x)
      p <- princomp(x, covmat=xMcd) 
    }
    if( method == "classical" ){ 
      p <- princomp(x) 
    }
    
    xneu <- p$sco[,1:k] %*% t(p$load[,1:k]) 
    residuals <- x - xneu
    xboot <- x
    residualboot <- function(){
      for(j in 1:ncol(x)){
        xboot[, j] <- x[, j] + sample(residuals[,j], replace = TRUE)
      }
      
      ### PCA, Iteration:
      d <- 1000000
      it <- 0
      
      while(d > eps & it <= maxit){
        it <- it + 1
        if( method == "mcd" ){ 
          xMcd <- robustbase::covMcd(xboot)
          p <- princomp(xboot, covmat=xMcd) 
        }
        if( method == "classical" ){ 
          p <- princomp(xboot) 
        }
        #if( method == "gridMAD" ){ p <- PCAgrid(x, method="mad", k=ncol(x)) }
        xneu <- p$sco[,1:k] %*% t(p$load[,1:k])  
        d <- sum(abs(xboot[indexMiss] - xneu[indexMiss]))    ## Konvergenzkriterium
        xboot[indexMiss] <- xneu[indexMiss]
      }
      
      ### Ruecktrans:
      
      # if(method == "classical"){
        for( i in 1: dim(x)[2] ){
          xboot[,i] <- (xboot[,i] * csd[i]) + cm[i]
        }
      # } else {
      #   for( i in 1: dim(x)[2] ){
      #     xboot[,i] <- (xboot[,i] * csdrob[i]) + cmed[i]
      #   }        
      # }
      # 
      xorig[indexMiss] <- xboot[indexMiss]
      return(xorig)
    }
    
    res <- list()
    for(r in 1:m){
      res[[r]] <- residualboot()    
    }
    if(verbose) message("\nIterations: ", it)
    return(res)
  }
}

# impPCA_boot <- function(x, method = "classical", m = 10 , eps = 0.5, 
#                    k = ncol(x) - 1, maxit = 10){
#   ### x ... Matrix or data.frame with missings
#   ### method ... mcd, classical, gridMAD
#   ### eps ... convergence criteria
#   ### all.obs ... TRUE, if the whole observation should be replaced with the pca-estimate
#   ###
#   ### Matthias Templ
#   ### original from 10.09.2006
#   ### adapted 31.10.2006
#   ### Imputation nach Sven
#   ### Statistics Austria
#   
#   # classx <- class(x)
#   classes <- lapply(x, class)
#   if(!all(classes %in% c("numeric", "integer"))) stop("only numeric variables allowed")
#   
#   indexMiss <- is.na(x)
# 
#   #P <- dim(x)[2]
#   
#   ## erste Schaetzung mit Median:
#   #w <- which(is.na(x), arr.ind=TRUE)
#   cm <- colMeans(x, na.rm=TRUE)  ## fuers Ruecktransf.
#   cmed <- apply(x, 2, median, na.rm = TRUE)
#   csd <- apply(x, 2, sd, na.rm = TRUE)   ## fuers Ruecktransf.
#   xorig <- x
#   x <- apply(x, 2, function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
#   
#   ## Initialisierung
#   if(method == "classical"){
#     for(j in 1:ncol(x)){
#       x[indexMiss[,j], j] <- cm[j] 
#     }
#   } else if(method == "mcd"){
#     for(j in 1:ncol(x)){
#       x[indexMiss[,j], j] <- cmmed[j] 
#     }    
#   }
#   
#   # create bootstrap samples
#   
#   if( method == "mcd" ){ 
#     xMcd <- robustbase::covMcd(x)
#     p <- princomp(x, covmat=xMcd) 
#   }
#   if( method == "classical" ){ 
#     p <- princomp(x) 
#   }
#   
#   xneu <- p$sco[,1:k] %*% t(p$load[,1:k]) 
#   residuals <- x - xneu
#   xboot <- x
#   residualboot <- function(){
#     for(j in 1:ncol(x)){
#       xboot[, j] <- x[, j] + sample(residuals[,j], replace = TRUE)
#     }
#     
#     ### PCA, Iteration:
#     d <- 1000000
#     it <- 0
#     
#     while(d > eps & it <= maxit){
#       it <- it + 1
#       if( method == "mcd" ){ 
#         xMcd <- robustbase::covMcd(xboot)
#         p <- princomp(xboot, covmat=xMcd) 
#       }
#       if( method == "classical" ){ 
#         p <- princomp(xboot) 
#       }
#       #if( method == "gridMAD" ){ p <- PCAgrid(x, method="mad", k=ncol(x)) }
#       xneu <- p$sco[,1:k] %*% t(p$load[,1:k])  
#       d <- sum(abs(xboot[indexMiss] - xneu[indexMiss]))    ## Konvergenzkriterium
#       xboot[indexMiss] <- xneu[indexMiss]
#     }
#   
#     ### Ruecktrans:
#     
#     for( i in 1: dim(x)[2] ){
#       xboot[,i] <- (xboot[,i] * csd[i]) + cm[i]
#     }
#     xorig[indexMiss] <- xboot[indexMiss]
#     return(xorig)
#   }
#   
#   res <- list()
#   for(r in 1:m){
#     res[[r]] <- residualboot()    
#   }
# 
#   return(res)
# }
# 
