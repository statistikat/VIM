library(VIM)
library(laeken)
library(robustbase)
library(mice)
data("Animals_na")

source("~/workspace/VIM/R/imputeRobust.R")

# debugonce(imputeRobust)
imputeRobust(data = Animals_na,
             form = formula("lbrain ~ lbody"), 
             method = "lm", uncert = "wresid", boot = FALSE)

source("~/workspace/vimbook/R/ani_plot.R")

data(Animals, package = "MASS")
ani_plot(Animals = Animals, method = "imputeRobust-lts-pmm", mainlab = NULL, 
         seed = 1)
ani_plot(Animals = Animals, method = "imputeRobust-lts-pmm-chain", mainlab = NULL, 
         seed = 1)


ani_plot(Animals = Animals, method = "imputeRobust-lts-normal", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-resid", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-wresid", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-pmm-nboot", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-normal-nboot", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-resid-nboot", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-wresid-nboot", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-pmm-noboot", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-normal-noboot", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-resid-noboot", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lts-wresid-noboot", mainlab = NULL, seed = 123)


ani_plot(Animals = Animals, method = "imputeRobust-gam-pmm", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-gam-normal", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-gam-resid", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-gam-wresid", mainlab = NULL, seed = 123)

ani_plot(Animals = Animals, method = "imputeRobust-lm-pmm", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lm-normal", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lm-resid", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-lm-wresid", mainlab = NULL, seed = 123)

ani_plot(Animals = Animals, method = "imputeRobust-MM-pmm", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-MM-normal", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-MM-resid", mainlab = NULL, seed = 123)
ani_plot(Animals = Animals, method = "imputeRobust-MM-wresid", mainlab = NULL, seed = 123)

debugonce("imputeRobustChain")

Animals_na$lbody[1:3] <- NA
xi <- imputeRobustChain(formulas = list(formula("lbody ~ lbrain"), 
                                  formula("lbrain ~ lbody")), 
                  data = Animals_na, 
                  boot = TRUE,
                  robustboot = TRUE,
                  method = "lts",
                  takeAll = TRUE,
                  eps = 0.5,
                  maxit = 4,
                  alpha = 0.5, 
                  uncert = "pmm", 
                  familiy = "Gaussian", 
                  value_back = "matrix", 
                  trace = TRUE)

data(iris)
iris[1:3, 1] <- iris[4:6, 2] <- iris[5:7, 3] <- iris[7:9, 4] <- NA
iris[9:11, 5] <- NA
debugonce("imputeRobustChain")
xi <- imputeRobustChain(formulas = list(formula("Sepal.Length ~ ."),
                                        NULL, NULL, NULL, formula("Species ~ .")), 
                        data = iris, 
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
                        trace = TRUE)



## blasting

library(stmo)
data(blasting)
head(blasting)

lm_blasting1 <- lm(log(vibration) ~ log(distance) + log(load),
                  data = blasting)
summary(lm_blasting1)
lm_blasting2 <- lm(log10(vibration) ~ log10(distance) * location + load,
          data = blasting)
summary(lm_blasting2)

blasting$vibration[sample(1:nrow(blasting), 7)] <- NA
w <- complete.cases(blasting)

forms <- list(NULL, NULL, NULL, formula("log10(vibration) ~ log10(distance) * location + load"))
debugonce("imputeRobustChain")
xi <- suppressWarnings(imputeRobustChain(formulas = forms, data = blasting, trace = TRUE, maxit = 1, uncert = "resid"))
xi2 <- suppressWarnings(imputeRobustChain(data = blasting, trace = TRUE, maxit = 20))

plot(xi[, c("vibration", "distance")], col = w + 3)
plot(xi2[, c("vibration", "distance")], col = w + 3)

library(mice)
x2 <- complete(mice(blasting, m = 1))

plot(x2[, c("vibration", "distance")], col = w + 3)


f <- formula("log10(vibration) ~ log10(distance) * location + load")
fn <- f[2]
cn <- "vibration"

data("cherry")
cherry.lm <- lm(log(volume) ~ log(diameter) + log(height), data = cherry)

data("leafburn", package = "faraway")
pairs(leafburn)
# log10(burntimei) = β0 + β1nitrogeni + β2log10(chlorine)i + β3potassiumi +

data(wood, package = "robustbase")
?wood

setMissOut <- function(x, vars = 1:ncol(x), r = rep(0.1, ncol(x))){
  # outlier detection
  o <- robCompositions::outCoDa(x, coda = FALSE, quantile = 0.99)
  n_outliers <- sum(o$outlierIndex)
  n_nonoutliers <- sum(!o$outlierIndex)  
  # sort data
  x_na <- x_orig <- rbind(x[!o$outlierIndex, ], x[o$outlierIndex, ])
  # set missings in all variables
  # but not in outliers
  n <- nrow(x)
  n_good <- sum(!o$outlierIndex)
  n_out <- sum(o$outlierIndex)
  for(i in vars){
    s <- sample(1:n_good, round(r[i] * n), replace = FALSE)
    x_na[s, i] <- NA 
  }
  m1 <- is.na(x_na[, vars])
  return(x_na)
}

wood_na <- setMissOut(wood)
xi <- imputeRobustChain(data = wood_na)



setMiss <- function(x, kind = "MCAR", rate = 0.3){
  # simple function assuming x has variables Y and X
  SEQ <- 1:nrow(x)
  n <- nrow(x) 
  p <- ncol(x)
  if(kind == "MCAR"){
    s <- sample(SEQ, size = round(rate * n, 0))
  }
  if(kind == "MAR"){
    s <- sample(SEQ, size = round(rate * n, 0), prob = (x$X+abs(min(x$X))+1)/sum(x$X+abs(min(x$X))+1))
  }
  if(kind == "MNAR"){
    s <- sample(SEQ, size = round(rate * n, 0), prob = (x$Y+abs(min(x$Y))+1)/sum(x$Y+abs(min(x$Y))+1))
  }
  x$Y[s] <- NA
  return(x)
}


# imputeRobust(x = Animals_na,
#              formulas = list(formula("lbody ~ lbrain"),
#                              formula("lbrain ~ lbody")))
# 

# head(Animals_na)
# aggr(Animals_na)
# First algorithm (fit robust and draw robust bootrap samples, 
#   then classical fit)

# 
# bootfunc.robust <- function (n) 
# {
#   random <- sample.int(n, replace = TRUE)
#   as.numeric(table(factor(random, levels = seq_len(n))))
# }
# 
# 
# imputeRobust <- function(x,
#                          formulas = NULL, 
#                          eps = 0.1, 
#                          maxit = 5, 
#                          mi = 1){
#   
#   # Missing index M
#   M <- is.na(x)
#   n <- nrow(x)
#   p <- ncol(x)
#   
#   # colswithNA
#   colswithNA <- apply(x, 2, function(x){any(is.na(x))})
#   
#   # Initialize
#   x <- kNN(x, imp_var = FALSE)
#   
#   
#   
#   for(i in 1:p){
#     if(colswithNA[i]){
#       if(!is.null(formulas[[i]])){
#         mf <- model.frame(formulas[[i]], data = x)
#         y <- model.extract(mf, "response")
#         y_name <- all.vars(formulas[[i]])[1]
#         X_names <- all.vars(formulas[[i]])[-1]
#         # robust fit
#         mod <- ltsReg(formulas[[1]], data = x)
#         # robust bootstrap sample
#         bs <- sample(x = mod$best, size = n, replace = TRUE)
#         # Fit a OLS regression
#         mod2 <- lm(formulas[[i]], data = x[bs, ])
#         # Predict/Update missings including imputation uncertainty
#         x[M[, i], y_name] <- predict(mod2, 
#                                      newdata = x[M[, i], X_names, drop = FALSE])
#       }
#       
#     }
#   }
#   return(x)
# }
# 
# debugonce(imputeRobust)
# imputeRobust(x = Animals_na, 
#              formulas = list(formula("lbody ~ lbrain"), 
#                              formula("lbrain ~ lbody")))

# 
# 
# # 1. calulate M
# # M <- is.na(data[, c(y_name, X_names)])
# # calculate m
# m <- is.na(y)
# n <- nrow(Animals_na)
# 
# # 2. initialize with knn
# x <- kNN(data[, c(y_name, X_names], imp_var = FALSE)])
# 
# # 3. LTS-regression
# mod <- ltsReg(lbrain ~ lbody, data = Animals_na)
# 
# # 4. Draw a bootstrap sample of size n
# #    from non-outliers
# bs <- sample(x = mod$best, size = n, replace = TRUE)
# 
# # 5. Fit a OLS regression
# mod2 <- lm(lbrain ~ lbody, data = Animals[bs, ])
# 
# # 6. Predict/Update missings including imputation uncertainty
# Animals[M, "lbrain"] <- predict(mod2, 
#                                 newdata = data.frame("lbody" = 1))
# 
# }
# 
# x <- Animals_na
# formulas <- list(formula("lbrain ~ lbody", data = x), 
#                  formula("lbody ~ lbrain", data = x))
# 
# 
# form <- formula(lbrain ~ lbody, data = Animals_na)
# mf <- model.frame(form, data = Animals_na)
# debugonce(imputeRobust)
# imputeRobust(lbrain ~ lbody, data = Animals_na)
# 
# 
# # 1. calulate M
# M <- is.na(Animals_na$lbrain)
# n <- nrow(Animals_na)
# 
# # 2. initialize with knn
# Animals <- kNN(Animals_na, imp_var = FALSE)
# 
# # 3. LTS-regression
# mod <- ltsReg(lbrain ~ lbody, data = Animals_na)
# 
# # 4. Draw a bootstrap sample of size n
# #    from non-outliers
# bs <- sample(x = mod$best, size = n, replace = TRUE)
# 
# # 5. Fit a OLS regression
# mod2 <- lm(lbrain ~ lbody, data = Animals[bs, ])
# 
# # 6. Predict/Update missings including imputation uncertainty
# Animals[M, "lbrain"] <- predict(mod2, 
#                                 newdata = data.frame("lbody" = 1))
# 
# }
# 
# # Second algorithm (draw calibrated bootstrap)
# 
# 
# # Third algorithm (draw bootrap sample and fit robust)