library(VIM)

x <- vimpute(sleep, method="ranger", sequential = FALSE, imp_var=TRUE)
# xm <- vimpute(sleep, method="ranger", sequential = FALSE, imp_var=TRUE, median = TRUE)
# 
# y <- vimpute(sleep, method="ranger", sequential = TRUE, imp_var=TRUE)
# z <- vimpute(sleep, method="ranger", sequential = TRUE, imp_var=TRUE,
#              imputation_uncertainty ="PMM_1")
# z <- vimpute(sleep, method="ranger", sequential = FALSE, imp_var=TRUE,
#              imputation_uncertainty ="PMM_5")
# 
# 
# a <- vimpute(sleep, method="xgboost", sequential = FALSE, imp_var=TRUE)
# b <- vimpute(sleep, method="xgboost", sequential = TRUE, imp_var=TRUE)
# c <- vimpute(sleep, method="xgboost", sequential = TRUE, imp_var=TRUE,
#              imputation_uncertainty ="PMM_1")
# 
# 
# a <- vimpute(sleep, method="regression", sequential = FALSE, imp_var=TRUE)
# b <- vimpute(sleep, method="regression", sequential = TRUE, imp_var=TRUE)
# c <- vimpute(sleep, method="regression", sequential = TRUE, imp_var=TRUE,
#              imputation_uncertainty ="PMM_3")
# 
# a <- vimpute(sleep, method="robust", sequential = FALSE, imp_var=TRUE)
# b <- vimpute(sleep, method="robust", sequential = TRUE, imp_var=TRUE)
# c <- vimpute(sleep, method="robust", sequential = TRUE, imp_var=TRUE,
#              imputation_uncertainty ="PMM_3")
# 
# 
# a <- vimpute(sleep, method="robust", sequential = FALSE, imp_var=TRUE)
# b <- vimpute(sleep, method="robust", sequential = TRUE, imp_var=TRUE)
# c <- vimpute(sleep, method="robust", sequential = TRUE, imp_var=TRUE,
#              imputation_uncertainty ="PMM_3", model_uncertainty = "bootstrap", nboot = 50)
# 
# df <- iris
# colnames(df) <- c("S.Length","S.Width","P.Length","P.Width","Species")
# # randomly produce some missing values in the data
# set.seed(1)
# nbr_missing <- 50
# y <- data.frame(row = sample(nrow(iris), size = nbr_missing, replace = TRUE),
#                 col = sample(ncol(iris), size = nbr_missing, replace = TRUE))
# y<-y[!duplicated(y), ]
# df[as.matrix(y)] <- NA
# 
# test8 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"),
#                  method="regression", imp_var=FALSE, sequential = FALSE, imp_suffix="imp")
# test9 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"), imputation_uncertainty = "PMM_3", 
#                  method="regression", imp_var=FALSE, sequential = FALSE, imp_suffix="imp")
# test10 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"),
#                   method="regression", imp_var=FALSE, nseq=10, sequential = TRUE, imp_suffix="imp")
# test11 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"), imputation_uncertainty = "PMM_3", 
#                   method="regression", imp_var=FALSE, nseq=10, sequential = TRUE, imp_suffix="imp")
# test11 <- vimpute(data=df, formula = list(S.Length=~S.Width+P.Length), variable = c("S.Length"), imputation_uncertainty = "PMM_3", 
#                   method="ranger", imp_var=FALSE, nseq=10, sequential = TRUE, imp_suffix="imp")
# 
