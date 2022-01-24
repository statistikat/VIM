message("impPCA")

data(Animals, package = "MASS")
Animals$brain[19] <- Animals$brain[19] + 0.01
Animals <- log(Animals)
colnames(Animals) <- c("log(body)", "log(brain)")
Animals_na <- Animals
probs <- abs(Animals$`log(body)`^2)
probs <- rep(0.5, nrow(Animals))
probs[c(6,16,26)] <- 0
set.seed(1234)
Animals_na[sample(1:nrow(Animals), 10, prob = probs), "log(brain)"] <- NA
w <- is.na(Animals_na$`log(brain)`)
imp1 <- impPCA(Animals_na)
expect_false(any(is.na(imp1[[2]])))
imp2 <- impPCA(Animals_na, method = "mcd")
expect_false(any(is.na(imp2[[2]])))
imp3 <- impPCA(Animals_na, boot = TRUE, m = 10)
expect_false(any(is.na(imp3[[2]])))
imp4 <- impPCA(Animals_na, method = "mcd", boot = TRUE)[[1]]
expect_false(any(is.na(imp4[[2]])))
