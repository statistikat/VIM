library(VIM)
d <- data.frame(x=LETTERS[1:6],y=as.double(1:6),z=as.double(1:6),w=ordered(LETTERS[1:6]))
d <- rbind(d,d)
setna <- function(d,i,col=2){
  d[i,col] <- NA
  d
}
wm <- function(x,weights)weighted.mean(x,weights)
## Tests for k==1
test_that("kNN Tests for k==1",{
  d1 <- kNN(setna(d,7:12,1)[,1:2],k=1)
  d2 <- kNN(setna(d,7:12,2)[,1:2],k=1)
  d3 <- kNN(setna(d,7:12,3)[,1:3],k=1,dist_var = c("x","y"),variable = "z")
  d4 <- kNN(setna(d,7:12,3)[-1,3:4],k=1)
  expect_false(!all(d1[7:12,1]==d1[1:6,1])||!all(d2[7:12,2]==d2[1:6,2])||
                 !all(d3[7:12,3]==d3[1:6,3])||!(all(d4[6:11,1]==d4[c(1,1:5),1])),info=
                 "kNN does not work as supposed.")
})

## Tests for k==2
d <- rbind(d,d[1:6,])
d[13:18,2] <- d[13:18,2]*2
test_that("kNN Tests for k==2",{
  d2 <- kNN(setna(d,7:12,2)[,1:2],k=2,numFun = mean, useImputedDist = FALSE)
  expect_false(!all(d2[7:12,2]==(d2[1:6,2]+d2[13:18,2])/2),info=
                 "kNN does not work as supposed.")
})

d$m <- rnorm(6)
d$m[1:2] <- 0
## Tests for k==5
test_that("kNN Tests for k==5 with mixed variable",{
  d <- setna(d,7:12,2)
  d <- setna(d,1:2,1)
  d <- setna(d,3:4,5)
  d2 <- kNN(d,k=5,numFun = wm,weightDist = TRUE, catFun = sampleCat, mixed = "m",
            addRandom = TRUE)
  
})

test_that("kNN Tests donorcond",{
  d <- setna(d,7:12,2)
  d2 <- kNN(d,k=5,donorcond = list(">3"),variable = "y")
  expect_true(all(d2[d2$y_imp,"y"]>3))
})


test_that("kNN All values NAs",{
  d <- setna(d,1:nrow(d),2)
  expect_warning(d2 <- kNN(d,k=5,donorcond = list(">3"),variable = "y"))
  expect_identical(d,d2[1:ncol(d)])
})


## Tests for random forest as distance variable
test_that("kNN Tests - randomForest",{
  d <- setna(d,7:12,2)
  d <- setna(d,1:2,1)
  d <- setna(d,3:4,5)
  d2 <- kNN(d,addRF = TRUE)
  
})
test_that("kNN Tests - randomForest onlyRF",{
  d <- setna(d,7:12,2)
  d <- setna(d,1:2,1)
  d <- setna(d,3:4,5)
  d2 <- kNN(d,addRF = TRUE,onlyRF = TRUE)
  
})
