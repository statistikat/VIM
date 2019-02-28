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
  d2 <- kNN(setna(d,7:12,2)[,1:2],k=2,numFun = mean, useImputedDist = FALSE,trace=TRUE)
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


test_that("kNN Tests for k==5 with mixed variable",{
  d <- setna(d,7:12,2)
  d <- setna(d,1:2,1)
  d <- setna(d,3:4,5)
  d <- cbind(d,d,d)
  colnames(d)[6:10] <- paste0(colnames(d)[6:10],2)
  colnames(d)[11:15] <- paste0(colnames(d)[6:10],3)
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

### Additional tests for randomForest methods
test_that("kNN Tests - randomForest dist_var vector",{
  d <- setna(d,7:12,2)
  d <- setna(d,1:2,1)
  
  dd <- kNN(d,variable=c("x","y"),
            dist_var=c("z","w"),
            addRF=TRUE,weights=c(1,2))
  
})

#with list of dist_var and weights
test_that("kNN Tests - randomForest list",{
  d <- setna(d,7:12,2)
  d <- setna(d,1:2,1)
  dd <- kNN(d,variable=c("x","y"),
            dist_var=list(c("z","w"),c("z","w")),
            addRF=TRUE,weights=list(c(1,2),c(1,2)))
  
})

test_that("kNN correct distances in different scenarios",{
  df <- structure(list(Class = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 
    2L, 2L, 2L), .Label = c("A", "B"), class = "factor"), X1 = c(1L, 
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), X2 = c(2, 2, NA, 2, 2, 3, 
    3, NA, 3, 3), ClassNum = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
    2L), Row = c(1L, 2L, NA, 4L, 5L, 6L, 7L, NA, 9L, 10L)), row.names = c(NA, 
    -10L), class = "data.frame")
  dfImp <- kNN(df 
               ,variable = c("X2","Row")
               ,k = 1
               ,dist_var = c("X1", "Class") 
  )             
  expect_true(dfImp$X2[3]==2)
  expect_true(dfImp$X2[8]==3)
  dfImp2 <- kNN(df 
               ,variable = c("X2","Row")
               ,k = 1
               ,dist_var = c("Class")) 
  expect_true(dfImp2$X2[3]==2)
  expect_true(dfImp2$X2[8]==3)
  dfImp3 <- kNN(df 
                ,variable = c("X2","Row")
                ,k = 1
                ,dist_var = c("X1", "ClassNum")) 
  expect_true(dfImp3$X2[3]==2)
  expect_true(dfImp3$X2[8]==3)
})

