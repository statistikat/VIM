library(VIM)
context("kNN exact results")
df <- data.frame(Class=c(rep("A",5),rep("B",5)),
                 X1=1L,X2=rep(c(2L,3L),each=5),
                 ClassNum=rep(c(1L,2L),each=5),
                 Row=1:10,Row2=1:10,
                 ord=ordered(c(rep("A",5),rep("B",5))),
                 stringsAsFactors = FALSE)

test_that("kNN Tests - exact1",{
  df[3,3] <- NA
  df[8,3] <- NA
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("X1", "Class") , trace = TRUE
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

test_that("kNN Tests - exact2",{
  df[3,5] <- NA
  df[8,5] <- NA
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class","X1"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
})


test_that("kNN Tests - exact3",{
  df[3,5] <- NA
  df[8,5] <- NA
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("ord"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==1)
  expect_true(dfImp$Row[8]==6)
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class","ord"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class","X1","ord"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
})

df <- data.frame(Class=c(rep("A",5),rep("B",5)),
                 X1=1L,X2=rep(c(2L,3L),each=5),
                 ClassNum=rep(c(1L,2L),each=5),
                 Row=1:10,Row2=1:10,
                 ord=ordered(c(rep("A",5),rep("B",5))),
                 stringsAsFactors = TRUE)

test_that("kNN Tests - exact1 - stringsAsFactors",{
  df[3,3] <- NA
  df[8,3] <- NA
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("X1", "Class") , trace = TRUE
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

test_that("kNN Tests - exact2 - stringsAsFactors",{
  df[3,5] <- NA
  df[8,5] <- NA
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class","X1"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
})


test_that("kNN Tests - exact3",{
  df[3,5] <- NA
  df[8,5] <- NA
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("ord"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==1)
  expect_true(dfImp$Row[8]==6)
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class","ord"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
  dfImp <- kNN(df 
               ,k = 1
               ,dist_var = c("Row2","Class","X1","ord"), addRandom = FALSE
  )             
  expect_true(dfImp$Row[3]==2)
  expect_true(dfImp$Row[8]==7)
  
})