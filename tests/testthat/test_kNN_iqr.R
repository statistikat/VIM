library(VIM)
context("kNN iqr")
set.seed(1)
d <- data.frame(x=rnorm(100),y=rnorm(100),z=rnorm(100))
setna <- function(d,i,col=2){
  d[i,col] <- NA
  d
}
## Tests for k==1
test_that("kNN Tests for k==1",{
  d1 <- kNN(setna(d,7:12,1),k=1)
  d2 <- kNN(setna(d,7:12,1),k=1, methodStand = "iqr")
  
  expect_false(identical(d1,d2))
})

