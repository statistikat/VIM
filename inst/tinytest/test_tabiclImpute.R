library(VIM)
d <- data.frame(id=1:4,x=c(1,NA,3,4),g=factor(c("a","b",NA,"a")))
backend <- function(x,categorical,seed) {
  expect_identical(ncol(x),2L)
  expect_identical(categorical,1L)
  x[is.na(x[,1]),1] <- 2
  x[is.na(x[,2]),2] <- 1
  x
}
out <- tabiclImpute(d,id_vars="id",backend=backend,imp_var=FALSE)
expect_equal(out$x,c(1,2,3,4)); expect_identical(as.character(out$g),c("a","b","b","a"))
expect_identical(out$id,d$id); expect_identical(levels(out$g),levels(d$g))
expect_error(tabiclImpute(d,n_iterations=0))
expect_error(tabiclImpute(d,temperature=-1))
expect_error(tabiclImpute(d,id_vars="id",backend=function(x,...) {x[,2]<-99;x}))
expect_identical(tabiclImpute(data.frame(x=1:4)),data.frame(x=1:4))
if(identical(Sys.getenv("VIM_RUN_TABICL_TESTS"),"true")) {
  set.seed(1); d <- data.frame(x=rnorm(30),y=rnorm(30)); d$y[1:2] <- NA
  out <- tabiclImpute(d,n_estimators=1,n_iterations=1,allow_download=FALSE,imp_var=FALSE)
  expect_false(anyNA(out)); expect_identical(out$y[-(1:2)],d$y[-(1:2)])
}
