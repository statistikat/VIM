library(VIM)
d <- data.frame(x=c(1,NA,3),g=factor(c("a","b",NA)))
out <- tabpfnImpute(d,imp_var=FALSE,backend=function(x,categorical,seed) {
  expect_identical(categorical,1L)
  x[2,1]<-2; x[3,2]<-0; x
})
expect_equal(out$x,c(1,2,3)); expect_identical(as.character(out$g),c("a","b","a"))
expect_error(tabpfnImpute(d))
expect_error(tabpfnImpute(d,regressor_path=tempfile()))
expect_error(tabpfnImpute(d,n_permutations=0))
expect_error(tabpfnImpute(d,model_version="unknown"))
expect_error(tabpfnImpute(d,backend=function(x,...) x[,1,drop=FALSE]))
expect_identical(tabpfnImpute(data.frame(x=1:3)),data.frame(x=1:3))
if(identical(Sys.getenv("VIM_RUN_TABPFN_TESTS"),"true")) {
  path <- Sys.getenv("VIM_TABPFN_REGRESSOR")
  if(!nzchar(path)) stop("Real tests require a local VIM_TABPFN_REGRESSOR checkpoint")
  set.seed(2); d<-data.frame(x=rnorm(30),y=rnorm(30)); d$y[1:2]<-NA
  out<-tabpfnImpute(d,n_estimators=1,regressor_path=path,imp_var=FALSE)
  expect_false(anyNA(out)); expect_identical(out$y[-(1:2)],d$y[-(1:2)])
}
