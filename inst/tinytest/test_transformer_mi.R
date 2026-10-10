library(VIM)
set.seed(1); d<-data.frame(x=rnorm(40)); d$y<-2*d$x+rnorm(40); d$y[1:10]<-NA
linear_fitter <- function(training,target,seed) {
  fit<-lm(y~x,data=training)
  function(newdata) as.numeric(predict(fit,newdata))
}
set.seed(22); before<-.Random.seed
mi<-transformer_mi(d,"y",m=3,seed=3,fitter=linear_fitter)
expect_identical(.Random.seed,before)
expect_true(inherits(mi,"vimmi"))
completed<-vim_complete(mi,"all")
expect_identical(length(completed),3L)
for(out in completed) {
  expect_identical(out$y[11:40],d$y[11:40])
  expect_true(all(out$y[1:10] %in% d$y[11:40]))
}
expect_false(identical(completed[[1]],completed[[2]]))
expect_equal(transformer_mi(d,"y",m=3,seed=3,fitter=linear_fitter)$imp,mi$imp)
expect_true(inherits(with(mi,lm(y~x)),"mira"))
expect_error(transformer_mi(d,"y",m=1))
expect_error(transformer_mi(d,"y",donors=100))
expect_error(transformer_mi(d,"y",fitter=function(...) function(x) rep(NA_real_,nrow(x))))
report<-VIM:::.transformer_mi_simulation(list(linear=function(d,s)
  transformer_mi(d,"y",m=3,seed=s,fitter=linear_fitter)),replications=3,n=50,mechanisms=c("MCAR","MAR"))
expect_equal(sum(report$summary$failures),0)
expect_true(all(is.finite(report$summary$coverage)))
