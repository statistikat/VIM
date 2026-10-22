if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                 "paradox", "R6", "future"), requireNamespace, logical(1), quietly=TRUE)))
  exit_file("mlr3 stack not installed")
library(VIM)

# One categorical schema, with ordered/unused levels and unknown predictions.
frame <- VIM:::.tabicl_input(data.frame(
  number=c(1,NA,100), group=ordered(c("b",NA,"new"),levels=c("b","a","new"))),
  list(number=NULL,group=c("a","b","unused")))
expect_true(is.data.frame(frame))
expect_true(is.factor(frame$group))
expect_false(is.ordered(frame$group))
expect_identical(levels(frame$group),c("a","b","unused"))
expect_identical(as.character(frame$group),c("b",NA_character_,NA_character_))
expect_true(is.na(frame$number[2]))
expect_true(is.matrix(VIM:::.tabicl_input(frame,
  list(number=NULL,group=c("a","b","unused")),input_type="matrix")))
expect_error(VIM:::.tabicl_input(frame,list(),input_type="other"),"input_type")

seen <- new.env(); seen$train <- list();seen$predict <- list()
reg <- function(...) {
 m<-new.env()
 m$fit<-function(x,y){seen$train[[length(seen$train)+1L]]<-x;m$mean<-mean(y);invisible(m)}
 m$predict<-function(x){seen$predict[[length(seen$predict)+1L]]<-x;rep(m$mean,nrow(x))}
 m
}
clf <- function(...) {
 m<-new.env()
 m$fit<-function(x,y){m$classes_<-sort(unique(y));invisible(m)}
 m$predict_proba<-function(x)matrix(1/length(m$classes_),nrow(x),length(m$classes_))
 m
}
api<-list(TabICLRegressor=reg,TabICLClassifier=clf,
          FinetunedTabICLRegressor=reg,FinetunedTabICLClassifier=clf)
set.seed(42)
d<-data.frame(Y=as.numeric(1:24),X=rnorm(24),A=factor(rep(c("A","B"),12)))
d$Y[c(3,9)]<-NA;d$X[c(3,4)]<-NA;d$A[c(9,10)]<-NA
for(fine in c(FALSE,TRUE)) {
 seen$train<-seen$predict<-list()
 result<-vimpute(d,method="tabicl2",sequential=FALSE,visit_sequence=c("Y","X","A"),
   learner_params=list(tabicl2=list(tabicl_module=api,finetune=fine)),
   pmm=FALSE,uncert="none",imp_var=FALSE)
 # Quality diagnostics may predict training rows first: select actual missing rows.
 pred<-Filter(function(z)nrow(z)==2L,seen$predict)[[1L]]
 expect_true(anyNA(seen$train[[1]]))
 expect_true(anyNA(pred))
 expect_identical(pred,VIM:::.tabicl_input(d[c(3,9),c("A","X")],
   list(A=levels(d$A),X=NULL)))
 expect_true(is.factor(pred$A))
 expect_false(anyNA(result))
 for(v in names(d))expect_identical(result[[v]][!is.na(d[[v]])],d[[v]][!is.na(d[[v]])])
}
# PMM donor scoring must use the same fitted transformation, not R-side fills.
VIM:::register_tabicl_learners()
l<-mlr3::lrn("regr.tabicl2")
l$param_set$values$tabicl_module<-api
obs<-which(!is.na(d$Y))
t<-mlr3::TaskRegr$new("donors",d[obs,],target="Y");l$train(t)
seen$predict<-list()
score<-VIM:::pmm_observed_scores(l,data.table::as.data.table(d),obs,c("A","X"),
                               "Y",list(A=levels(d$A)),"tabicl2",FALSE)
expect_equal(length(score),length(obs))
expect_true(anyNA(seen$predict[[1]]))
expect_identical(seen$predict[[1]],VIM:::.tabicl_input(d[obs,c("A","X")],
   list(A=levels(d$A),X=NULL)))

# Legacy coding remains an explicit compatibility option.
l$param_set$values$input_type<-"matrix";l$train(t)
expect_true(is.matrix(tail(seen$train,1)[[1]]))
expect_true(is.null(VIM:::.tabicl_constructor_args(list(input_type="dataframe"))$input_type))

# Semicontinuous regression retains missing predictors for the TabICL regressor.
s<-d;s$Y[seq(1,24,by=2)]<-0;s$Y[c(4,10)]<-NA_real_
seen$predict<-list()
invisible(vimpute(s,method="tabicl2",sequential=FALSE,visit_sequence=c("Y","X","A"),
 learner_params=list(tabicl2=list(tabicl_module=api)),pmm=FALSE,uncert="none",imp_var=FALSE))
pred<-Filter(function(z)nrow(z)==sum(is.na(s$Y)),seen$predict)[[1L]]
expect_true(anyNA(pred))
expect_true(is.factor(pred$A))
