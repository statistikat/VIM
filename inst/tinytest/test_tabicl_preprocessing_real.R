# Opt-in Python parity checks; routine package checks never start Python.
if (!identical(Sys.getenv("VIM_TEST_TABICL"),"true"))
  exit_file("set VIM_TEST_TABICL=true for typed-input Python parity checks")
if (!all(vapply(c("reticulate","mlr3","R6","paradox"),requireNamespace,
                logical(1),quietly=TRUE))) exit_file("optional dependencies absent")
library(VIM)
VIM:::register_tabicl_learners()
reticulate::import("torch")$set_num_threads(1L)
pd<-reticulate::import("pandas",convert=FALSE)
api<-VIM:::.tabicl_import()
set.seed(73)
x<-rnorm(48);h<-rep(c("a","b"),24)
d<-data.frame(x=x,h=ordered(h,levels=c("b","a","unused")),
              empty=NA_real_,y=3*x+ifelse(h=="a",2,-2)+rnorm(48,sd=.2),
              g=factor(ifelse(x>0,"pos","neg"),levels=c("neg","pos","unused")))
d$empty_group<-factor(rep(NA_character_,48),levels=c("a","b"))
d$x[c(2,5,41)]<-NA_real_;d$h[c(3,6,42)]<-NA
train<-d[1:40,];test<-d[41:48,]
unseen_test<-test
unseen_test$h<-factor(c("new",as.character(test$h[-1])),levels=c("new","a","b"))
# Match mlr3's required column schema; the adapter maps unknown values to NA.
# Include an unused declared level as a true new category for Python's encoder.
test$h<-ordered(as.character(unseen_test$h),levels=levels(train$h))
test$h[3]<-"unused"
# Construct an independent pandas frame using known codes, without .tabicl_input.
manual_frame<-function(frame,features) {
  values<-setNames(lapply(features,function(nm) {
    z<-frame[[nm]]
    if(nm=="h") {
      codes<-match(as.character(z),c("b","a","unused"))-1L
      codes[is.na(codes)]<--1L
      pd$Categorical$from_codes(as.array(codes),categories=list("b","a","unused"))
    } else if(nm=="empty_group") {
      pd$Categorical$from_codes(as.array(rep(-1L,nrow(frame))),categories=list("a","b"))
    } else if(nm=="g") {
      codes<-match(as.character(z),c("neg","pos","unused"))-1L
      codes[is.na(codes)]<--1L
      pd$Categorical$from_codes(as.array(codes),categories=list("neg","pos","unused"))
    } else as.array(as.numeric(z))
  }),features)
  pd$DataFrame$from_dict(values)
}
for(fine in c(FALSE,TRUE))for(classif in c(FALSE,TRUE)) {
  target<-if(classif)"g" else "y"
  task<-if(classif)mlr3::TaskClassif$new("typed",train,target=target) else
    mlr3::TaskRegr$new("typed",train,target=target)
  pred_task<-if(classif)mlr3::TaskClassif$new("typed_test",test,target=target) else
    mlr3::TaskRegr$new("typed_test",test,target=target)
  learner<-mlr3::lrn(if(classif)"classif.tabicl2" else "regr.tabicl2")
  learner$param_set$values<-utils::modifyList(learner$param_set$values,
    list(finetune=fine,device="cpu",n_estimators=1L,allow_auto_download=FALSE,
         epochs=1L,finetune_params=list(n_estimators_finetune=1L,
                                       n_estimators_validation=1L,amp=FALSE)))
  learner$train(task)
  model<-learner$model
  actual_x<-VIM:::.tabicl_input(unseen_test[model$features],model$encoding,python=TRUE)
  expect_identical(reticulate::py_to_r(actual_x$h$dtype$name),"category")
  expect_equal(as.integer(reticulate::py_to_r(actual_x$h$cat$codes$to_numpy()))[1:2],c(-1L,-1L))
  expect_true(is.na(reticulate::py_to_r(actual_x$x$to_numpy())[1]))
  expect_identical(reticulate::py_to_r(actual_x$`__getitem__`("empty")$dtype$name),"float64")
  expect_identical(reticulate::py_to_r(actual_x$empty_group$dtype$name),"category")
  # Direct Python fitting and prediction must match the mlr3 adapter.
  kind<-paste0(if(fine)"Finetuned" else "","TabICL",if(classif)"Classifier" else "Regressor")
  direct<-do.call(api[[kind]],VIM:::.tabicl_constructor_args(learner$param_set$get_values()))
  y<-if(classif)as.character(train[[target]]) else train[[target]]
  direct$fit(manual_frame(train,model$features),y)
  if(classif) {
    learner$predict_type<-"prob"
    actual<-learner$predict(pred_task)$prob
    expected<-as.matrix(direct$predict_proba(manual_frame(test,model$features)))
    classes<-as.character(direct$classes_)
    expect_equal(unname(actual[,classes,drop=FALSE]),unname(expected),tolerance=1e-6)
    expect_equal(rowSums(actual),rep(1,nrow(test)),tolerance=1e-6)
  } else {
    actual<-learner$predict(pred_task)$response
    expected<-as.numeric(direct$predict(manual_frame(test,model$features)))
    expect_equal(actual,expected,tolerance=1e-6)
    expect_true(all(is.finite(actual)))
  }
  # One-row input and a fitted model's categorical schema remain valid.
  one<-if(classif)mlr3::TaskClassif$new("one",test[1,,drop=FALSE],target=target) else
    mlr3::TaskRegr$new("one",test[1,,drop=FALSE],target=target)
  expect_equal(length(learner$predict(one)$response),1L)
}
