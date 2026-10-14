if (!all(vapply(c("validate", "ECOSolveR", "mlr3", "mlr3learners", "mlr3pipelines",
                 "mlr3tuning", "paradox", "R6", "future"), requireNamespace,
               logical(1), quietly = TRUE))) exit_file("optional imputation/edit stack not installed")
if (!at_home()) exit_file("model integration checks run at home")
library(VIM)
set.seed(7)
n <- 60L
truth <- data.frame(x = runif(n, 1, 3))
truth$turnover <- 10 + 2 * truth$x + rnorm(n, sd = .2)
truth$costs <- runif(n, .5, .8) * truth$turnover
truth$profit <- truth$turnover - truth$costs
truth$cap <- truth$turnover + 20
truth$kind <- factor(rep(c("A", "B"), 30))
d <- truth; d$turnover[1:8] <- NA; d$costs[5:12] <- NA; d$profit[9:16] <- NA
vars <- c("x", "turnover", "costs", "profit")
rules <- validate::validator(identity = profit == turnover - costs,
  costs >= 0, profit >= 0, turnover <= cap, if(kind == "A") turnover >= 0)
compliant <- function(out, original = d) {
  complete <- truth
  for(v in intersect(names(out), names(truth))) complete[[v]] <- out[[v]]
  expect_true(max(abs(complete$profit - complete$turnover + complete$costs)) <= 1e-6)
  expect_true(all(complete$costs >= -1e-6 & complete$profit >= -1e-6))
  for(v in vars) expect_equal(out[[v]][!is.na(original[[v]])], original[[v]][!is.na(original[[v]])])
  expect_true(!is.null(attr(out,"edit_diagnostics")))
}
api <- list(TabICLRegressor = function(...) {
  e <- new.env(); e$fit <- function(x,y) {e$mu <- mean(y); invisible(e)}
  e$predict <- function(x) rep(e$mu,nrow(x)); e
})
params <- list(ranger = list(num.trees = 15L, num.threads = 1L),
  xgboost = list(nrounds = 3L, nthread = 1L), tabicl2 = list(tabicl_module = api),
  restricted = list(rules = validate::validator(turnover >= 0, costs >= 0, profit >= 0)))
for (method in c("ranger", "xgboost", "tabicl2", "regularized", "robust", "gam", "robgam", "restricted")) {
  if (method == "regularized" && !requireNamespace("glmnet", quietly = TRUE)) next
  if (method %in% c("gam","robgam") && !requireNamespace("mgcv", quietly = TRUE)) next
  out <- vimpute(d, considered_variables = vars, method = method,
    learner_params = if(method %in% names(params)) params[method] else NULL, edit_rules = rules,
    sequential = FALSE, imp_var = TRUE, uncert = "none", seed = 8)
  compliant(out)
  expect_equal(out$turnover_imp, is.na(d$turnover), info = method)
  expect_equal(out$cap, truth$cap)
}
# Both validators are independent, including deliberately conflicting edits.
simple <- data.frame(y = c(1,2,NA_real_), x = c(1,2,3))
model <- validate::validator(y >= 4)
final <- validate::validator(if(x > 2) y <= 3)
base <- vimpute(simple, method = "restricted", learner_params = list(restricted = list(rules = model,
  save_optimization_problem = TRUE)), sequential = FALSE, uncert = "none", imp_var = FALSE)
expect_true(base$y[3] >= 4 - 1e-6)
dual <- vimpute(simple, method = "restricted", learner_params = list(restricted = list(rules = model,
  save_optimization_problem = TRUE)), edit_rules = final,
  edit_control = list(save_optimization_problem = TRUE), sequential = FALSE, uncert = "none", imp_var = FALSE)
expect_equal(dual$y[3], 3, tolerance = 1e-6)
expect_true(dual$y[3] < 4 - 1e-6)
expect_equal(attr(base,"restricted_optimization_problems"), attr(dual,"restricted_optimization_problems"))
expect_true(attr(dual,"edit_optimization_problems")[[1]][[1]]$original_predictions["y"] >= 4 - 1e-6)
expect_equal(attr(dual,"edit_diagnostics")$optimization_problems_solved, 1)
identical_rules <- validate::validator(if(x > 2) y >= 4)
a <- vimpute(simple, method = "restricted", learner_params = list(restricted = list(rules = identical_rules)),
  edit_rules = identical_rules, sequential = FALSE, uncert = "none", imp_var = FALSE)
expect_true(a$y[3] >= 4 - 1e-6)
expect_equal(attr(a,"edit_diagnostics")$optimization_problems_solved, 0)
expect_error(vimpute(simple,method="restricted",edit_rules=final,sequential=FALSE),"rules")

# Mixed estimators jointly project, including bootstrap and stochastic values.
mixed <- list(turnover = "restricted", costs = "ranger", profit = "tabicl2")
out <- vimpute(d, considered_variables = vars, method = mixed, learner_params = params,
  edit_rules = rules, sequential = TRUE, nseq = 3, uncert = "normalerror", boot = TRUE,
  imp_var = FALSE, seed = 29)
compliant(out)
ch <- attr(out,"chain")$mean
for(v in c("turnover","costs","profit"))
  expect_equal(unname(tail(ch[,v],1)),mean(out[[v]][is.na(d[[v]])]))
# Unrestricted default and explicit NULL are identical under a fixed seed.
a <- vimpute(d[vars],method="ranger",learner_params=params["ranger"],sequential=FALSE,seed=1)
b <- vimpute(d[vars],method="ranger",learner_params=params["ranger"],sequential=FALSE,seed=1,edit_rules=NULL)
expect_identical(a,b)

# makeNA eligibility, passthrough edit variables, omitted output columns, table
# preservation, and per-imputation summaries/optional original stochastic draws.
sentinel <- d; sentinel$turnover[1:8] <- -9
out <- vimpute(sentinel, considered_variables = vars, method = "tabicl2", learner_params = params["tabicl2"],
  makeNA = list(turnover = -9), edit_rules = rules, sequential = FALSE,
  uncert = "pmm", imp_var = TRUE, keep_all_columns = FALSE, seed = 3)
expect_false(any(c("cap","kind") %in% names(out)))
compliant(out, d)
expect_equal(out$turnover_imp, seq_len(n) %in% 1:8)
dt <- data.table::as.data.table(d); original_dt <- data.table::copy(dt)
out <- vimpute(dt, considered_variables=vars, method="tabicl2",learner_params=params["tabicl2"],
  edit_rules=rules,sequential=FALSE,uncert="resid",seed=3)
compliant(out)
expect_true(data.table::is.data.table(out)); expect_identical(dt,original_dt)
mi <- vimpute(d,considered_variables=vars,method="ranger",learner_params=params["ranger"],
  edit_rules=rules,edit_control=list(save_optimization_problem=TRUE),m=3,boot=TRUE,
  uncert="normalerror",sequential=TRUE,nseq=2,seed=15)
expect_true(inherits(mi,"vimmi"))
expect_equal(length(attr(mi,"edit_diagnostics")),3L)
for(i in 1:3) {
  complete <- vim_complete(mi,i); compliant(complete)
  expect_identical(attr(complete,"edit_diagnostics"),attr(mi,"edit_diagnostics")[[i]])
  expect_true(length(attr(complete,"edit_optimization_problems"))>0)
}
mi2 <- vimpute(d,considered_variables=vars,method="ranger",learner_params=params["ranger"],
  edit_rules=rules,m=3,boot=TRUE,uncert="normalerror",sequential=TRUE,nseq=2,seed=15)
expect_equal(mi$imp,mi2$imp)
# Specs and formula grammar accept top-level edits without learner overloads.
out <- vimpute(d,considered_variables=vars,
  spec = list(.default = vs_ranger(num.trees=15L)), edit_rules=rules,
  sequential=FALSE,uncert="none",seed=1)
compliant(out)
out <- vimpute(simple, y ~ x | robust(), edit_rules=final, sequential=FALSE, uncert="none",seed=1)
expect_true(out$y[3]<=3+1e-6)

# A registered numeric learner exposes next-sweep predictor data. Visiting costs
# first makes its second fit observe turnover constrained at the previous sweep.
seen <- new.env(); seen$costs <- list()
mock <- R6::R6Class("LearnerRegrEditMock", inherit=mlr3::LearnerRegr,
  public=list(initialize=function() super$initialize(id="regr.edit_mock",
    feature_types=c("numeric","integer","factor","logical"), predict_types="response",
    properties="missings", param_set=paradox::ps(), packages="VIM")),
  private=list(.train=function(task) {
    if(task$target_names=="costs") seen$costs[[length(seen$costs)+1L]] <- as.data.frame(task$data())
    list(mu=mean(task$truth()))
  }, .predict=function(task) list(response=rep(self$model$mu,task$nrow))))
mlr3::mlr_learners$add("regr.edit_mock",mock)
register_vimpute_method("edit_mock",list(regr="regr.edit_mock"))
f <- truth[c("x","turnover","costs")]; f$gate <- seq_len(n)<=3
f$turnover[1:3] <- NA; f$costs[4:6] <- NA
out <- vimpute(f,method="edit_mock",edit_rules=validate::validator(if(gate == TRUE) turnover>=30),
  visit_sequence=c("costs","turnover"),sequential=TRUE,nseq=8,uncert="none",imp_var=FALSE)
expect_equal(out$turnover[1:3],rep(30,3),tolerance=1e-6)
expect_true(length(seen$costs)>=2L)
second <- seen$costs[[2]]
expect_equal(second$turnover[second$x %in% f$x[1:3]],rep(30,3),tolerance=1e-6)
expect_equal(nrow(attr(out,"chain")$mean),3L)
unregister_vimpute_method("edit_mock")

# Projection follows PMM draws and rounding, preserving the raw draw only when requested.
final_bound <- validate::validator(if(x > 2) y >= 10)
for (noise in c("pmm", "resid", "normalerror")) {
  bounded <- tabiclImpute(simple, learner_params = list(tabicl_module = api),
    edit_rules = final_bound, edit_control = list(save_optimization_problem = TRUE),
    uncert = noise, imp_var = FALSE, seed = 4)
  expect_equal(bounded$y[3], 10, tolerance = 1e-6)
  raw <- attr(bounded,"edit_optimization_problems")[[1]][[1]]$original_predictions["y"]
  expect_true(is.finite(raw) && raw < 10)
  if(noise == "pmm") expect_true(raw %in% simple$y[1:2])
}
# Joint formula-wrapper projection and suffix/observed-value preservation.
for (wrapper in c("rangerImpute", "xgboostImpute", "regressionImp")) {
  args <- list(formula=turnover+costs+profit~x,data=d,edit_rules=rules,
    edit_control=list(weights="equal"),imp_suffix="filled")
  if(wrapper=="rangerImpute") args$num.trees <- 15L
  if(wrapper=="xgboostImpute") args$nrounds <- 3L
  result <- do.call(get(wrapper),args)
  compliant(result)
  expect_equal(result$turnover_filled,is.na(d$turnover))
}
# Fractional continuous solutions survive post-result construction.
fraction <- data.frame(a=c(0,0,NA_real_),b=c(0,0,NA_real_),x=c(1,2,3))
result <- tabiclImpute(fraction,learner_params=list(tabicl_module=api),
  edit_rules=validate::validator(if(x>2) a+b==1),edit_control=list(weights="equal"),imp_var=FALSE)
expect_equal(result$a[3],.5,tolerance=1e-6)
expect_equal(result$b[3],.5,tolerance=1e-6)
expect_true(is.logical(out$gate))

long <- vim_complete(mi,"long")
expect_identical(attr(long,"edit_diagnostics"),attr(mi,"edit_diagnostics"))
expect_identical(attr(long,"edit_optimization_problems"),attr(mi,"edit_optimization_problems"))

# Preserve even logical covariates not mentioned in the edit validator.
with_flag <- d; with_flag$flag <- rep(c(TRUE,FALSE),30)
checked <- tabiclImpute(with_flag,learner_params=list(tabicl_module=api),edit_rules=rules,
  imp_var=FALSE,seed=3)
expect_identical(checked$flag,with_flag$flag)
