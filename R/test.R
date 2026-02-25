library(data.table)
library(grDevices)
library(Rcpp)
library(sp)
library(stats)
library(methods)
library(nnet)
library(e1071)
library(grid)
library(robustbase)
library(colorspace)
library(car)  # bcPower, powerTransform werden mitgeladen
library(vcd)  # mosaic, labeling_border werden mitgeladen
library(laeken)  # weightedMedian, weightedMean werden mitgeladen
library(graphics)
library(utils)
library(ranger)  # ranger, importance
library(MASS)  # stepAIC, lqs, polr, rlm
library(mlr3)
library(mlr3pipelines)
library(paradox)
library(mlr3tuning)
library(mlr3learners)












library(data.table)
data(mtcars)
mtcars

set.seed(123)
dt <- as.data.table(mtcars)
dt[, cyl := factor(cyl)]
   
num_na_idx <- sample(nrow(dt), 5)
dt[num_na_idx, mpg := NA]

fac_na_idx <- sample(nrow(dt), 5)
dt[fac_na_idx, cyl := NA]

dt

undebug(vimpute)
# debug(vimpute)
vp <- vimpute(
  data = dt, 
  method = list(mpg = "ranger", cyl = "xgboost"),
  learner_params = list(predict_median = TRUE),
  verbose = TRUE,
  pred_history = TRUE,
  nseq=2
  # pmm = TRUE,
  # pmm_k = 3,
)




vp_A <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="xgboost"),
  learner_params = list(
    mpg = list(num.trees = 900)     # only for mpg
  ),
  verbose = TRUE,
  pred_history = TRUE
)


vp_B <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="xgboost"),
  learner_params = list(
    ranger  = list(num.trees = 800),
    xgboost = list(eta = 0.2, nrounds = 50)
  ),
  nseq = 2,
  verbose = TRUE
)


vp_C <- vimpute(
  data = dt,
  method = list(mpg="xgboost", cyl="xgboost"),
  learner_params = list(eta = 0.33, nrounds = 66),
  nseq=2,
  verbose = TRUE
)


vp_D <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="xgboost"),
  learner_params = list(
    predict_median = TRUE    # matches none of variable/method names
  ),
  nseq=2,
  verbose = TRUE
)



vp_E <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="xgboost"),
  learner_params = list(
    ranger = list(
      num.trees = 700,     # valid
      foobar = 123         # invalid
    ),
    xgboost = list(
      eta = 0.2, 
      nonsense = 9999
    )
  ),
  nseq=2,
  verbose = TRUE
)


vp_F <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="xgboost"),
  learner_params = list(
    mpg     = list(num.trees=999),   # wins for mpg
    ranger  = list(num.trees=500),   # used only for mpg if not overwritten
    eta     = 0.22                   # unmatched -> warning
  ),
  verbose = TRUE
)


dt2 <- copy(dt)
set.seed(12)
dt2[, semi := ifelse(runif(.N) < 0.4, 0, rlnorm(.N, 2, .3))]
dt2[sample(.N, 20), semi := NA]

vp_G <- vimpute(
  data = dt2,
  method = list(semi = "ranger", mpg = "xgboost", cyl = "xgboost"),
  learner_params = list(ranger=list(num.trees=300)),
  nseq=2,
  verbose = TRUE
)


vp_H <- vimpute(
  data=dt,
  method=list(mpg="ranger", cyl="xgboost"),
  verbose=TRUE
)




####################################################
vp_A <- vimpute(
  data = dt,
  method = list(mpg="robust", cyl="ranger"),
  learner_params = list(
    mpg = list(num.trees = 900)
  ),
  pmm=TRUE,
  pmm_k=5,
  verbose = TRUE,
  pred_history = TRUE,
  nseq = 2
)

# Prüfung manuell:
table(is.na(vp_A$data$mpg))
table(is.na(vp_A$data$cyl))

vp_B <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="ranger"),
  learner_params = list(
    ranger = list(num.trees = 800)
  ),
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_B$mpg))
table(is.na(vp_B$cyl))


vp_C <- vimpute(
  data = dt,
  method = list(mpg="xgboost", cyl="xgboost"),
  learner_params = list(
    eta = 0.33,
    nrounds = 66,
    quatsch = 2
  ),
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_C$mpg))
table(is.na(vp_C$cyl))

vp_D <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="xgboost"),
  learner_params = list(
    mpg    = list(num.trees=999),                                               
    ranger = list(num.trees=500), # verboten → mixing var & method              # nach tune: num.trees=563,
    eta    = 0.22                 # unmatched, mehrere Methoden → warning
  ),
  tune= TRUE,
  verbose = TRUE,
  nseq = 2
)
vp_D

dt_glm <- copy(dt)
dt_glm[sample(.N, 5), hp := NA]  # numeric target

vp_E <- vimpute(
  data = dt_glm,
  method = list(hp="regularized"),
  learner_params = list(
    hp = list(alpha=0.5)
  ),
  tune=TRUE,
  pred_history = TRUE,
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_E$hp))

dt_cls <- as.data.table(iris)
dt_cls[sample(.N, 20), Species := NA]

vp_F <- vimpute(
  data = dt_cls,
  method = list(Species="regularized"),
  learner_params = list(
    Species = list(alpha = 0.7, lambda = 0.01)
  ),
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_F$Species))

dt_rob <- copy(dt)
dt_rob[sample(.N, 5), mpg := NA]

vp_G <- vimpute(
  data = dt_rob,
  method = list(mpg="robust", cyl="ranger"),
  learner_params = list(
    mpg = list(max.it = 100)
  ),
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_G$mpg))

dt_bin <- as.data.table(iris)
dt_bin[, Species2 := ifelse(Species == "setosa", "setosa", "other")]
dt_bin[, Species2 := factor(Species2)]
dt_bin[sample(.N, 10), Species2 := NA]

vp_H <- vimpute(
  data = dt_bin,
  method = list(Species2="robust"),
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_H$Species2))


dt2 <- copy(dt)
set.seed(12)
dt2[, semi := ifelse(runif(.N) < 0.4, 0, rlnorm(.N, 2, .3))]
dt2[sample(.N, 20), semi := NA]

vp_I <- vimpute(
  data = dt2,
  method = list(semi="ranger", mpg="xgboost", cyl="xgboost"),
  learner_params = list(ranger=list(num.trees=300)),
  nseq = 2,
  verbose = TRUE
)

table(is.na(vp_I$semi))

dt_pmm <- copy(dt)
dt_pmm[sample(.N, 5), mpg := NA]

vp_J <- vimpute(
  data = dt_pmm,
  method = list(mpg="ranger"),
  pmm = list(mpg = TRUE),
  pmm_k = 1,
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_J$mpg))

vp_K <- vimpute(
  data = dt_pmm,
  method = list(mpg="ranger"),
  pmm = list(mpg = TRUE),
  pmm_k = 5,
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_K$mpg))

# test robust
  # multiclass
library(data.table)
dt <- as.data.table(iris)
set.seed(123)
dt[sample(.N, 25), Species := NA]

vp_mc <- vimpute(
  data = dt,
  method = list(Species = "robust"),
  verbose = TRUE,
  nseq = 2
)
table(is.na(vp_mc$Species))

  # binär
dtb <- as.data.table(iris)
dtb[, Species2 := factor(ifelse(Species=="setosa","setosa","other"))]
set.seed(123)
dtb[sample(.N, 15), Species2 := NA]

vp_bin <- vimpute(
  data = dtb,
  method = list(Species2 = "robust"),
  verbose = TRUE,
  nseq = 2
)
table(is.na(vp_bin$Species2))



# pmm
dt <- as.data.table(mtcars)
set.seed(123)
dt[sample(.N, 5), mpg := NA]
dt[sample(.N, 5), cyl := NA]
dt[sample(.N, 5), hp  := NA]

vp <- vimpute(
  data = dt,
  method = list(mpg="ranger", cyl="xgboost", hp="regularized"),
  learner_params = list(mpg=list(num.trees=300), cyl=list(nrounds=50), hp=list(alpha=0.5)),
  pmm = list(mpg=TRUE, cyl=FALSE, hp=TRUE),
  pmm_k = 5,
  tune = FALSE,
  verbose = TRUE,
  nseq = 2
)


# FORMULA ######################################################################
## 1
library(data.table)

dt <- as.data.table(mtcars)
set.seed(123)

dt[sample(.N, 5), mpg := NA]

vp_formula1 <- vimpute(
  data = dt,
  method = list(mpg = "regularized"),
  formula = list(
    mpg = mpg ~ hp + wt + cyl
  ),
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_formula1$mpg))

## 2
dt <- as.data.table(mtcars)
set.seed(99)

dt[sample(.N, 6), mpg := NA]
dt[sample(.N, 5), hp  := NA]

vp_formula2 <- vimpute(
  data = dt,
  method = list(
    mpg = "ranger",
    hp  = "regularized"
  ),
  formula = list(
    #mpg = mpg ~ wt + disp,
    hp  = hp  ~ drat + cyl
  ),
  verbose = TRUE,
  nseq = 2
)

table(is.na(vp_formula2$data$mpg))
table(is.na(vp_formula2$data$hp))

##3
dt <- as.data.table(mtcars)
set.seed(42)
dt[sample(.N, 5), mpg := NA]

tryCatch({
  vimpute(
    data = dt,
    method = list(mpg = "regularized"),
    formula = list(
      mpg = mpg ~ hp + does_not_exist     # bewusst falsch
    ),
    verbose = TRUE,
    nseq = 2
  )
}, error = function(e) {
  message("EXPECTED ERROR:", e$message)
})

## 4
dt <- as.data.table(mtcars)
set.seed(77)

dt[sample(.N, 5), mpg := NA]
dt[sample(.N, 5), hp  := NA]

vp_formula4 <- vimpute(
  data = dt,
  method = list(
    mpg="robust",
    hp ="robust"
  ),
  learner_params = list(
    mpg = list(alpha=0.2),
    hp  = list(alpha=0.1)
  ),
  formula = list(
    mpg = mpg ~ wt + drat + qsec,
    hp  = hp  ~ cyl + disp
  ),
  pmm = list(mpg = TRUE, hp = FALSE),
  pmm_k = list(mpg = 3),
  verbose = TRUE,
  pred_history = TRUE,
  nseq = 2
)

table(is.na(vp_formula4$data$mpg))
table(is.na(vp_formula4$data$hp))


#### Hyperparameter tuning #####################################################
library(data.table)

dt <- as.data.table(mtcars)
set.seed(1)

dt[sample(.N, 5), mpg := NA]
dt[sample(.N, 5), hp  := NA]

vp_ht0 <- tryCatch({
  vimpute(
    data = dt,
    method = list(mpg="ranger", hp="regularized"),
    learner_params = list(mpg = list(num.trees=200)),
    tune = list(mpg=TRUE, hp=TRUE),
    pred_history = TRUE,
    verbose = TRUE,
    nseq = 3
  )
  TRUE
}, error = function(e) e$message)

vp_ht0


# test 2
dt <- as.data.table(mtcars)
set.seed(3)
dt[sample(.N,4), hp := NA]

obj <- vimpute(
  data = dt,
  method = list(hp="ranger"),
  tune = TRUE,
  verbose = TRUE,
  pred_history = TRUE,
  nseq = 4
)

# The object should contain tuned parameters or fallback defaults
names(obj)

















# ----------------------------------------------------------------------------------------------------------------
# tests/testthat/test-vimpute.R

library(testthat)
library(data.table)

# Nur laufen, wenn die wichtigsten Pakete verfügbar sind
skip_if_not_installed("mlr3")
skip_if_not_installed("mlr3tuning")
skip_if_not_installed("mlr3pipelines")
skip_if_not_installed("mlr3learners")
skip_if_not_installed("paradox")
skip_if_not_installed("future")

# Optional: diese Tests nur laufen lassen, wenn auch xgboost und ranger da sind
has_xgboost <- requireNamespace("xgboost", quietly = TRUE)
has_ranger  <- requireNamespace("ranger",  quietly = TRUE)
has_robust  <- requireNamespace("robustbase", quietly = TRUE)

set.seed(123)

# -------------------------------------------------------------------
# Hilfsfunktion: künstlichen Testdatensatz erzeugen
# -------------------------------------------------------------------
make_test_data <- function(n = 200L) {
  dt <- data.table(
    # numerisch mit linearem Zusammenhang
    x_num1 = rnorm(n, mean = 10, sd = 3),
    x_num2 = runif(n, 0, 5),
    
    # Faktor mit 3 Levels
    x_fac1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    
    # semicontinuous: viele Nullen + positive Werte
    y_sc = {
      z <- rpois(n, lambda = 2)
      z[z < 1] <- 0
      as.numeric(z)
    },
    
    # target rein numerisch
    y_num = rnorm(n, 50, 10),
    
    # target als Faktor
    y_fac = factor(sample(c("yes", "no"), n, replace = TRUE))
  )
  
  # künstliche Missing Values
  set(dt, i = sample(n, n * 0.1), j = "x_num1", value = NA_real_)
  set(dt, i = sample(n, n * 0.1), j = "x_num2", value = NA_real_)
  set(dt, i = sample(n, n * 0.1), j = "x_fac1", value = NA)
  set(dt, i = sample(n, n * 0.2), j = "y_sc",  value = NA_real_)
  set(dt, i = sample(n, n * 0.2), j = "y_num", value = NA_real_)
  set(dt, i = sample(n, n * 0.2), j = "y_fac", value = NA)
  
  dt
}

# -------------------------------------------------------------------
# 1) Basis-Test: ranger, keine Tuning/PMM, sequential = FALSE
# -------------------------------------------------------------------
test_that("vimpute basic ranger imputation without tuning & PMM", {
  skip_if_not(has_ranger, "ranger not installed")
  
  dt <- make_test_data(150L)
  
  res <- vimpute(
    data = dt,
    considered_variables = names(dt),
    method = "ranger",
    sequential = FALSE,
    nseq = 5,          # sollte intern auf 1 gesetzt werden
    pmm = FALSE,
    tune = FALSE,
    imp_var = TRUE,
    pred_history = FALSE,
    verbose = TRUE
  )
  
  expect_s3_class(res, "data.table")
  
  # keine NAs mehr in den betrachteten Variablen
  expect_false(anyNA(res[, .(x_num1, x_num2, x_fac1, y_sc, y_num, y_fac)]))
  
  # _imp-Variablen existieren und sind logisch
  imp_cols <- grep("_imp$", names(res), value = TRUE)
  expect_true(length(imp_cols) > 0)
  expect_true(all(vapply(res[, ..imp_cols], is.logical, logical(1))))
})

# -------------------------------------------------------------------
# 2) Sequential = TRUE, Konvergenz-Kriterium + pred_history
# -------------------------------------------------------------------
test_that("vimpute sequential imputation with pred_history and early stop", {
  dt <- make_test_data(100L)
  
  res <- vimpute(
    data = dt,
    method = "ranger",
    sequential = TRUE,
    nseq = 5,
    eps = 1e-3,
    tune = FALSE,
    pred_history = TRUE,
    verbose = TRUE
  )
  
  # Rückgabe: Liste mit data + pred_history (kein Tuning)
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_true("pred_history" %in% names(res))
  
  expect_s3_class(res$data, "data.table")
  expect_s3_class(res$pred_history, "data.table")
  
  # auch hier sollten keine NAs übrig sein
  expect_false(anyNA(res$data))
})

# -------------------------------------------------------------------
# 3) Methodenmix + learner_params (per Methode & per Variable)
# -------------------------------------------------------------------
test_that("vimpute supports different methods and learner_params mapping", {
  dt <- make_test_data(120L)
  
  # Verschiedene Methoden pro Variable
  method_list <- list(
    x_num1 = "ranger",
    x_num2 = if (has_xgboost) "xgboost" else "ranger",  # fallback falls xgboost fehlt
    y_num  = "regularized",
    y_sc   = "ranger",
    y_fac  = "robust"   # robust classification (classif.glm_rob)
  )
  
  # learner_params:
  # - global für ranger
  # - spezifisch für Variable y_num (glmnet)
  learner_params <- list(
    ranger = list(num.trees = 100L),
    regularized = list(alpha = 0.5),
    y_num = list(alpha = 0.7)  # per Variable überschreibt ggf. method-basierte
  )
  
  res <- vimpute(
    data = dt,
    considered_variables = names(dt),
    method = method_list,
    learner_params = learner_params,
    sequential = TRUE,
    nseq = 3,
    pmm = FALSE,
    tune = FALSE,
    verbose = TRUE
  )
  
  expect_s3_class(res, "data.table")
  expect_false(anyNA(res))
})

# -------------------------------------------------------------------
# 4) PMM Tests: pmm global, pmm_k global + per Variable
# -------------------------------------------------------------------
test_that("vimpute PMM (k=1 and k>1) for numeric variables works", {
  dt <- make_test_data(150L)
  
  # Nur numerische Targets betrachten für PMM
  considered <- c("x_num1", "x_num2", "y_sc", "y_num")
  
  # a) PMM global TRUE, k = 1
  res1 <- vimpute(
    data = dt,
    considered_variables = considered,
    method = "ranger",
    pmm = TRUE,
    pmm_k = 1,
    sequential = FALSE,
    tune = FALSE,
    verbose = FALSE
  )
  
  expect_s3_class(res1, "data.table")
  expect_false(anyNA(res1[, ..considered]))
  
  # b) PMM per Variable, k > 1 für y_num
  pmm_list <- list(
    x_num1 = TRUE,
    x_num2 = FALSE,
    y_sc   = TRUE,
    y_num  = TRUE
  )
  pmm_k_list <- list(
    x_num1 = 1L,
    y_sc   = 3L,
    y_num  = 5L
  )
  
  res2 <- vimpute(
    data = dt,
    considered_variables = considered,
    method = "regularized",
    pmm = pmm_list,
    pmm_k = pmm_k_list,
    sequential = TRUE,
    nseq = 3,
    tune = FALSE,
    verbose = TRUE
  )
  
  expect_false(anyNA(res2[, ..considered]))
})

# -------------------------------------------------------------------
# 5) Formel-Support inkl. Transformationen (log, sqrt, inverse)
# -------------------------------------------------------------------
test_that("vimpute formula interface with LHS transformations works", {
  dt <- make_test_data(150L)
  
  # künstlich positive y_num, damit log/sqrt/inverse sinnvoll sind
  dt[, y_num_pos := abs(y_num) + 1]
  dt[sample(.N, .N * 0.2), y_num_pos := NA_real_]
  
  # Formeln: log(y_num_pos) ~ x_num1 + x_fac1, y_sc ~ x_num2 + x_fac1
  form_list <- list(
    y_num_pos = log(y_num_pos) ~ x_num1 + x_fac1,
    y_sc      = y_sc ~ x_num2 + x_fac1
  )
  
  method_list <- list(
    y_num_pos = "regularized",
    y_sc      = "robust"
  )
  
  res <- vimpute(
    data = dt,
    considered_variables = c("x_num1", "x_num2", "x_fac1", "y_sc", "y_num_pos"),
    method = method_list,
    formula = form_list,
    sequential = TRUE,
    nseq = 3,
    tune = FALSE,
    verbose = FALSE
  )
  
  # keine NAs in y_num_pos und y_sc
  expect_false(anyNA(res$y_num_pos))
  expect_false(anyNA(res$y_sc))
})

# -------------------------------------------------------------------
# 6) Semikontinuierliche Variablen: zweistufiges Modell (class + regr)
# -------------------------------------------------------------------
test_that("vimpute handles semicontinuous variables via two-stage model", {
  dt <- make_test_data(200L)
  
  # sicherstellen, dass y_sc wirklich semicontinuous ist
  expect_true(is_semicontinuous(dt$y_sc))
  
  res <- vimpute(
    data = dt,
    considered_variables = c("x_num1", "x_num2", "x_fac1", "y_sc"),
    method = "ranger",
    sequential = TRUE,
    nseq = 3,
    tune = FALSE,
    verbose = FALSE
  )
  
  expect_false(anyNA(res$y_sc))
  
  # Werte sollten >= 0 sein (wegen Konstruktion)
  expect_true(all(res$y_sc >= 0, na.rm = TRUE))
})

# -------------------------------------------------------------------
# 7) Tuning: tune = TRUE, per Variable, Rückgabe mit tuning_log
# -------------------------------------------------------------------
test_that("vimpute tuning runs once per variable and returns tuning_log", {
  dt <- make_test_data(120L)
  
  # Nur eine handvoll Variablen, sonst wird's zu langsam
  considered <- c("x_num1", "x_num2", "y_num")
  
  # Methoden: ranger + ggf. xgboost
  method_list <- list(
    x_num1 = "ranger",
    x_num2 = if (has_xgboost) "xgboost" else "ranger",
    y_num  = "regularized"
  )
  
  # Tuning: nur für x_num1 und y_num
  tune_list <- list(
    x_num1 = TRUE,
    x_num2 = FALSE,
    y_num  = TRUE
  )
  
  res <- vimpute(
    data = dt,
    considered_variables = considered,
    method = method_list,
    sequential = TRUE,
    nseq = 4,          # Mitte = Iteration 2
    tune = tune_list,
    pred_history = FALSE,
    verbose = FALSE
  )
  
  # Rückgabe: Liste mit data + tuning_log
  expect_type(res, "list")
  expect_true("data" %in% names(res))
  expect_true("tuning_log" %in% names(res))
  
  expect_s3_class(res$data, "data.table")
  expect_type(res$tuning_log, "list")
  
  # mind. so viele Einträge wie Variablen mit NA
  tuned_vars <- vapply(res$tuning_log, function(x) x$variable, character(1))
  expect_true(all(tuned_vars %in% considered))
})

# -------------------------------------------------------------------
# 8) Rückgabeform: nur data, wenn weder pred_history noch tuning aktiv
# -------------------------------------------------------------------
test_that("vimpute returns only data.table if no pred_history and no tuning", {
  dt <- make_test_data(80L)
  
  res <- vimpute(
    data = dt,
    method = "ranger",
    sequential = FALSE,
    pred_history = FALSE,
    tune = FALSE,
    verbose = FALSE
  )
  
  # in diesem Fall: direkt data.table, keine Liste
  expect_s3_class(res, "data.table")
  expect_false(anyNA(res))
})

# -------------------------------------------------------------------
# 9) Verhalten bei Variablen mit nur NAs (sollte Fehler werfen)
# -------------------------------------------------------------------
test_that("vimpute errors if a variable contains only NAs", {
  dt <- make_test_data(50L)
  
  dt[, only_na := NA_real_]
  
  expect_error(
    vimpute(
      data = dt,
      considered_variables = c("only_na", "x_num1"),
      method = "ranger",
      sequential = FALSE,
      verbose = FALSE
    ),
    regexp = "contains only missing values",
    fixed = FALSE
  )
})

# -------------------------------------------------------------------
# 10) Verhalten bei Faktoren: Klassifikation + probabilistische Vorhersage
# -------------------------------------------------------------------
test_that("vimpute correctly imputes factor targets with classification models", {
  dt <- make_test_data(100L)
  
  considered <- c("x_num1", "x_num2", "x_fac1", "y_fac")
  
  res <- vimpute(
    data = dt,
    considered_variables = considered,
    method = "ranger",
    sequential = TRUE,
    nseq = 3,
    tune = FALSE,
    verbose = FALSE
  )
  
  expect_false(anyNA(res$y_fac))
  expect_true(is.factor(res$y_fac))
})
