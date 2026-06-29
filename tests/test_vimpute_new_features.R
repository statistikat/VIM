required_pkgs <- c(
  "data.table",
  "future",
  "mlr3",
  "mlr3pipelines",
  "mlr3learners",
  "mlr3tuning",
  "paradox",
  "robustbase",
  "glmnet",
  "ranger",
  "mgcv"
)

installed_pkgs <- rownames(installed.packages())
missing_pkgs <- required_pkgs[!required_pkgs %in% installed_pkgs]

loadable <- vapply(required_pkgs, function(pkg) {
  isTRUE(tryCatch(requireNamespace(pkg, quietly = TRUE), error = function(e) FALSE))
}, logical(1))

unloadable_pkgs <- required_pkgs[!loadable]

if (length(missing_pkgs) > 0L || length(unloadable_pkgs) > 0L) {
  stop(
    "Cannot run tests. Missing or unloadable packages: ",
    paste(unique(c(missing_pkgs, unloadable_pkgs)), collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(mlr3)
  library(mlr3pipelines)
  library(mlr3learners)
  library(mlr3tuning)
  library(paradox)
  library(robustbase)
  library(glmnet)
  library(ranger)
  library(mgcv)
})

find_package_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "DESCRIPTION")) && dir.exists(file.path(current, "R"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find package root.", call. = FALSE)
    }
    current <- parent
  }
}

root_dir <- find_package_root()

source(file.path(root_dir, "R", "helper_vimpute.R"))
source(file.path(root_dir, "R", "vimmi.R"))
source(file.path(root_dir, "R", "vimpute.R"))

check <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

show_test <- function(label) {
  cat("\n---", label, "---\n")
}

make_data <- function(n = 100L) {
  set.seed(123)

  x1 <- rnorm(n)
  x2 <- runif(n)
  grp <- factor(sample(c("a", "b", "c"), n, replace = TRUE))

  dt <- data.table(
    x1 = x1,
    x2 = x2,
    grp = grp
  )

  dt[, y_num := 3 + x1 - x2 + rnorm(.N, sd = 0.2)]
  dt[, y_pos := exp(1 + 0.5 * x1 + rnorm(.N, sd = 0.1))]

  prob <- plogis(1.2 * x1 - 0.8 * x2 + ifelse(grp == "c", 0.7, 0))
  dt[, y_fac := factor(ifelse(runif(.N) < prob, "yes", "no"))]

  dt[sample(.N, 15), y_num := NA_real_]
  dt[sample(.N, 12), y_pos := NA_real_]
  dt[sample(.N, 12), y_fac := NA]
  dt[sample(.N, 8), x1 := NA_real_]
  dt[sample(.N, 8), grp := NA]

  dt
}

set.seed(20260319)

show_test("1) Einfaches vimpute mit m = 1")
dt1 <- make_data(100L)
res1 <- vimpute(
  data = dt1,
  considered_variables = c("x1", "x2", "grp", "y_num"),
  method = list(y_num = "ranger"),
  sequential = FALSE,
  verbose = TRUE
)

check(is.data.table(res1), "m = 1 sollte ein data.table zurueckgeben.")
check(!anyNA(res1$y_num), "y_num sollte nach einfacher Imputation keine NAs mehr haben.")
cat("Rueckgabe:", class(res1)[1], "\n")

show_test("2) Multiple Imputation mit m = 3")
dt2 <- make_data(100L)
res2 <- vimpute(
  data = dt2,
  considered_variables = c("x1", "x2", "grp", "y_num", "y_pos"),
  method = list(y_num = "ranger", y_pos = "ranger"),
  sequential = FALSE,
  imp_var = FALSE,
  m = 3L,
  boot = TRUE,
  uncert = "resid",
  verbose = TRUE
)

check(inherits(res2, "vimmi"), "m > 1 sollte ein vimmi-Objekt zurueckgeben.")
check(identical(res2$m, 3L), "vimmi sollte m = 3 speichern.")
cat("Rueckgabe:", class(res2)[1], "| m =", res2$m, "\n")

show_test("3) complete() auf vimmi")
d1 <- complete(res2, 1L)
dall <- complete(res2, "all")
dlong <- complete(res2, "long")

check(is.data.frame(d1), "complete(res, 1) sollte einen Datensatz liefern.")
check(length(dall) == 3L, "complete(res, 'all') sollte drei Datensaetze liefern.")
check(nrow(dlong) == 3L * nrow(dt2), "complete(res, 'long') sollte alle Imputationen stapeln.")
check(!anyNA(d1$y_num), "Im kompletterten Datensatz darf y_num keine NAs mehr haben.")
check(!anyNA(d1$y_pos), "Im kompletterten Datensatz darf y_pos keine NAs mehr haben.")
cat("complete() funktioniert.\n")

show_test("4) with() auf vimmi")
shift <- 5
with_res <- with(res2, y_num + x2 + shift)

check(length(with_res) == 3L, "with(vimmi, ...) sollte ein Ergebnis pro Imputation liefern.")
check(all(vapply(with_res, length, integer(1)) == nrow(dt2)), "with(vimmi, ...) hat unerwartete Laengen geliefert.")
cat("with() funktioniert.\n")

show_test("5) uncert = 'normalerror'")
dt3 <- make_data(100L)
res3 <- vimpute(
  data = dt3,
  considered_variables = c("x1", "x2", "grp", "y_num"),
  method = list(y_num = "ranger"),
  sequential = FALSE,
  boot = TRUE,
  robustboot = "standard",
  uncert = "normalerror",
  verbose = TRUE
)

check(!anyNA(res3$y_num), "uncert = 'normalerror' sollte y_num imputieren.")
cat("uncert = 'normalerror' funktioniert.\n")

show_test("6) uncert = 'midastouch'")
dt4 <- make_data(100L)
res4 <- vimpute(
  data = dt4,
  considered_variables = c("x1", "x2", "grp", "y_num"),
  method = list(y_num = "ranger"),
  sequential = FALSE,
  uncert = "midastouch",
  verbose = FALSE
)

check(!anyNA(res4$y_num), "uncert = 'midastouch' sollte y_num imputieren.")
cat("uncert = 'midastouch' funktioniert.\n")

show_test("7) robuster Bootstrap mit robustboot = 'psi'")
dt5 <- make_data(100L)
res5 <- vimpute(
  data = dt5,
  considered_variables = c("x1", "x2", "grp", "y_num"),
  method = list(y_num = "robust"),
  sequential = FALSE,
  boot = TRUE,
  robustboot = "psi",
  uncert = "normalerror",
  verbose = TRUE
)

check(!anyNA(res5$y_num), "robust + boot + robustboot = 'psi' sollte y_num imputieren.")
cat("robustboot funktioniert.\n")

show_test("8) Methode gam fuer numerisches Ziel")
dt6 <- make_data(100L)
res6 <- vimpute(
  data = dt6,
  considered_variables = c("x1", "x2", "grp", "y_pos"),
  method = list(y_pos = "gam"),
  formula = list(y_pos = log(y_pos) ~ x1 + x2 + grp),
  sequential = FALSE,
  verbose = FALSE
)

check(!anyNA(res6$y_pos), "gam sollte y_pos imputieren.")
cat("gam funktioniert.\n")

show_test("9) Methode robgam fuer numerisches Ziel")
dt7 <- make_data(100L)
res7 <- vimpute(
  data = dt7,
  considered_variables = c("x1", "x2", "grp", "y_num"),
  method = list(y_num = "robgam"),
  formula = list(y_num = y_num ~ x1 + x2 + grp),
  sequential = FALSE,
  verbose = FALSE
)

check(!anyNA(res7$y_num), "robgam sollte y_num imputieren.")
cat("robgam funktioniert.\n")

show_test("10) Methode gam fuer Faktor-Ziel")
dt8 <- make_data(100L)
res8 <- vimpute(
  data = dt8,
  considered_variables = c("x1", "x2", "grp", "y_fac"),
  method = list(y_fac = "gam"),
  formula = list(y_fac = y_fac ~ x1 + x2 + grp),
  sequential = FALSE,
  verbose = FALSE
)

check(is.factor(res8$y_fac), "gam-Klassifikation sollte Faktorwerte zurueckgeben.")
check(!anyNA(res8$y_fac), "gam-Klassifikation sollte y_fac imputieren.")
cat("gam-Klassifikation funktioniert.\n")

show_test("11) Methode robgam fuer Faktor-Ziel")
dt9 <- make_data(100L)
res9 <- vimpute(
  data = dt9,
  considered_variables = c("x1", "x2", "grp", "y_fac"),
  method = list(y_fac = "robgam"),
  formula = list(y_fac = y_fac ~ x1 + x2 + grp),
  sequential = TRUE,
  nseq = 2L,
  verbose = FALSE
)

check(is.factor(res9$y_fac), "robgam-Klassifikation sollte Faktorwerte zurueckgeben.")
check(!anyNA(res9$y_fac), "robgam-Klassifikation sollte y_fac imputieren.")
cat("robgam-Klassifikation funktioniert.\n")

show_test("12) Warnung bei m > 1 ohne Unsicherheitsquelle")
dt10 <- make_data(100L)
warn_text <- NULL

withCallingHandlers(
  vimpute(
    data = dt10,
    considered_variables = c("x1", "x2", "grp", "y_num"),
    method = list(y_num = "ranger"),
    sequential = FALSE,
    imp_var = FALSE,
    m = 2L,
    boot = FALSE,
    uncert = "none",
    pmm = FALSE,
    verbose = TRUE
  ),
  warning = function(w) {
    warn_text <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)

check(grepl("all imputations will be identical", warn_text, fixed = TRUE), "Warnung fuer identische Imputationen wurde nicht gefunden.")
cat("Warnung fuer m > 1 ohne Unsicherheit funktioniert.\n")

show_test("13) pmm hat Vorrang vor uncert")
dt11 <- make_data(100L)
warn_text2 <- NULL

withCallingHandlers(
  vimpute(
    data = dt11,
    considered_variables = c("x1", "x2", "grp", "y_num"),
    method = list(y_num = "ranger"),
    sequential = FALSE,
    pmm = TRUE,
    pmm_k = 3L,
    uncert = "resid",
    verbose = TRUE
  ),
  warning = function(w) {
    warn_text2 <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)

check(grepl("'pmm' takes precedence", warn_text2, fixed = TRUE), "Warnung fuer pmm-Vorrang wurde nicht gefunden.")
cat("Warnung fuer pmm-Vorrang funktioniert.\n")

show_test("14) as.mids.vimmi optional")
if (requireNamespace("mice", quietly = TRUE)) {
  mids_obj <- as.mids.vimmi(res2)
  check(inherits(mids_obj, "mids"), "as.mids.vimmi() sollte ein mids-Objekt liefern.")
  cat("as.mids.vimmi funktioniert.\n")
} else {
  cat("mice nicht installiert, as.mids.vimmi wurde nicht getestet.\n")
}

show_test("15) makeNA und donorcond")
set.seed(20260428)
dt12 <- make_data(80L)
dt12[is.na(x1), x1 := 0]
dt12[is.na(y_num), y_num := 999]
sentinel_idx <- which(dt12$y_num == 999)
donor_rows <- setdiff(seq_len(nrow(dt12)), sentinel_idx)[1:5]
dt12[donor_rows, y_num := -abs(y_num) - 1]

res12 <- vimpute(
  data = dt12,
  considered_variables = c("x1", "x2", "y_num"),
  method = list(y_num = "ranger"),
  makeNA = list(y_num = 999),
  donorcond = list(y_num = ">0"),
  pmm = TRUE,
  pmm_k = 3L,
  sequential = FALSE,
  verbose = FALSE
)

check(!any(res12$y_num == 999), "makeNA-Werte sollten nach der Imputation nicht erhalten bleiben.")
check(all(res12$y_num_imp[sentinel_idx]), "Durch makeNA erzeugte fehlende Werte sollten als imputiert markiert werden.")
check(all(res12$y_num[sentinel_idx] > 0), "Imputierte Werte sollten die donorcond-Bedingung erfuellen.")
cat("makeNA und donorcond funktionieren.\n")
