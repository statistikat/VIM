# Verify the strong-signal data design and variant routing without Python.
if (!all(vapply(c("mlr3", "mlr3learners", "mlr3pipelines", "mlr3tuning",
                 "paradox", "R6", "future"), requireNamespace, logical(1), quietly = TRUE)))
  exit_file("mlr3 stack not installed")
library(VIM)
e <- new.env()
sys.source(system.file("benchmarks", "tabicl2-correlated.R", package = "VIM"), e)
d <- e$tabicl_correlated_data()
expect_identical(d, e$tabicl_correlated_data())
expect_identical(names(d), c("X", "Y", "A", "B"))
expect_true(all(table(d$A, d$B) > 10L))
expect_true(summary(stats::lm(Y ~ X + A * B, d))$r.squared > .99)
mu <- c("A:B" = -8, "A:C" = -2, "B:B" = 2, "B:C" = 10)
residual <- d$Y - 4*d$X - unname(mu[paste(d$A, d$B, sep = ":")])
expect_true(stats::sd(residual) > .35 && stats::sd(residual) < .65)

seen <- list()
reg <- function(...) {
  m <- new.env()
  m$fit <- function(x, y) { seen[[length(seen) + 1L]] <<- x; m$mean <- mean(y); invisible(m) }
  m$predict <- function(x) rep(m$mean, nrow(x))
  m
}
clf <- function(...) {
  m <- new.env()
  m$fit <- function(x, y) { seen[[length(seen) + 1L]] <<- x; m$classes_ <- sort(unique(y)); invisible(m) }
  m$predict_proba <- function(x) matrix(1 / length(m$classes_), nrow(x), length(m$classes_))
  m
}
api <- list(TabICLRegressor = reg, TabICLClassifier = clf,
  FinetunedTabICLRegressor = reg, FinetunedTabICLClassifier = clf)
x <- e$tabicl_correlated_run(backend = api)
expect_equal(colSums(x$mask), setNames(rep(32, 4), names(d)))
expect_identical(is.na(x$incomplete), x$mask)
expect_equal(nrow(x$metrics), 12L)
for (method in names(x$completed)) {
  expect_false(anyNA(x$completed[[method]]))
  fine <- grepl("finetune", method, fixed = TRUE)
  calls <- x$fit_calls[[method]]
  expect_true(all(vapply(calls, function(c)
    startsWith(c$kind, "Finetuned") == fine, logical(1))))
  expect_true(any(vapply(calls, function(c) endsWith(c$kind, "Regressor"), logical(1))))
  expect_true(any(vapply(calls, function(c) endsWith(c$kind, "Classifier"), logical(1))))
  if (endsWith(method, "seq10")) {
    expect_true(length(calls) > 4L && length(calls) <= 40L)
  } else expect_equal(length(calls), 4L)
  if (fine) expect_true(all(vapply(calls, function(c)
    identical(c$args$epochs, 20L) && identical(c$args$learning_rate, 1e-5), logical(1))))
}
# Equal predictions from an API double are allowed. Real-model differences are
# measured, never required to favor fine-tuning on one random small dataset.
expect_true(all(x$differences$fraction_changed == 0))

# Regression: the first sweep must see actual X/Y values, not exclusively
# binary missingness indicators. Every target has at least one numeric feature.
for (features in seen[1:4]) {
  expect_equal(ncol(features), 3L)
  expect_true(any(is.finite(features) & !features %in% c(0, 1)))
}
expect_true(any(vapply(seen[1:4], anyNA, logical(1))))

expect_error(e$tabicl_correlated_run(epochs = 0, backend = api), "positive integer")
