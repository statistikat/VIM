library(VIM)
data <- VIM:::.transformer_benchmark_data(30)
set.seed(42); before <- .Random.seed
m <- VIM:::.transformer_benchmark_mask(data, seed = 7)
expect_identical(.Random.seed, before)
expect_identical(m, VIM:::.transformer_benchmark_mask(data, seed = 7))
expect_true(!any(m[, 1]))
expect_equal(sum(VIM:::.transformer_benchmark_score(data, data, m)$rmse, na.rm = TRUE), 0)
bad <- data; bad$x[1] <- 999
expect_error(VIM:::.transformer_benchmark_score(data, bad, m))
report <- VIM:::.transformer_benchmark(data,
  list(oracle = function(x) data, unavailable = function(x) stop("backend unavailable")),
  seeds = 1, mechanisms = "MAR")
expect_true(all(report$results$status[report$results$method == "oracle"] == "ok"))
expect_true(all(report$results$status[report$results$method == "unavailable"] == "failed"))
expect_true(all(is.na(report$results$peak_memory_bytes)))
expect_error(VIM:::.transformer_benchmark_mask(data, rate = 1))
