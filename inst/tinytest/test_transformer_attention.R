library(VIM)
resolve <- VIM:::.transformer_head_dim
expect_identical(resolve(NULL, 128, 8), 128L)
expect_identical(resolve("compact", 128, 8), 16L)
expect_identical(resolve(32, 128, 8), 32L)
expect_error(resolve("compact", 10, 3))
expect_error(resolve(0, 128, 8))
expect_error(resolve(NA_real_, 128, 8))
expect_error(resolve(1.5, 128, 8))
expect_error(resolve(16, 128, 0))
if (identical(Sys.getenv("VIM_RUN_TRANSFORMER_TESTS"), "true")) {
  build <- function(h) VIM:::build_model_cLM(5L, 16L, 4L, 32L, 1L, 5L,
    10L, 0, 8L, 3L, head_dim = h, pooling = "legacy")
  legacy <- build(NULL); compact <- build("compact")
  expect_true(keras::count_params(compact) < keras::count_params(legacy))
  for (model in list(legacy, compact, build(8))) {
    keras::compile(model, optimizer = "adam", loss = "sparse_categorical_crossentropy")
    x <- matrix(rep(1:5, 6), nrow = 6, byrow = TRUE)
    keras::fit(model, x, rep(0:2, 2), epochs = 1L, verbose = 0L)
    p <- predict(model, x, verbose = 0L)
    expect_identical(dim(p), c(6L, 3L))
    expect_true(all(is.finite(p)))
  }
}
