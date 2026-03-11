library(VIM)

new_mock_transformer_model <- function(predictions, checks = NULL) {
  state <- new.env(parent = emptyenv())
  state$call <- 0L

  structure(
    list(predictions = predictions, checks = checks, state = state),
    class = "mock_transformer_model"
  )
}

predict.mock_transformer_model <- function(object, newdata, ...) {
  object$state$call <- object$state$call + 1L
  call_idx <- object$state$call

  if (!is.null(object$checks) && length(object$checks) >= call_idx &&
      !is.null(object$checks[[call_idx]])) {
    object$checks[[call_idx]](newdata)
  }

  object$predictions[[call_idx]]
}

registerS3method("predict", "mock_transformer_model", predict.mock_transformer_model)

# transformerImpute imputes categorical targets with a supplied model", {
cat_df <- data.frame(
  x = c(1, 2, 1),
  y = factor(c("A", "B", NA_character_), levels = c("A", "B"))
)

cat_tok <- data.table::data.table(
  Word_ngram = c("_pad_", ",", "1", "2", "A", "B"),
  column = c("u", "u", "x", "x", "y", "y"),
  TOKEN = 0:5
)

cat_model <- list(
  tok = cat_tok,
  target_tok = cat_tok[column == "y"],
  lens = list(y = 1),
  input_shape = 2,
  model = new_mock_transformer_model(
    predictions = list(matrix(c(0.1, 0.9), nrow = 1))
  )
)

cat_out <- transformerImpute(
  cat_df,
  target = "y",
  cat_vars = "y",
  model = cat_model,
  decimal_points = 0,
  imp_col = TRUE
)

expect_identical(as.character(cat_out$y), c("A", "B", "B"))
expect_identical(as.numeric(cat_out$imp), c(0, 0, 1))
expect_identical(as.character(cat_out$y[1:2]), as.character(cat_df$y[1:2]))

# transformerImpute keeps decimal tokens in the prediction context", {
num_df <- data.frame(
  grp = c("low", "high", "low"),
  score = c(1.2, 2.3, NA_real_)
)

num_tok <- data.table::data.table(
  Word_ngram = c("_pad_", ",", "low", "high", "1", ".", "2", "3"),
  column = c("u", "u", "grp", "grp", "score", "score", "score", "score"),
  TOKEN = 0:7
)

period_token <- as.numeric(num_tok[column == "score" & Word_ngram == ".", TOKEN])

num_model <- list(
  tok = num_tok,
  target_tok = num_tok[column == "score"],
  lens = list(score = 3),
  input_shape = 4,
  model = new_mock_transformer_model(
    predictions = list(
      matrix(c(0.8, 0.01, 0.1, 0.09), nrow = 1),
      matrix(c(0.05, 0.01, 0.9, 0.04), nrow = 1)
    ),
    checks = list(
      NULL,
      function(newdata) {
        expect_identical(as.numeric(newdata[1, ncol(newdata)]), period_token)
      }
    )
  )
)

num_out <- transformerImpute(
  num_df,
  target = "score",
  cat_vars = "grp",
  model = num_model,
  decimal_points = 0,
  decimal_points_target = 1
)

expect_equal(num_out$score[3], 1.2)
expect_identical(num_out$score[1:2], num_df$score[1:2])

# transformerImpute returns early when the target has no missings", {
no_missing_df <- data.frame(
  x = c(1, 2, 3),
  y = factor(c("A", "B", "A"), levels = c("A", "B"))
)

unused_model <- list(
  tok = cat_tok,
  target_tok = cat_tok[column == "y"],
  lens = list(y = 1),
  input_shape = 2,
  model = new_mock_transformer_model(
    predictions = list(matrix(c(0.5, 0.5), nrow = 1))
  )
)

no_missing_out <- transformerImpute(
  no_missing_df,
  target = "y",
  cat_vars = "y",
  model = unused_model,
  decimal_points = 0
)

expect_identical(as.character(no_missing_out$y), as.character(no_missing_df$y))
expect_identical(unused_model$model$state$call, 0L)
