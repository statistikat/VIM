# irmi can handle logicals (#16)", {
  df <- data.frame(x = as.factor(c("a", "a", "b", "b")),
                   y = as.integer(c(1, NA, 1, 2)),
                   z = rnorm(4))
  df_imputed <- irmi(df)
  expect_true(is.data.frame(df_imputed))
  expect_false(any(is.na(df_imputed$y)))


# irmi can handle integers (#17)", {
  n <- 1000
  df <- data.frame(
    lgl1 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
    lgl2 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
    lgl3 = sample(c(TRUE, FALSE), size = n, replace = TRUE)
  )
  df$lgl1[sample(1000, 100)] <- NA
  df_imputed <- irmi(df)

  expect_true(is.data.frame(df_imputed))
  expect_false(any(is.na(df_imputed$lgl3)))

  df <- data.frame(
    lgl1 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
    lgl2 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
    lgl3 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
    num = rnorm(n)
  )
  df$num[sample(1000, 100)] <- NA
  df_imputed <- irmi(df)

  expect_true(is.data.frame(df_imputed))
  expect_false(any(is.na(df_imputed$num)))

  df <- data.frame(
    lgl1 = sample(c(TRUE, FALSE), size = n, replace = TRUE),
    num1 = rnorm(n), num2 = rnorm(n),
    num3 = rnorm(n)
  )
  df$lgl1[sample(1000, 100)] <- NA
  df_imputed <- irmi(df)

  expect_true(is.data.frame(df_imputed))
  expect_false(any(is.na(df_imputed$lgl1)))

