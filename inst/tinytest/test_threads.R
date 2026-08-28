## CRAN policy: a package must never use more than 2 cores during R CMD check.
## VIM's only uncapped parallelism is the OpenMP code behind gowerD()/kNN();
## its thread count is governed by option "VIM.ncores": if unset, at most 2
## threads under R CMD check (_R_CHECK_LIMIT_CORES_) and OpenMP's own default
## otherwise (encoded as 0 = "let OpenMP decide").
ns <- asNamespace("VIM")
expect_true(exists("vim_ncores", envir = ns, inherits = FALSE))

if (exists("vim_ncores", envir = ns, inherits = FALSE)) local({
  vim_ncores <- get("vim_ncores", envir = ns)
  old_opt <- options(VIM.ncores = NULL)
  old_env <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  on.exit({
    options(old_opt)
    if (is.na(old_env)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    else Sys.setenv("_R_CHECK_LIMIT_CORES_" = old_env)
  }, add = TRUE)

  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE"); expect_equal(vim_ncores(), 2L)
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "warn"); expect_equal(vim_ncores(), 2L)
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "false"); expect_equal(vim_ncores(), 0L)
  Sys.unsetenv("_R_CHECK_LIMIT_CORES_"); expect_equal(vim_ncores(), 0L)
  ## the option wins over the check environment
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  options(VIM.ncores = 4); expect_equal(vim_ncores(), 4L)
  options(VIM.ncores = 1L); expect_equal(vim_ncores(), 1L)
  options(VIM.ncores = 0); expect_error(vim_ncores(), "VIM.ncores")
  options(VIM.ncores = "many"); expect_error(vim_ncores(), "VIM.ncores")

  ## results must not depend on the thread count
  data(sleep, package = "VIM")
  options(VIM.ncores = 1L); g1 <- gowerD(sleep); set.seed(1); k1 <- kNN(sleep)
  options(VIM.ncores = 2L); g2 <- gowerD(sleep); set.seed(1); k2 <- kNN(sleep)
  expect_identical(g1, g2)
  expect_identical(k1, k2)
})
