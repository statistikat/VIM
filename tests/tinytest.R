if ( requireNamespace("tinytest", quietly=TRUE) ){
  ## CRAN runs the fast core of the suite only: the full suite takes several
  ## minutes on the CRAN Windows builder, and CRAN's overall check-time budget
  ## is 10 minutes. The complete suite (hyperparameter tuning, multiple-
  ## imputation regression tests, ...) runs whenever NOT_CRAN=true -- set by
  ## devtools::check()/test() and by the GitHub Actions workflow -- or through
  ## tinytest::test_all()/run_test_dir() (at_home = TRUE); the long-running
  ## files/blocks are guarded with `if (at_home())` / `exit_file()`.
  tinytest::test_package("VIM", at_home = identical(Sys.getenv("NOT_CRAN"), "true"))
}
