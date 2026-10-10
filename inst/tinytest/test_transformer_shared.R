library(VIM)
expect_error(fit_shared_transformer(data.frame(x=1:5)))
expect_error(fit_shared_transformer(data.frame(x=1:20),variables="x",id_vars="x"))
if(identical(Sys.getenv("VIM_RUN_TRANSFORMER_TESTS"),"true")) {
  set.seed(5); d <- data.frame(x=rnorm(40),y=rnorm(40),g=factor(rep(c("a","b"),20)),id=1:40)
  d$y[1:3] <- NA
  fit <- fit_shared_transformer(d,variables=c("y","g"),id_vars="id",seed=2,
    epochs=2,embedding_dim=8,num_heads=2)
  expect_false("id" %in% names(fit$schema))
  expect_true(all(is.finite(unlist(fit$history))))
  natural <- is.na(d[fit$provenance$validation_rows,c("y","g")])
  expect_false(any(fit$validation_mask & natural))
  d$g[4:6] <- NA
  out <- predict(fit,d)
  expect_false(anyNA(out))
  for(v in names(d)) expect_identical(out[[v]][!is.na(d[[v]])],d[[v]][!is.na(d[[v]])])
  path <- tempfile(); save_transformer(fit,path)
  expect_equal(predict(load_transformer(path),d),out,tolerance=1e-6)
  unlink(path,recursive=TRUE)
}
