library(VIM)
expect_equal(VIM:::col_to_string(c(1,NA),1,width=5)$x[1],"001.0")
expect_error(VIM:::col_to_string(12345,1,width=3))
expect_equal(VIM:::col_to_string(c(NA,NA),1,width=5)$len,5)
expect_error(load_transformer(tempfile()))
obj <- structure(list(schema=list(x=list(class="numeric",levels=NULL),
  y=list(class="factor",levels=c("a","b")))),class="vim_transformer_model")
expect_error(VIM:::.transformer_validate_schema(obj,data.frame(x=1,y=factor("c"))))
expect_error(VIM:::.transformer_validate_schema(obj,data.frame(x=1)))
if (identical(Sys.getenv("VIM_RUN_TRANSFORMER_TESTS"),"true")) {
  d <- data.frame(x=seq(0,1,length.out=20),y=seq(1,2,length.out=20))
  set.seed(18); before <- .Random.seed
  fit <- fit_transformer(d,"y",seed=5,epochs=1,verbose=0,
    embedding_dim=8,num_heads=2,head_dim="compact",feed_forward_dim=16,dense_dim=8)
  expect_identical(.Random.seed,before)
  new <- d; new$y[1:2] <- NA
  p <- predict(fit,new)
  expect_identical(p$y[-(1:2)],d$y[-(1:2)])
  expect_false(anyNA(p))
  expect_equal(predict(fit,new[c("y","x")])$y,p$y)
  path <- tempfile(); save_transformer(fit,path)
  expect_equal(predict(load_transformer(path),new),p)
  expect_error(save_transformer(fit,path))
  unlink(path,recursive=TRUE)
}

expect_error(fit_transformer(data.frame(x=1:10,y=1:10),"y",decimal_points=NULL))
expect_error(fit_transformer(data.frame(x=1:10,y=1:10),"y",decimal_points_target=1.5))

fake <- structure(list(schema=list(x=list(class="numeric",levels=NULL)),
  options=list(cat_vars=character(),decimal_points=0,round_long_decs=TRUE),
  target="y",lens=list(x=1),tok=data.frame(column="x",Word_ngram="1")),
  class="vim_transformer_model")
expect_error(VIM:::.transformer_validate_schema(fake,data.frame(x=2)))
expect_true(VIM:::.transformer_validate_schema(fake,data.frame(x=1)))
