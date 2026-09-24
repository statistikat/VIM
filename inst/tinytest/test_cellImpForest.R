# cellImpForest(): scaffold
expect_true(is.function(VIM::cellImpForest))
expect_error(VIM::cellImpForest(data.frame(a = 1:3)), "two columns")
