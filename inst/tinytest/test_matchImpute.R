library(VIM)
message("matchImpute general")
d <- data.frame(x=LETTERS[1:6],y=as.double(1:6),z=as.double(1:6),
                w=ordered(LETTERS[1:6]), stringsAsFactors = FALSE)
dorig <- rbind(d,d)
# minimal example with one match var
d1 <- matchImpute(setna(dorig,7:12,1)[,1:2],match_var = "y", variable="x")
expect_identical(d1$x[d1$x_imp],d1$x[!d1$x_imp])

d1b <- matchImpute(setna(dorig,7:12,1)[,1:2],match_var = "y", variable="x", imp_var = FALSE)
expect_identical(d1b$x[d1$x_imp],d1b$x[!d1$x_imp])
expect_false("x_imp" %in% colnames(d1b))
expect_true("x_imp" %in% colnames(d1))


# all missing in x -> error
expect_error(matchImpute(setna(dorig,1:12,1)[,1:2],match_var = "y", variable="x"))


# example with two match vars
d1 <- matchImpute(setna(dorig,7:12,1)[,1:3],match_var = c("y","z"), variable="x")
expect_identical(d1$x[d1$x_imp],d1$x[!d1$x_imp])
