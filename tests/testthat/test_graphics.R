# graphics tests
# 
# Author: alex
###############################################################################
data(sleep,package = "VIM")
data(chorizonDL, package = "VIM")
data(kola.background, package = "VIM")
data(tao, package = "VIM")
test_that("aggr missing failed", {
  # switch to a tempdir() to ensure writeability
  withr::local_dir(withr::local_tempdir())
  a <- aggr(sleep, numbers=TRUE)
  summary(a)
  print(a)
  expect_equal(sum(a$missings$Count),38)
})

test_that("aggr imputed failed", {
  
  sleep_IMPUTED <- kNN(sleep)
  a <- aggr(sleep_IMPUTED, delimiter="_imp")
  summary(a)
  print(a)
  expect_equal(sum(a$missings$Count),38)
})

test_that("barMiss missing failed", {
  barMiss(sleep[, c("Exp", "Sleep")], interactive = FALSE)
  b <- barMiss(sleep[, c("Exp", "Sleep")], only.miss = FALSE, interactive = FALSE)
  expect_equal(b[,1],c(.7,1.9,3.1,4.3,5.5))
})

test_that("barMiss imputed failed", {
  x_IMPUTED  <- kNN(sleep[, c("Exp", "Sleep")])
  barMiss(x_IMPUTED, delimiter = "_imp", interactive = FALSE)
  b <- barMiss(x_IMPUTED, delimiter = "_imp", only.miss = FALSE, interactive = FALSE)
  expect_equal(b[,1],c(.7,1.9,3.1,4.3,5.5))
})


test_that("growdotMiss missing failed", {
  x <- chorizonDL[, c("Ca","As", "Bi")]
  expect_null(growdotMiss(x, chorizonDL[, c("XCOO", "YCOO")], kola.background, border = "white",
              interactive = FALSE))
})

test_that("growdotMiss imputed failed", {
  x_imp <- kNN(chorizonDL[,c("Ca","As","Bi" )])
  expect_null(growdotMiss(x_imp, chorizonDL[, c("XCOO", "YCOO")], kola.background, delimiter = "_imp",
              border = "white", interactive = FALSE))
})

test_that("histMiss missing failed", {
  x <- tao[, c("Air.Temp", "Humidity")]
  histMiss(x, interactive = FALSE)
  h <- histMiss(x, only.miss = FALSE, interactive = FALSE)
  expect_equal(sum(h$counts),655)
})

test_that("histMiss imputed failed", {
  ## for imputed values
  x_IMPUTED <- kNN(tao[, c("Air.Temp", "Humidity")])
  histMiss(x_IMPUTED, delimiter = "_imp", interactive = FALSE)
  h <- histMiss(x_IMPUTED, delimiter = "_imp", only.miss = FALSE, interactive = FALSE)
  expect_equal(sum(h$counts),736)
})


test_that("mapMiss missing failed", {
  x <- chorizonDL[, c("As", "Bi")]
  expect_null(mapMiss(x, chorizonDL[, c("XCOO", "YCOO")], kola.background,
                      interactive = FALSE))
})

test_that("mapMiss imputed failed", {
  ## for imputed values
  x_imp <- kNN(chorizonDL[, c("As", "Bi")])
  expect_null( mapMiss(x_imp, chorizonDL[, c("XCOO", "YCOO")], kola.background,
          delimiter = "_imp", interactive = FALSE))
})


test_that("marginmatrix missing failed", {
  ## for missing values
  x <- sleep[, 1:5]
  x[,c(1,2,4)] <- log10(x[,c(1,2,4)])
  expect_warning(marginmatrix(x))
})

test_that("marginmatrix imputed failed", {
  ## for imputed values
  x_imp <- kNN(sleep[, 1:5])
  x_imp[,c(1,2,4)] <- log10(x_imp[,c(1,2,4)])
  expect_warning(marginmatrix(x_imp, delimiter = "_imp"))
})

test_that("matrixplot missing failed", {
  ## for missing values
  x <- sleep[, -(8:10)]
  x[,c(1,2,4,6,7)] <- log10(x[,c(1,2,4,6,7)])
  expect_warning(matrixplot(x, sortby = "BrainWgt", interactive = FALSE))
})

test_that("matrixplot imputed failed", {
  ## for imputed values
  x_imp <- kNN(sleep[, -(8:10)])
  x_imp[,c(1,2,4,6,7)] <- log10(x_imp[,c(1,2,4,6,7)])
  expect_warning(matrixplot(x_imp, delimiter = "_imp", sortby = "BrainWgt",
                            interactive = FALSE))
})

test_that("mosaicMiss missing failed", {
  ## for missing values
  m <- mosaicMiss(sleep, highlight = 4, 
             plotvars = 8:10, miss.labels = FALSE)
  expect_equal(sum(m[1,]),14)
})

test_that("mosaicMiss imputed failed", {
  ## for imputed values
  m <- mosaicMiss(kNN(sleep), highlight = 4, 
             plotvars = 8:10, delimiter = "_imp", miss.labels = FALSE)
  expect_equal(sum(m[1,]),14)
})

test_that("parcoordMiss missing failed", {
  ## for missing values
  expect_null(parcoordMiss(chorizonDL[,c(15,101:110)], 
               plotvars=2:11, interactive = FALSE))
})

test_that("parcoordMiss imputed failed", {
  ## for imputed values
  p <- parcoordMiss(kNN(chorizonDL[,c(15,101:110)]), delimiter = "_imp" ,
               plotvars=2:11, interactive = FALSE)
  expect_true(is.null(p))
})

test_that("scattJitt missing failed", {
  ## for missing values
  expect_null( scattJitt(tao[, c("Air.Temp", "Humidity")]))
})

test_that("scattJitt imputed failed", {
  ## for imputed values
  expect_null(scattJitt(kNN(tao[, c("Air.Temp", "Humidity")]), delimiter = "_imp"))
})

test_that("scattMiss missing failed", {
  ## for missing values
  expect_null(scattMiss(tao[,c("Air.Temp", "Humidity")], interactive = FALSE))
})

test_that("scattMiss imputed failed", {
  ## for imputed values
  expect_null(scattMiss(kNN(tao[,c("Air.Temp", "Humidity")]), delimiter = "_imp", interactive = FALSE))
})

test_that("scattmatrixMiss missing failed", {
  ## for missing values
  x <- sleep[, 1:5]
  x[,c(1,2,4)] <- log10(x[,c(1,2,4)])
  expect_warning(scattmatrixMiss(x, highlight = "Dream", interactive = FALSE))
})

test_that("scattmatrixMiss imputed failed", {
  ## for imputed values
  x_imp <- kNN(sleep[, 1:5])
  x_imp[,c(1,2,4)] <- log10(x_imp[,c(1,2,4)])
  expect_warning(scattmatrixMiss(x_imp, delimiter = "_imp", highlight = "Dream",
                                 interactive = FALSE))
})

test_that("spineMiss missing failed", {
  ## for missing values
  sm <- spineMiss(tao[, c("Air.Temp", "Humidity")], interactive = FALSE)
  spineMiss(sleep[, c("Exp", "Sleep")], interactive = FALSE)
  expect_equal(sum(sm[,1]),93)
})

test_that("spineMiss imputed failed", {
  ## for imputed values
  sm <- spineMiss(kNN(tao[, c("Air.Temp", "Humidity")]), delimiter = "_imp", interactive = FALSE)
  spineMiss(kNN(sleep[, c("Exp", "Sleep")]), delimiter = "_imp", interactive = FALSE)
  expect_equal(sum(sm[,1]),96)
})

test_that("marginplot missing failed", {
  ## for missing values
  expect_null(marginplot(tao[,c("Air.Temp", "Humidity")]))
  expect_null(marginplot(log10(chorizonDL[,c("CaO", "Bi")])))
})

test_that("marginplot imputed failed", {
  ## for imputed values
  expect_null(marginplot(kNN(tao[,c("Air.Temp", "Humidity")]), delimiter = "_imp"))
  expect_null(marginplot(kNN(log10(chorizonDL[,c("CaO", "Bi")])), delimiter = "_imp"))
})


test_that("pbox missing failed", {
  ## for missing values
  suppressWarnings( pb <- pbox(log(chorizonDL[, c(4,5,8,10,11,16:17,19,25,29,37,38,40)]), interactive = FALSE))
  expect_equal(sum(pb$n),7865)
})

test_that("pbox imputed failed", {
  ## for imputed values
  suppressWarnings(pb<- pbox(hotdeck(log(chorizonDL[, c(4,8,10,11,17,19,25,29,37,38,40)])),
       delimiter = "_imp", interactive = FALSE))
  expect_equal(sum(pb$n),6666)
})

