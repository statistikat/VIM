message("hotdeck")
library(data.table)
# some basic tests for hotdeck
# 
# Author: alex
###############################################################################
set.seed(104)
dfx <- data.frame(unit_id=101:104, state=rep("NSW",4), wages01=c(NA,NA,NA,229305),r=runif(4))
df2 <- data.frame(unit_id=c(NA,101:104), state=rep("NSW",5), wages01=c(NA,NA,NA,2434,229305),
                  r=runif(5), fac=c(NA,NA,"a","b","c"))
# hotdeck should fill all values", {
  df.out <- hotdeck(dfx, variable="wages01", domain_var="state")
  expect_identical(df.out,na.omit(df.out))


# matchImpute should fill all values", {
  set.seed(104)
  df.out2 <- matchImpute(dfx, variable="wages01",match_var="state")
  expect_identical(df.out2,na.omit(df.out2))


# hotdeck should fill all values but give a warning", {
  set.seed(104)
  expect_warning(df.out <- hotdeck(dfx, variable="wages01", domain_var="state",ord_var = "r"))
  expect_identical(df.out,na.omit(df.out))


# hotdeck with colnames starting with a number", {
  dfx = data.frame(Id = 1:3, x = c(NA, 0.2, 0.3))
  set.seed(1)
  dfi <- hotdeck(dfx)
  names(dfx) = c("Id", "1x")
  set.seed(1)
  dfi2 <- hotdeck(dfx)
  colnames(dfi) <- colnames(dfi2)
  expect_identical(dfi,dfi2)


## Test for missings at the beginning of the data set
# hotdeck should fill all values including the first one", {
  df.out <- hotdeck(df2, variable=c("unit_id","wages01","fac"))
  expect_identical(df.out,na.omit(df.out))


# hotdeck should fill all values including the first one - domain_var", {
  df.out <- hotdeck(df2, variable=c("unit_id","wages01","fac"), domain_var="state")
  expect_identical(df.out,na.omit(df.out))


# hotdeck should fill all values including the first one - ord_var", {
  df.out <- hotdeck(df2, variable=c("unit_id","wages01","fac"), ord_var="r")
  expect_identical(df.out,na.omit(df.out))


# hotdeck should fill all values including the first one - ord_var", {
  df.out <- hotdeck(df2, variable=c("fac"), ord_var="r")
  expect_identical(df.out$fac,na.omit(df.out$fac))



# hotdeck returns an error when ord_var and variable overlap", {
  expect_error(hotdeck(df2, variable=c("fac","r"), ord_var="r"))

# donorcond exclude the correct observations",{
  data(sleep)
  sleepI3 <- hotdeck(
   sleep,
   variable = c("NonD", "Dream", "Sleep", "Span", "Gest"),
   ord_var = "BodyWgt", domain_var = "Pred",
   donorcond = list(">4", "<6", ">8.5", "%between%c(8,13)", ">15"))
   expect_true(all(sleepI3[,"NonD"][sleepI3$NonD_imp]>4))
   expect_true(all(sleepI3[,"Dream"][sleepI3$Dream_imp]<6))
   expect_true(all(sleepI3[,"Sleep"][sleepI3$Sleep_imp]>8.5))
   expect_true(all(sleepI3[,"Span"][sleepI3$Span_imp]%between%c(8,13)))
   expect_true(all(sleepI3[,"Gest"][sleepI3$Gest_imp]>15))
   



# multiple donorcond per variable exclude the correct observations",{
  data(sleep)
  sleepI3 <- hotdeck(
    sleep,
    variable = c("NonD", "Dream", "Sleep", "Span", "Gest"),
    ord_var = "BodyWgt", domain_var = "Pred",
    donorcond = list(c(">4","<48"), c("<6",">1"), NULL, NULL, NULL))
  expect_true(all(sleepI3[,"NonD"][sleepI3$NonD_imp]>4))
  expect_true(all(sleepI3[,"NonD"][sleepI3$NonD_imp]<48))
  expect_true(all(sleepI3[,"Dream"][sleepI3$Dream_imp]<6))
  expect_true(all(sleepI3[,"Dream"][sleepI3$Dream_imp]>1))

# A recoded sentinel cannot donate to the next row, even if it passes donorcond.
sentinel_data <- data.frame(order = 1:4, value = c(4, 99, NA, 7))
sentinel_result <- hotdeck(sentinel_data, ord_var = "order", makeNA = list(99),
                           donorcond = list("> 0"))
expect_identical(sentinel_result$value, c(4, 4, 4, 7))
expect_identical(sentinel_result$value_imp, c(FALSE, TRUE, TRUE, FALSE))

# Consecutive sentinels are filled while the original NA is preserved.
sentinel_data <- data.frame(order = 1:5, value = c(4, -99, -98, NA, 7))
sentinel_result <- hotdeck(sentinel_data, ord_var = "order", impNA = FALSE,
                           makeNA = list(c(-99, -98)))
expect_identical(sentinel_result$value, c(4, 4, 4, NA, 7))
expect_identical(sentinel_result$value_imp, c(FALSE, TRUE, TRUE, FALSE, FALSE))

# Ties in the requested ordering retain the input row order.
tied_data <- data.frame(order = c(1L, 1L, 1L, 2L), value = c(2, 99, 3, 4))
tied_result <- hotdeck(tied_data, ord_var = "order", impNA = FALSE,
                       makeNA = list(99))
expect_identical(tied_result$value, c(2, 2, 3, 4))

# Removing NAs for one target must not change the donor order for the next.
tied_data <- data.frame(
  group = rep(c("a", "b"), each = 4L), order = rep(c(1L, 1L, 1L, 2L), 2L),
  first = c(NA_real_, 2, NaN, 4, rep(NA_real_, 4L)),
  second = rep(c(10, 99, 30, 40), 2L)
)
tied_result <- hotdeck(tied_data, ord_var = "order", domain_var = "group",
                       impNA = FALSE, makeNA = list(NULL, 99))
expect_identical(tied_result$first, tied_data$first)
expect_identical(tied_result$second, rep(c(10, 10, 30, 40), 2L))

