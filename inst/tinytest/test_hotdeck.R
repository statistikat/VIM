message("hotdeck")
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

