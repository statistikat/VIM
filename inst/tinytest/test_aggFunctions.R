library(VIM)
message("kNN ordered")
d <- data.frame(x=LETTERS[1:6],y=as.double(1:6),z=as.double(1:6),
                w=ordered(LETTERS[1:6]), stringsAsFactors = FALSE)
d <- rbind(d,d)
setna <- function(d,i,col=2){
  d[i,col] <- NA
  d
}
d$w[1] <- "F"
## Test for medianSamp
# medianSamp as expected"
  expect_true(medianSamp(d$w)%in%c("C","D"))
  expect_true(medianSamp(d$w, weights = d$y)=="E")


## Test for maxCat
# maxCat as expected"
  expect_true(maxCat(d$w)=="F")
  expect_true(maxCat(d$w, weights = d$y)=="F")


## Test for sampleCat
# sampleCat as expected"
  expect_true(sampleCat(d$w)%in%LETTERS[1:6])
  expect_true(sampleCat(d$w, weights = d$y)%in%LETTERS[1:6])


