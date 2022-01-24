library(VIM)
message("kNN ordered")
d <- data.frame(x=LETTERS[1:6],y=as.double(1:6),z=as.double(1:6),
                w=ordered(LETTERS[1:6]), stringsAsFactors = FALSE)
d <- rbind(d,d)
setna <- function(d,i,col=2){
  d[i,col] <- NA
  d
}




# kNN ordered Tests for k==1",{
  d2 <- kNN(setna(d,7:12,4),k=1)
  expect_false(any(is.na(d2$w)))
  expect_equal(sum(d2$w_imp),6L)
# 

# kNN ordered Tests for k==2",{
  d2 <- kNN(setna(d,7:12,4),k=2)
  expect_false(any(is.na(d2$w)))
  expect_equal(sum(d2$w_imp),6L)
# 

# kNN ordered Tests for k==1 weighted",{
  d2 <- kNN(setna(d,7:12,4),k=1, weightDist = TRUE)
  expect_false(any(is.na(d2$w)))
  expect_equal(sum(d2$w_imp),6L)
# 

# kNN ordered Tests for k==2",{
  d2 <- kNN(setna(d,7:12,4),k=2, weightDist = TRUE)
  expect_false(any(is.na(d2$w)))
  expect_equal(sum(d2$w_imp),6L)
# 

