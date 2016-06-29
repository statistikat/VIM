require(VIM)
d <- data.frame(x=LETTERS[1:6],y=as.double(1:6),z=as.double(1:6),w=ordered(LETTERS[1:6]))
d <- rbind(d,d)
setna <- function(d,i,col=2){
  d[i,col] <- NA
  d
}
## Tests for k==1
d1 <- kNN(setna(d,7:12,1)[,1:2],k=1)
d2 <- kNN(setna(d,7:12,2)[,1:2],k=1)
d3 <- kNN(setna(d,7:12,3)[,1:3],k=1,dist_var = c("x","y"),variable = "z")
d4 <- kNN(setna(d,7:12,3)[-1,3:4],k=1)
if(!all(d1[7:12,1]==d1[1:6,1])||!all(d2[7:12,2]==d2[1:6,2])||
   !all(d3[7:12,3]==d3[1:6,3])||!(all(d4[6:11,1]==d4[c(1,1:5),1]))
  ){
  stop("kNN does not work as supposed.")
}
## Tests for k==2
d <- rbind(d,d[1:6,])
d[13:18,2] <- d[13:18,2]*2
d2 <- kNN(setna(d,7:12,2)[,1:2],k=2,numFun = mean)

if(!all(d2[7:12,2]==(d2[1:6,2]+d2[13:18,2])/2)){
  stop("kNN does not work as supposed.")
}
