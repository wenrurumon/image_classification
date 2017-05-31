
rm(list=ls())
setwd('/home/zhu/lung_ct_china/model')

#Load data
f <- dir(pattern='regrlt')
raw <- lapply(f,function(x){
  print(x); 
  load(x); 
  return(rlt)
})
names(raw) <- f
raw2 <- do.call(c,raw)
raw2 <- lapply(raw2,function(x){x[[1]]})
raw139 <- raw2[sapply(raw2,dim)[3,]==139]
raw578 <- raw2[sapply(raw2,dim)[3,]==578]
control139 <- grepl('health',names(raw139))
control578 <- grepl('health',names(raw578))

#process X
X <- raw139
i <- 0
X <- lapply(X,function(x){
  print(i<<-i+1)
  xn <- min(x,na.rm=TRUE)
  xx <- max(x,na.rm=TRUE)
  x <- (x-xn)/(xx-xn)
  x
})
i <- 0
X2 <- lapply(X,function(x){
  print(i<<-i+1)
  image2block(x,i=3,j=3,k=3)
})
Y <- 1-grepl('health',names(X))
i <- 0
X2_mean <- sapply(X2,function(x){
  print(i<<-i+1)
  sapply(x,mean,na.rm=TRUE)
})
