
rm(list=ls())
setwd('/home/zhu/lung_ct_china/model')

#macro

image2block <- function(x,i,j,k){
  ci <- as.numeric(cut(1:dim(x)[1],i))
  cj <- as.numeric(cut(1:dim(x)[2],j))
  ck <- as.numeric(cut(1:dim(x)[3],j))
  sel <- expand.grid(i=1:i,j=1:j,k=1:k)
  out <- lapply(1:nrow(sel),function(r){
    ri <- ci == sel$i[r]
    rj <- cj == sel$j[r]
    rk <- ck == sel$k[r]
    x[ri,rj,rk]
  })
  out
}

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
rm(i)
X2 <- lapply(X,image2block,i=3,j=3,k=3)
Y <- 1-grepl('health',names(X))

