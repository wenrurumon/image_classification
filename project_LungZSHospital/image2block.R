x <- array(rnorm(1000),c(10,10,10))

image2block <- function(x,i,j,k,smooth=F){
  if(smooth){
    image2block_smooth(x,i,j,k)
  } else {
    image2block_ori(x,i,j,k)
  }
}

image2block_ori <- function(x,i,j,k){
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
  names(out) <- apply(sel,1,paste,collapse=',')
  return(out)
}

image2block_smooth <- function(x,i,j,k){
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
  names(out) <- apply(sel,1,paste,collapse=',')
  outv <- sapply(out,mean)
  out <- array(NA,apply(sel,2,max))
  for(r in 1:nrow(sel)){
    rsel <- sel[r,]
    out[as.numeric(rsel[1]),as.numeric(rsel[2]),as.numeric(rsel[3])] <- outv[r]
  }
  return(out)
}
