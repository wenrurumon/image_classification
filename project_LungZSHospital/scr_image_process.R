
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
