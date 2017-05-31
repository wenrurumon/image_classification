
l2a <- function(x){
  s <- length(x)
  d <- dim(x[[1]])
  o <- array(0,c(d,s))
  for(i in 1:s){
    o[,,,i] <- x[[i]]
  }
  o
}
