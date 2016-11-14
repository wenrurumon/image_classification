rm(list=ls())

library(MASS)
library(GenABEL)
library(oro.dicom)
library(oro.nifti)
library(RNiftyReg)

qpca <- function(A,rank=0){
  A <- scale_ivn(A)
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-10]
  r <- length(d)
  prop <- d^2; prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop)
  return(rlt)
}
pca <- function(X,ifscale=T){
  if(ifscale) {X <- scale(as.matrix(X))}
  m = nrow(X)
  n = ncol(X)
  X = scale(X)
  Xeigen <- svd(as.matrix(X))
  value <- (Xeigen$d)^2/m
  value <- cumsum(value/sum(value))
  score <- X %*% Xeigen$v
  mat <- Xeigen$v
  list(score=score,prop=value,mat=mat)
}
scale_ivn <- function(x){apply(x,2,rntransform)}
subsample <- function(n,ngroup){
  n <- c(cut(sample(1:n),ngroup))
  lapply(1:ngroup,function(i){n==i})
}
eva <- function(mat){sum(diag(mat))/sum(mat)}

setwd('C:\\Users\\zhu2\\Documents\\Lung_CT_CHINA\\processed')
disease <- unique(read.csv('samplemap.csv')[,c(1,5)])
disease <- tapply(disease$disease,disease$sample,max)
load('gs_feature.rda')

x1 <- t(sapply(gs1_feature,function(x){x[,1]})); x1[is.na(x1)] <- 0; x1 <- scale(x1)[,]
x2 <- t(sapply(gs2_feature,function(x){x[,1]})); x2[is.na(x2)] <- 0; x2 <- scale(x2)[,]
gs1_na <- sapply(gs1_feature,function(x) sum(x[,2]))
gs2_na <- sapply(gs2_feature,function(x) sum(x[,2]))

colnames(x1) <- colnames(x2) <- paste0('block',1:125)
x.sample <- as.numeric(sapply(rownames(x1),function(x){strsplit(x,' |_')[[1]][1]}))
x1 <- x1[x.sample!=39,]; x2 <- x2[x.sample!=39,]
gs1_na <- gs1_na[x.sample!=39]; gs2_na <- gs2_na[x.sample!=39]
x.sample <- as.numeric(sapply(rownames(x1),function(x){strsplit(x,' |_')[[1]][1]}))
names(gs1_na) <- names(gs2_na) <- rownames(x1) <- rownames(x2) <- x.sample
disease <- disease[match(paste(x.sample),names(disease))]

#sel

sel1 <- (disease==0)|(gs1_na<=sort(gs1_na[disease==1])[40])
sel2 <- (disease==0)|(gs2_na<=sort(gs2_na[disease==1])[40])
d1 <- disease[sel1]; x1 <- x1[sel1,]
d2 <- disease[sel2]; x2 <- x2[sel2,]
f1 <- qpca(x1);f1 <- qpca(x1,rank=which(f1$prop>=0.99)[1])$X
f2 <- qpca(x2);f2 <- qpca(x2,rank=which(f2$prop>=0.99)[1])$X
f3 <- pca(x1);f3 <- f3$score[,1:which(f3$prop>=0.99)[1]]
f4 <- pca(x2);f4 <- f4$score[,1:which(f4$prop>=0.99)[1]]

list(
  g1.qlda <- table(d=d1,p=predict(lda(d1~f1))$class)  
  ,g2.qlda <- table(d=d2,p=predict(lda(d2~f2))$class)
  ,g1.lda <- table(d=d1,p=predict(lda(d1~f3))$class)
  ,g2.lda <- table(d=d2,p=predict(lda(d2~f4))$class)
)

sapply(list(
  g1.qlda <- table(d=d1,p=predict(lda(d1~f1))$class)  
  ,g2.qlda <- table(d=d2,p=predict(lda(d2~f2))$class)
  ,g1.lda <- table(d=d1,p=predict(lda(d1~f3))$class)
  ,g2.lda <- table(d=d2,p=predict(lda(d2~f4))$class)
),eva)

table(d1,predict(glm(d1~f1[,1:25],family = binomial(link='logit')))>0)
table(d2,predict(glm(d2~f2[,1:33],family = binomial(link='logit')))>0)


#train and test

seeds <- function(seed){
  print(seed)
  set.seed(seed);trainsel <- subsample(length(d1),2)
  # lapply(trainsel,function(x){tapply(d1[x],d1[x],length)})
  # lapply(trainsel,function(x){tapply(d2[x],d2[x],length)})
  
  d <- d1; x <- x1; s <- trainsel
  
  di1 <- d[s[[1]]]
  xi1 <- x[s[[1]],]
  xi1.pca <- pca(xi1)
  mat.pca <- pca(xi1)$mat[,1:which(xi1.pca$prop>0.98)[1]]
  xi1 <- xi1%*%mat.pca
  mat.lda <- lda(di1~xi1)$scaling
  pi1 <- (xi1 %*% mat.lda)>0
  train1 <- table(di1,pi1)
  
  di2 <- d[s[[2]]]
  xi2 <- x[s[[2]],]%*%mat.pca
  pi2 <- (xi2 %*% mat.lda)>0
  test1 <- table(di2,pi2)
  
  d <- d2; x <- x2; s <- trainsel
  
  di1 <- d[s[[1]]]
  xi1 <- x[s[[1]],]
  xi1.pca <- pca(xi1)
  mat.pca <- pca(xi1)$mat[,1:which(xi1.pca$prop>0.98)[1]]
  xi1 <- xi1%*%mat.pca
  mat.lda <- lda(di1~xi1)$scaling
  pi1 <- (xi1 %*% mat.lda)>0
  train2 <- table(di1,pi1)
  
  di2 <- d[s[[2]]]
  xi2 <- x[s[[2]],]%*%mat.pca
  pi2 <- (xi2 %*% mat.lda)>0
  test2 <- table(di2,pi2)
  
  list(seed=seed,
       rlt=cbind(train1=train1,test1=test1,train2=train2,test2=test2),
       eva=c(seed,eva(train1),eva(test1),eva(train2),eva(test2)))
}

test <- lapply(1:20000,function(x){try(seeds(x))})
test <- t(sapply(test[sapply(test,is.list)],function(x){x$eva}))
summary(test)
