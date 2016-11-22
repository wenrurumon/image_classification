

rm(list=ls())
library(oro.dicom)
library(oro.nifti)
library(RNiftyReg)
source('/home/zhu/lung_ct_china/3DFPCA.R')

setwd("/home/zhu/lung_ct_china/model")
#dir()

load('segmentation.rda')
load('target.rda')
load('regrlt10_1.rda')

list2array <- function(X){
  out <- array(0,dim=c(dim(X[[1]]),length(X)))
  for(i in 1:length(X)){
  	print(i)
    out[,,,i] <- X[[i]]
  }
  out
}

seg.sel <- function(i,g.base){
	temp <- (g.base==i)
	dimsum <- lapply(1:3,function(j){apply(temp,j,sum)})
	dimsel <- lapply(dimsum,function(x) range(which(x>0)))
	#lapply(dimsel,function(x) x[[1]]:x[[2]])
	return(dimsel)
}
g.sel <- function(g,sel){
	g[sel[[1]][1]:sel[[1]][2],sel[[2]][1]:sel[[2]][2],sel[[3]][1]:sel[[3]][2]]
}

#####################################

#Generate selection array by segmentation
system.time(
	g.base_sel <- lapply(min(segmentation):max(segmentation),function(i){
		print(i)
		seg.sel(i,g.base=segmentation)
	})
)

#generate the list of graphs
gs <- lapply(rlt,function(x) x[[1]])
g1 <- lapply(gs,function(x) x[g.seg[[1]]])

#####################################

