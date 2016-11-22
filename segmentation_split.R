

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

seg.sel <- function(i,g.base,g.out = NULL){
	temp <- (g.base==i)
	dimsum <- lapply(1:3,function(j){apply(temp,j,sum)})
	dimsel <- lapply(dimsum,function(x) x>0)
	if (is.null(g.out)) {
		return(dimsel)
	} else {
		return(g.out[dimsel[[1]],dimsel[[2]],dimsel[[3]]])
	}
}

seg.list <- function(g.base){
	is <- unique(as.vector(g.base))	
	lapply(sort(is),function(i){
		print(i)
		temp <- (g.base==i)
		dimsum <- lapply(1:3,function(j){apply(temp,j,sum)})
		dimsel <- lapply(dimsum,function(x) x>0)
		g.out <- array(FALSE,dim(g.base))
		g.out[dimsel[[1]],dimsel[[2]],dimsel[[3]]] <- TRUE
		return(g.out)
	})
}

#####################################

#Generate selection array by segmentation
system.time(g.seg <- seg.list(segmentation))

#generate the list of graphs
gs <- lapply(rlt,function(x) x[[1]])

#####################################

