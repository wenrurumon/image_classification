

rm(list=ls())
library(oro.dicom)
library(oro.nifti)
library(RNiftyReg)
source('/home/zhu/lung_ct_china/3DFPCA.R')

setwd("/home/zhu/lung_ct_china/model")
#dir()
#load('segmentation.rda')
#load('target.rda')
#load('regrlt10_1.rda')

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
#system.time(
#	g.base_sel <- lapply(min(segmentation):max(segmentation),function(i){
#		print(i)
#		seg.sel(i,g.base=segmentation)
#	})
#)
load('g_base_sel.rda')

#Load all graphs
files <- dir(pattern='1.rda')
regrlt <- lapply(files,function(x){
	print(x)
	load(x);
	return(rlt)
})

#####################################

regrlt <- do.call(c,regrlt)
names(regrlt)[is.na(names(regrlt))|names(regrlt)==' 1'] <- paste0('health',1:66)
names(regrlt) <- gsub(' 1','',names(regrlt))

gs <- lapply(regrlt,function(x) x[[1]])
gs_seg <- lapply(g.base_sel,function(segi){
	lapply(gs,function(gi){
		g.sel(gi,segi)
		})
	})
array_seg <- lapply(gs_seg,list2array)
