
rm(list=ls())

####################
# Process
####################

g.grad <- function(g){
	g.data <- g@.Data
	g.grad <- array(0,dim=dim(g.data))
	for(i in 3:dim(g.data)[1]-1){
		for(j in 3:dim(g.data)[2]-1){
			for(k in 3:dim(g.data)[3]-1){
				g.grad[i,j,k] <- max(abs(g.data[i,j,k]-as.vector(g.data[i+(-1):1,j+(-1):1,k+(-1):1])))
	}}}
	g@.Data <- g.grad
	return(g)
}
g.process <- function(g){ 
	g@.Data[g@.Data<0] <- 0
	return(g)
}

####################
# Load Rawdata
####################

library(oro.dicom)
library(oro.nifti)

rawfolder <- '/home/zhu/lung_ct_china/raw/'
f <- function(di){
  print(di)
  setwd(paste0(rawfolder,di))
  data.dicom <- readDICOM(getwd())
  data.nifti <- dicom2nifti(data.dicom)
  data.nifti <- g.process(data.nifti)
  return(data.nifti)
}

files.target <- dir(rawfolder,pattern='229')
g.target <- lapply(files.target,f)
names(g.target) <- files.target
save(g.target,file='/home/zhu/lung_ct_china/processed/target.rda')

files <- dir(rawfolder)
for(i in 1:23){
	ftemp <- files[1:10+(i-1)*10]
	print(i)
	g.temp <- lapply(ftemp,f)
	names(g.temp) <- ftemp
	save(g.temp,file=paste0('/home/zhu/lung_ct_china/processed/source',i,'.rda'))
}
