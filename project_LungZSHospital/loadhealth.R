
mf <- '/home/zhu/lung_ct_china/healthct'
tf <- '/home/zhu/lung_ct_china/new'
setwd(mf)
sample <- dir()

for(i in sample){
       setwd(paste0(mf,'/',i,'/PAT1'))
       setwd(dir()[1]);setwd(dir()[1])
       files <- dir(pattern='IM')
       system(paste('mkdir',paste(tf,i,sep='/')))
       syn <- paste('cp',files,paste(tf,i,files,sep='/'))
       for (j in syn){
              system(j)
       }
}

##############################

rm(list=ls())

library(oro.dicom)
library(oro.nifti)

rawfolder <- '/home/zhu/lung_ct_china/new/'
f <- function(di){
  print(di)
  setwd(paste0(rawfolder,di))
  data.dicom <- readDICOM(getwd())
  data.nifti <- dicom2nifti(data.dicom)
  data.nifti <- g.process(data.nifti)
  return(data.nifti)
}

setwd(rawfolder)
files <- dir()

system.time(test <- f(files[1]))
