
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
g.process <- function(g){ 
       g@.Data[g@.Data<0] <- 0
       return(g)
}

setwd(rawfolder)
files <- dir()

g.health <- lapply(files,f)
names(g.health) <- files

for(i in 1:6){
       g.temp <- g.health[1:11 + (i-1)*11]
       save(g.temp,file=paste0('/home/zhu/lung_ct_china/processed/health',i,'.rda'))
}

save(g.health,file='/home/zhu/lung_ct_china/processed/health.rda')

########################################

load('/home/zhu/lung_ct_china/processed/target.rda')
g.reg <- function(source,target){
  g.step1 <- niftyreg(source,target,scope='affine')
  #g.step2 <- niftyreg(source,target,scope="nonlinear",init=forward(g.step1))
  list(affine=g.step1,nonlinear=g.step2)
}
rlt <- lapply(g.health,function(source){g.reg(source=source,targets=g.target[[1]])})

#############################################

rm(list=ls())

library(oro.dicom)
library(oro.nifti)
library(RNiftyReg)

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
       arg1 <- 1; arg2 <- 2
} else {
       arg1 <- as.numeric(args[[1]])
       arg2 <- as.numeric(args[[2]])
}

setwd('/home/zhu/lung_ct_china/processed')
load('target.rda')
load(paste0('health',arg1,'.rda'))

i <- 0
g.reg <- function(source,target){
       print(paste(arg1,arg2,i<<-i+1))
       return(niftyreg(source,target,scope='affine'))
}

rlt <- lapply(g.temp,function(source){g.reg(source=source,target=g.target[[arg2]])})
names(rlt) <- paste(names(g.temp),arg2)

save(args,rlt,file=paste0('/home/zhu/lung_ct_china/model/regrlt_health',arg1,'_',arg2,'.rda'))

#

 R CMD BATCH --no-save --no-restore '--args 1 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 1 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 2 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 2 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 3 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 3 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 4 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 4 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 5 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 5 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 6 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 6 2' target_regression.R & 
