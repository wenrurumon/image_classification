
###

library(oro.dicom)
library(oro.nifti)
library(RNiftyReg)

args = commandArgs(trailingOnly=TRUE)
arg1 <- as.numeric(args[[1]])
arg2 <- as.numeric(args[[2]])

#if( length(args)==0 ) { args <- 1 }
setwd("/home/zhu/lung_ct_china/processed")
load('target.rda')
load(paste0('source',arg1,'.rda'))

g.reg <- function(source,target){
  g.step1 <- niftyreg(source,target,scope='affine')
  g.step2 <- niftyreg(source,target,scope="nonlinear",init=forward(g.step1))
  list(affine=g.step1,nonlinear=g.step2)
}

rlt <- lapply(g.temp,function(source){g.reg(source=source,targets=g.target[[arg2]])})
names(rlt) <- paste(names(g.temp),arg2)

save(args,rlt,file=paste0('/home/zhu/lung_ct_china/model/regrlt',arg1,'_',arg2,'.rda'))

###
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
 R CMD BATCH --no-save --no-restore '--args 7 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 7 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 8 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 8 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 9 1' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 9 2' target_regression.R & 
 R CMD BATCH --no-save --no-restore '--args 10 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 10 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 11 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 11 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 12 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 12 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 13 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 13 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 14 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 14 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 15 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 15 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 16 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 16 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 17 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 17 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 18 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 18 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 19 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 19 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 20 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 20 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 21 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 21 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 22 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 22 2' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 23 1' target_regression.R &
 R CMD BATCH --no-save --no-restore '--args 23 2' target_regression.R &
 
