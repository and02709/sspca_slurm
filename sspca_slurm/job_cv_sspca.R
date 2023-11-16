args <- commandArgs()
# define arguments
setwd <- args[6]
index <- as.numeric(args[7])
npc <- as.numeric(args[8])
nfolds <- as.numeric(args[9])
stype <- args[10]
kernel <- args[11]
niter <- as.numeric(args[12])
trace <- as.numeric(args[13])

cat("setwd: ",setwd,"\n")
cat("index: ", index,"\n")
cat("npc: ",npc,"\n")
cat("nfolds: ",nfolds,"\n")
cat("stype: ",stype,"\n")
cat("kernel: ",kernel,"\n")
cat("niter: ",niter,"\n")
cat("trace: ",trace,"\n")

cat("load libraries \n")
library(tidyverse)
library(sgmeth2)

dfpath <- paste(setwd,"temp/df.txt",sep="")
parampath <- paste(setwd, "temp/param.txt",sep="")
df <- read.table(file=dfpath, header=T)
paramgrid <- read.table(file=parampath, header=T)

if(stype=="sumabs"){
  sspca.obj <- cv.partition.SSPCA(arg.sparse=paramgrid[index,],df.partition=df,npc=npc,n.folds=nfolds,sparsity.type="sumabs",sumabsv=NULL,kernel=kernel,niter=niter,trace=trace)
} else{
  sspca.obj <- cv.partition.SSPCA(arg.sparse=paramgrid[index,],df.partition=df,npc=npc,n.folds=nfolds,sparsity.type="loadings",nonzero.loadings=NULL,kernel=kernel,niter=niter,trace=trace)
}
fpath <- paste(setwd,"temp/cv_outputs/job_",index,".txt",sep="")
data.obj <- data.frame(job=index,fold=paramgrid[index,1],sparse=paramgrid[index,2],cv.metric=sspca.obj)
write.table(data.obj,file=fpath,quote=F,row.names=F,col.names=T)
