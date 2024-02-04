args <- commandArgs()
# define arguments
setwd <- args[6]
xfile <- args[7]
yfile <- args[8]
npc <- as.numeric(args[9])
nfolds <- as.numeric(args[10])
sparams <- args[11]
stype <- args[12]
kernel <- args[13]
niter <- as.numeric(args[14])
trace <- as.numeric(args[15])
balance <- as.numeric(args[16])
cat("setwd: ",setwd,"\n")
cat("xfile: ", xfile,"\n")
cat("yfile: ",yfile,"\n")
cat("npc: ",npc,"\n")
cat("nfolds: ",nfolds,"\n")
cat("sparams: ",sparams,"\n")
cat("stype: ",stype,"\n")
cat("kernel: ",kernel,"\n")
cat("niter: ",niter,"\n")
cat("trace: ",trace,"\n")
cat("balance: ",balance,"\n")
## check parameters
cat("read in arguments \n")
cat("about to read in data \n")
X <- read.table(xfile, header=T)
cat("successfully loaded X data \n")
Y <- read.table(yfile)
cat("successfully loaded Y data \n")
X <- as.matrix(X)
cat("converted X to matrix \n")
Y <- as.matrix(Y)
cat("converted Y to matrix \n")
spm <- read.table(file=sparams, header=F) |> as.matrix() |> as.vector()
cat("read in sparse data \n")
cat("read in and converted all data \n")
n <- nrow(X)
p <- ncol(X)
cat("determining n and p \n")
cat("label data \n")
colnames(X) <-paste0("x",c(1:p))
colnames(Y) <- "y"
cat("data successfully loaded \n")

if(nrow(Y)!=n) stop("number of observations in predictors and response must match")

if(stype!="loadings" && stype!="sumabs") stop("Please select a valid sparsity type")

if(stype=="loadings"){
    if(min(spm) < 1) stop("minimum number of nonzero loadings must be greater than or equal to one")
    if(max(spm) > p) stop("maximum number of nonzero loadings must be less than or equal to the number of predictors")
    sp.arg <- spm
  }
  if(stype=="sumabs"){
    if(min(spm) < 1) stop("minimum number of nonzero loadings must be greater than or equal to one")
    if(max(spm) > sqrt(p)) stop("maximum number of nonzero loadings must be less than or equal to the square root of the number of predictors")
    sp.arg <- spm
  }
cat("successfully passed QC sparse parameters step \n")

if(kernel!="linear" && kernel!="delta") stop("Please select a valid kernel")
df <- data.frame(Y,X)
cat("combined Y and X into dataframe \n")
n.sp <- length(sp.arg)
cat("count number of sparse arguments to cross-validate \n")
cat("set up arguments \n")
if(kernel=="delta" && balance){
  df.partition <- groupdata2::fold(data=df,k=nfolds,cat_col = "y")
} else{
  df.partition <- groupdata2::fold(data=df,k=nfolds)
}
cat("partitioned data successfully \n")
fold.arg <- c(1:nfolds)
param.grid <- expand.grid(fold.arg,sp.arg)
colnames(param.grid) <- c("fold.arg","sp.arg")
cat("set up argument grid \n")
fpath <- paste(setwd,"temp",sep="")
dfpath <- paste(fpath,"/df.txt",sep="")
parampath <- paste(fpath,"/param.txt",sep="")
spath <- paste(fpath,"/sparg.txt",sep="")
cat("set up paths \n")
write.table(df.partition,file=dfpath,row.names = F,col.names = T,quote=F)
cat("save partitioned data \n")
write.table(param.grid,file=parampath,row.names = F,col.names = T,quote=F)
cat("save parameter grid \n")
write.table(sp.arg,file=spath,quote=F)
cat("save sparse argument vector \n")
