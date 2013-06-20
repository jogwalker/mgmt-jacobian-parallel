args <- commandArgs(T)

outdir <- args[1]
n <- args[2]
r <- args[3]
date <- Sys.Date()

# read in the necessary functions
source("~/mgmt-jacobian-parallel/mgmt-functions.R")

# load network data
load("~/mgmt-jacobian-parallel/network.rdata")

# read in matrices 
stable <- list()
inverse <- list()
count <- 0
mat.count <- 0

for (i in 1:n) {
  
  filename.m <- paste("~/mgmt-jacobian-parallel/results/mat_stable_",i,".Rdata",sep="")
  filename.i <- paste("~/mgmt-jacobian-parallel/results/mat_inverse_",i,".Rdata",sep="")
  
  if(file.exists(filename.i)) {
    count <- count+1
    
    load(filename.m)
    mat.count <- mat.count + dim(mat.filtered)[3]
    stable[[count]] <- mat.filtered
    
    load(filename.i)
    inverse[[count]] <- inverses
  }
}

# save data in one file

mat.filtered <- array(unlist(stable),dim=c(dim(network),mat.count)))
mat.inverses <- array(unlist(inverse),dim=c(dim(network),mat.count)))

mat.all <- list(mat.filtered=mat.filtered,mat.inverses=mat.inverses)
filename.save1 <- paste(outdir,"mat_all_",date,".Rdata",sep="")
save(mat.all,file=filename.save1)

# create summary 
summary <- list()
summary[["success"]] <- mat.count
summary[["n"]] <- n*r
save(summary, file=paste(outdir,"summary_",date,".Rdata",sep=""))





