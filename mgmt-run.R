args <- commandArgs(T)

outdir <- args[1]
jid <- as.numeric(args[2])

# read in the necessary functions
source("~/mgmt-jacobian-parallel/mgmt-functions.R")

# load network data
load("~/mgmt-jacobian-parallel/network.rdata")

# run simulations
set.seed(jid)

n <- 10000

mat.all <- array(dim=c(dim(network),n))
stable <- vector(length=n)

for (i in 1:n) {
  mat.all[,,i] <- createMatrix(network)
  stable[i] <- filterMatrix(mat.all[,,i])
}

inverses <- array(dim=c(dim(network),sum(stable)))
stable.ind <- which(stable==TRUE)

if (length(stable.ind > 0)) {
  for (i in 1:sum(stable)) {
    mat <- mat.all[stable.ind[i]]
    inverses[,,i] <- invertMatrix(mat)
  }
}


# simulate the fence
# fenced <- buildFence(filtered)

# write output files
filename1 <- paste(outdir,"mat_stable_",jid,".Rdata",sep="")
filename2 <- paste(outdir,"mat_neginverse_",jid,".Rdata",sep="")
# filename3 <- paste(outdir,"mat_fenced_",jid,".Rdata",sep="")
save(list(mat.all,stable), file=filename1)
save(inverses,file=filename2)
# save(fenced,file=filename3)

