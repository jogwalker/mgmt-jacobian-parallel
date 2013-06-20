args <- commandArgs(T)

outdir <- args[1]
jid <- as.numeric(args[2])

# read in the necessary functions
source("~/mgmt-jacobian-parallel/mgmt-functions.R")

# load network data
load("~/mgmt-jacobian-parallel/network.rdata")

# run simulations
set.seed(jid)

n <- 100000
mat.stable <- list()
count <- 0

for (i in 1:n) {
  mat <- createMatrix(network)
  stable <- filterMatrix(mat)
  if (stable==TRUE) {
    count <- count+1
    mat.stable[[count]] <- mat
  }
}

# save stable ones as array instead of list
mat.filtered <- array(unlist(mat.stable),dim=c(dim(network),length(mat.stable)))

inverses <- array(NA,dim=dim(mat.filtered))

if (count > 0)) {
  for (i in 1:dim(mat.filtered)[3]) {
    inverses[,,i] <- invertMatrix(mat.filtered[,,i])
  }
}

# write output files
filename1 <- paste(outdir,"mat_stable_",jid,".Rdata",sep="")
filename2 <- paste(outdir,"mat_inverse_",jid,".Rdata",sep="")
# filename3 <- paste(outdir,"mat_fenced_",jid,".Rdata",sep="")
save(mat.filtered, file=filename1)
save(inverses,file=filename2)
# save(fenced,file=filename3)

