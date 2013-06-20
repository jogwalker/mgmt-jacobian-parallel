args <- commandArgs(T)

outdir <- args[1]
n <- args[2]

# read in the necessary functions
source("~/mgmt-jacobian-parallel/mgmt-functions.R")

# load network data
load("~/mgmt-jacobian-parallel/network.rdata")

# read in inverses 
success.num <- vector(length=n)
all <- list()
for (i in 1:n) {
    load(paste("C:/Users/Josephine/Dropbox/Dropbox Documents/Bristol_PhD/R/mgmt-jacobian-parallel/inversemats/mat_neginverse_",i,".Rdata",sep=""))
    all[[i]] <- inverses
    success.num[i] <- length(inverses)/289
}

