args <- commandArgs(T)

outdir <- args[1]
r <- as.numeric(args[2])
date <- Sys.Date()

# read in the necessary functions
source("~/mgmt-jacobian-parallel/mgmt-functions.R")

# load network data
load("~/mgmt-jacobian-parallel/network.rdata")
load("~/mgmt-jacobian-parallel/n.rdata")

# read in matrices 
stable <- list()
inverse <- list()
count <- 0
mat.count <- 0

for (i in 1:r) {
  
  filename.m <- paste("~/mgmt-jacobian-parallel/results/mat_stable_",i+100,".Rdata",sep="")
  filename.i <- paste("~/mgmt-jacobian-parallel/results/mat_inverse_",i+100,".Rdata",sep="")
  
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

mat.filtered <- array(unlist(stable),dim=c(dim(network),mat.count))
mat.inverses <- array(unlist(inverse),dim=c(dim(network),mat.count))

mat.all <- list(mat.filtered=mat.filtered,mat.inverses=mat.inverses)
filename.save1 <- paste(outdir,"mat_all_",date,".Rdata",sep="")
save(mat.all,file=filename.save1)

# create summary 
summary <- list()
summary[["success"]] <- mat.count
summary[["nsims"]] <- n*r
save(summary, file=paste(outdir,"summary_",date,".Rdata",sep=""))

#########################################

# calculate press perturbation results

# STRATEGY 1: Decrease parasites in domestic ruminants
nodes1 <- "par_rum_dom"
sign1 <- "decrease"
strat1 <- calcPress(nodes1,sign1,mat.inverses,network)

# STRATEGY 2: Decrease parasites in domestic equids
nodes2 <- "par_eq_dom"
sign2 <- "decrease"
strat2 <- calcPress(nodes2,sign2,mat.inverses,network)

# STRATEGY 3: Decrease parasites in domestic ruminants and equids together
nodes3 <- c("par_rum_dom","par_eq_dom")
sign3 <- "decrease"
strat3 <- calcPress(nodes3,sign3,mat.inverses,network)

# STRATEGY 4: Increase wildlife (carnivores, elephants, wild eq & ruminants)
nodes4 <- c("carnivores","elephants","wild_eq","wild_rum")
sign4 <- "increase"
strat4 <- calcPress(nodes4,sign4,mat.inverses,network)

# STRATEGY 5: Increase carnivores
nodes5 <- "carnivores"
sign5 <- "increase"
strat5 <- calcPress(nodes5,sign5,mat.inverses,network)

########################################

# simulate fence in mat.filtered
mat.fenced <- buildFence(mat.all) # or loop individual ones?

# invert again
mat.inverses.f <- mat.fenced
for (i in 1:dim(mat.fenced[3])) {
  mat.inverses.f[,,i] <- invertMatrix(mat.fenced[,,i])
}


# compare difference...

# STRATEGY 1: Decrease parasites in domestic ruminants
strat1f <- calcPress(nodes1,sign1,mat.inverses.f,network)

# STRATEGY 2: Decrease parasites in domestic equids
strat2f <- calcPress(nodes2,sign2,mat.inverses.f,network)

# STRATEGY 3: Decrease parasites in domestic ruminants and equids together
strat3f <- calcPress(nodes3,sign3,mat.inverses.f,network)

# STRATEGY 4: Increase wildlife (carnivores, elephants, wild eq & ruminants)
strat4f <- calcPress(nodes4,sign4,mat.inverses.f,network)

# STRATEGY 5: Increase carnivores
strat5f <- calcPress(nodes5,sign5,mat.inverses.f,network)




