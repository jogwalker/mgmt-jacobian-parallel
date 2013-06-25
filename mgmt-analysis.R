args <- commandArgs(T)

outdir <- args[1]
r <- as.numeric(args[2])
r1 <- as.numeric(args[3])
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

for (i in r1:(r1+r-1)) {
  
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

# make a key
key <- matrix(data=c("strategy1","strategy2","strategy3","strategy4","strategy5","Decrease par_rum_dom","Decrease par_eq_dom","Decrease par_eq_dom and par_rum_dom","Increase carnivores, elephants, wild_eq, wild_rum","Increase carnivores"),nrow=5,ncol=2,byrow=FALSE)

# save file with results
results.nofence <- list(key=key,strategy1=strat1,strategy2=strat2,strategy3=strat3,strategy4=strat4,strategy5=strat5)
save(results.nofence, file=paste(outdir,"press_results_",date,".Rdata",sep=""))

########################################

# simulate fence in mat.filtered and invert again
mat.fenced <- mat.filtered
mat.inverses.f <- mat.filtered
unstable.f <- vector(length=dim(mat.filtered)[3])
prop.conserved <- 0.5 #how much land for .w
fence.efficacy <- 0.9 #how effective is the fence
for (i in 1:dim(mat.filtered)[3]) {
  mat.fenced[,,i] <- buildFence(prop.conserved,fence.efficacy,mat.filtered[,,i],network)
  .stable <- filterMatrix(mat.fenced[,,i])
  if(.stable==TRUE) {    
    mat.inverses.f[,,i] <- invertMatrix(mat.fenced[,,i])
  }
  if(.stable==FALSE) {
    mat.inverses.f[,,i] <- NA
    unstable.f[i] <- TRUE
  }  
  )
}

# compare difference...save
inverses.f.s <- mat.inverses.f[,,which(x==FALSE)]
dims <- list(dim.mat.fenced=dim(mat.fenced),dim.mat.inverses.f.s=dim(mat.inverses.f.s))
mat.all.fenced <- list(dims=dims,mat.fenced=mat.fenced,mat.inverses.f.s=mat.inverses.f.s)
save(mat.all,file=paste(outdir,"mat_all_fenced_",date,".Rdata",sep=""))

# STRATEGY 1: Decrease parasites in domestic ruminants
strat1f <- calcPress(nodes1,sign1,mat.inverses.f.s,network)

# STRATEGY 2: Decrease parasites in domestic equids
strat2f <- calcPress(nodes2,sign2,mat.inverses.f.s,network)

# STRATEGY 3: Decrease parasites in domestic ruminants and equids together
strat3f <- calcPress(nodes3,sign3,mat.inverses.f.s,network)

# STRATEGY 4: Increase wildlife (carnivores, elephants, wild eq & ruminants)
strat4f <- calcPress(nodes4,sign4,mat.inverses.f.s,network)

# STRATEGY 5: Increase carnivores
strat5f <- calcPress(nodes5,sign5,mat.inverses.f.s,network)

# make new key and save file with results
keyf <- key
keyf[,1] <- sapply(key[,1],function(x) {temp <- paste(x,"f",sep=""); return(temp)})
keyf[,2] <- sapply(key[,2],function(x) {temp <- paste(x,"with fence"); return(temp)})
results.fence <- list(key=keyf,strategy1f=strat1f,strategy2f=strat2f,strategy3f=strat3f,strategy4f=strat4f,strategy5f=strat5f)
save(results.fence, file=paste(outdir,"fence_press_results_",date,".Rdata",sep=""))



