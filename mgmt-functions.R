###Function to determine how unknown links are handled
# input is length of vector of parameters to be simulated

#-------------

unknown.links <- function(n) {
  n <- n
  param.vector <- rep(NA,n)
  p <- runif(1,0,1) # how many will be zero
  sign <- 0.5 # what proportion positive 
  p.vector <- rbinom(n,1,p)
  sign.vector <- rbinom(n,1,sign)
  sign.vector[which(sign.vector==0)] <- -1
  param.vector <- rbeta(n,0.5,1)*p.vector*sign.vector
  return(param.vector)
}


### function to simulate the parameters for all

simParamsAll <- function(n,network) {
  n <- n # number of simulation runs
  .network <- network
  if(class(.network)=="matrix") {
    .network <- melt(.network) 
    names(.network)  <- c("to","from","class")
  }
  
  #create a data frame to hold the parameter estimates
  parameters <- data.frame(matrix(nrow=nrow(.network),ncol=n,dimnames=list(NULL,seq(1,n,1))))
  
  # create a list of indices for each link class based on network
  link.class <- as.character(unique(.network$class))
  n.class <- length(link.class)
  link.index <- data.frame(matrix(ncol=n.class,nrow=nrow(parameters)))
  names(link.index) <- link.class
  for (i in 1:n.class) {
    link.index[,link.class[i]] <- as.logical(match(.network$class,link.class[i]))
  }
  link.index[is.na(link.index)] <- FALSE # clean up NAs
  
  # simulate values based on each class for each iteration
  # here they are defined explicitly for each class, so this has to change if your classes change
  # "ab" "uk" "dd" "co" "py" "na" "mu" "pr" "in"
  for (i in 1:n) {
    # standard beta distribution (positive or negative)
    parameters[which(link.index$ab),i] <- rbeta(sum(link.index$ab),0.5,1)
    parameters[which(link.index$co),i] <- -rbeta(sum(link.index$co),0.5,1)
    parameters[which(link.index$py),i] <- rbeta(sum(link.index$py),0.5,1)
    parameters[which(link.index$mu),i] <- rbeta(sum(link.index$mu),0.5,1)
    parameters[which(link.index$pr),i] <- -rbeta(sum(link.index$pr),0.5,1)
    parameters[which(link.index$"in"),i] <- -rbeta(sum(link.index$"in"),0.5,1)
    #other distributions
    parameters[which(link.index$uk),i] <- unknown.links(sum(link.index$uk))
    parameters[which(link.index$dd),i] <- -runif(sum(link.index$dd),0,1)
    parameters[which(link.index$na),i] <- 0
    
    ## confirm - what do I want to do with dd?.... round to fewer digits?
  }  
  parameters<- (cbind(.network,parameters)) 
  return(parameters)
}


# Simulate values for the matrix (see Raymond et al 2010)
# Keep all matrices for now, with separate vector of stability

itN <- 100000 #number of iterations

# initialize matrix to hold simulation results
mat.all <- array(dim=c(dim(network),itN))
stable <- vector(length=itN)
dets <- vector(length=itN)
kappas  <- vector(length=itN)

maxval <- 1

for (k in 1:itN) {
  # simulate values based on each network cell definition
  for (i in 1:nrow(network)) {
    for (j in 1:ncol(network)) {
      if (network[i,j] == "ab") {mat.all[i,j,k] <- round(runif(1,0,maxval),digits=2)}
      else if (network[i,j] == "uk") {if (rbinom(1,1,.33) == 1) {mat.all[i,j,k] <- round(runif(1,-maxval,maxval),digits=2)} else mat.all[i,j,k] <- 0} 
      # reducing the probability of giving a value to unknown increases proportion stable
      # else if (network[i,j] == "uk") {mat.all[i,j,k] <- round(runif(1,-1,1),digits=2)} 
      else if (network[i,j] == "mu") {mat.all[i,j,k] <- round(runif(1,0,maxval),digits=2)}
      else if (network[i,j] == "pr") {mat.all[i,j,k] <- round(runif(1,-maxval,0),digits=2)}
      else if (network[i,j] == "py") {mat.all[i,j,k] <- round(runif(1,0,maxval),digits=2)}
      else if (network[i,j] == "co") {mat.all[i,j,k] <- round(runif(1,-maxval,0),digits=2)}
      else if (network[i,j] == "dd") {mat.all[i,j,k] <- -1} #round(runif(1,-maxval,-.25),digits=2)}
      else if (network[i,j] == "na") {mat.all[i,j,k] <- 0} 
      else if (network[i,j] == "in") {mat.all[i,j,k] <- round(runif(1,-maxval,0),digits=2)}
    }
  }
  # check for Lyapunov stability (this is based on Raymond)
  if (all(Re(eigen(mat.all[,,k],only.values=T)$values)<0)) {stable[k] <- TRUE}
  
  # calculate the determinant and kappa
  dets[k] <- det(mat.all[,,k])
  kappas[k] <- kappa(mat.all[,,k])
}

# how many iterations are stable?
sum(stable)

# how many are non-invertible?
x <- length((which(dets==0)))
which(dets==0) %in% stable 

# take subset of stable matrices and work with them 
mat.stable <- mat.all[,,which(stable)]
dets.stable <- dets[which(stable)]
kappas.stable <- kappas[which(stable)]

# what do the stable matrices look like?
summary(dets.stable) # really small determinants! (all negative)
summary(kappas.stable)

# invert the matrices
mat.inv <- array(dim=dim(mat.stable), dimnames=list(nodes,nodes))

# take the negative inverse of the community matrix
for (k in 1:sum(stable)) {
  mat.inv[,,k] <- -solve(mat.stable[,,k])
} # set small values to zero?