# functions necessary for mgmt-run.R

######################################

# create one matrix based on network
createMatrix <- function(network) {
  
  # initialize array and make class indices
  mat <- matrix(NA,nrow(network),ncol(network)) 
  index <- makeIndex(network)
  
  # fill in mat with simulated parameter values
  mat[index$ab] <- rgamma(sum(index$ab),shape=0.5,scale=1)
  mat[index$co] <- -rgamma(sum(index$co),shape=0.5,scale=1)
  mat[index$py] <- rgamma(sum(index$py),shape=0.5,scale=1)
  mat[index$mu] <- rgamma(sum(index$mu),shape=0.5,scale=1)
  mat[index$pr] <- -rgamma(sum(index$pr),shape=0.5,scale=1)
  mat[index$'in'] <- -rgamma(sum(index$'in'),shape=0.5,scale=1)
  mat[index$uk] <- unknown.links(sum(index$uk))
  mat[index$na] <- rep(0,sum(index$na))
  mat[index$dd] <- -rgamma(sum(index$dd),shape=0.5,scale=1)    
  
  stopifnot(all(!is.na(mat)))
  
  return(mat)
}

###################################

# Function to determine unknown links
unknown.links <- function(x,n=1) {
  n <- n
  x <- x
  param.vector <- vector(length=0)
  sign <- 0.5 # what proportion positive 
  
  for (i in 1:n) {
    p <- runif(1,0,1) # how many will be zero
    p.vector <- rbinom(x,1,p)
    sign.vector <- rbinom(x,1,sign)
    sign.vector[which(sign.vector==0)] <- -1
    
    param.vector[length(param.vector)+seq(1,x,1)] <- rgamma(x,shape=0.5,scale=1)*p.vector*sign.vector
  }
  return(param.vector)
}

###################################

# check for Lyapunov stability 
filterMatrix <- function(matrix) {
  ifelse(
      all(Re(eigen(matrix,only.values=T)$values)<0),
      stable <- TRUE,
      stable <- FALSE
  )
  return(stable)
}

###################################

# find inverse of the matrix (negative or positive)
invertMatrix <- function(matrix,neg=FALSE) {
  ifelse(
    neg==TRUE,
    inverted <- -solve(matrix),
    inverted <- solve(matrix)
    )
  ind <- which(abs(inverted) <= 1e-6)
  inverted[ind] <- 0
  return(inverted)
}

##################################

# make indices for classes from the network

makeIndex <- function(network) {
  classes <- unique(as.character(network))
  index <- list()
  for (i in 1:length(classes)) {
    index[[classes[i]]] <- network==classes[i]
  }
  return(index)
}


##################################

# buildFence <- function(prop.conserved,fence.efficacy,matrix,network) {
#   index <- makeIndex(network)
#   nodes <- rownames(network)
#   mat <- matrix
#   pw <- prop.conserved
#   pd <- 1-pw
#   f <- fence.efficacy # assumes same efficacy for all nodes
#   
#   #
#   
#   # increases density 
#   
#   
# }
