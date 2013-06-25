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
invertMatrix <- function(matrix,neg=TRUE) {
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
# create template to fill for each management strategy
pressTemplate <- function(network) {
  press.template <- data.frame(matrix(nrow=nrow(network),ncol=15))
  rownames(press.template) <- rownames(network)
  names(press.template) <- c("node","numsims","increase","decrease","nochange","pq0","pq25","pq50","pq75","pq100","nq0","nq25","nq50","nq75","nq100")
  press.template$node <- c("Water","Grass","Trees","Crops","Domestic Ruminants","Domestic Equids","Wild Ruminants","Wild Equids","Parasites of Dom. Equids","Parasites of Wild Equids","Parasites of Dom. Ruminants","Parasites of Wild Ruminants","Infectious Equid Parasites","Infectious Ruminant Parasites","Carnivores","Elephants","People")
  return(press.template)
}

##################################
# create data.frame with results of a particular press perturbation
calcPress <- function(pressednodes,sign,mat.inverses,network) {
  ms <- mat.inverses
  # if decrease, change sign
  ifelse(sign=="decrease",
    ms <- mat.inverses*(-1),
    ms <- mat.inverses
  )
  # subset columns of press perturbed
  n <- dim(ms)[3]
  cols <- match(pressednodes,colnames(network))
  ms <- ms[,cols,]
  # sum effect if multiple pressed
  if(length(dim(ms))==3) {
    ms <- apply(ms,c(1,3),sum)
  }
  # make indices for positive, negative, zero
  ind <- makeInd(ms)
  # summarize results
  result <- pressTemplate(network)
  result$numsims <- n
  result$increase <- apply(ind$p,1,sum)/n
  result$decrease <- apply(ind$n,1,sum)/n
  result$nochange <- apply(ind$z,1,sum)/n
  result[,c("pq0","pq25","pq50","pq75","pq100")] <- t(apply(
    ms, 1, function(x){
    quantile(x[x>0])
  }))
  result[,c("nq0","nq25","nq50","nq75","nq100")] <- t(apply(
    ms, 1, function(x){
      quantile(x[x<0])
    }))
  return(result)
}

####################################

# make index of positive, negative, zero
makeInd <- function(x) {
  ind <- list()
  ind$p <- matrix(FALSE,nrow=nrow(x),ncol=ncol(x))
  ind$n <- ind$p
  ind$z <- ind$p
  
  ind$p[which(x>0)] <- TRUE
  ind$n[which(x<0)] <- TRUE
  ind$z[which(x==0)] <- TRUE
  return(ind)
}

###################################

buildFence <- function(prop.conserved,fence.efficacy,matrix,network) {
  nodes <- rownames(network)
  mat <- matrix
  pw <- prop.conserved
  pd <- 1-pw
  f <- fence.efficacy # assumes same efficacy for all nodes
  w.nodes <- c("wild_rum","wild_eq","carnivores","elephants")
  d.nodes <- c("dom_rum","dom_eq","people","crops")
  
  # reduce interactions between w.nodes and d.nodes by f
  for (i in 1:length(nodes)) { # rows
    for (j in 1:length(nodes)) { # columns
        if (nodes[i] %in% w.nodes & nodes[j] %in% d.nodes) {
          mat[i,j] <- mat[i,j]*(1-f)
          mat[j,i] <- mat[i,j]*(1-f)
        }
      }
    }
  }
  
  # scale interactions with infectious parasites by f and pw,pd?

  # increases density - would building fence increase density? 
  return(mat)
}
