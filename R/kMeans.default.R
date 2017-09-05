kMeans.default <- function(X, k, m=10, ind, max.iter=50, ...) {

  n <- nrow(X)
  v <- ncol(X)
  Xname <- deparse(substitute(X))
  
  # Check that given indices are valid, if no indices given, use all
  if (missing(ind)) {
    ind <- seq(1:v)
  } else if (!all(ind < v)) {
    stop("ind index out of bound.") 
  }
  
  # Check that numeric arguments are valid
  if (!is.numeric(m)) {
    stop("m needs to be numeric.") 
  }
  
  # m must be positive
  if(m < 1) {
    m = 10
  }
  
  # Subset X by vector ind to get Y
  Y <- subset(X, select=ind)
  
  # Initiate variables used in the for loop
  # a matrix with m rows and n columns giving the cluster assignments for all m runs.
  # thus each row m houses a vector of class labels for that run
  Call <- matrix(nrow = m, ncol = n) 
  ObjAll <- rep(0,m)
  StatusAll <- rep(0,m)
  Cbest <- rep(0,n)
  CentroidsBest <- matrix(nrow = k, ncol=length(ind)) # Centroids is a matrix with k rows and columns equaling the number of used indices
  
  # Call kMeansAlg m times  
  for(i in 1:m) {
    
    # Save current iteration result
    iterationResult <- kMeansAlg(Y, k, max.iter)
    
    # Save cluster assigments
    Call[i,] <- iterationResult$Clusters
    
    # Save objective function results
    ObjAll[i] <- iterationResult$Crit
    
    # Save status of an iteration (converged/not converged)
    StatusAll[i] <- iterationResult$Status
    
    # Check if the run was the best this far
    # use value of the objective function to determine this
    if(!exists("ObjBest")) { # If first run use the current value
      ObjBest <- iterationResult$Crit
    } 
    
    if (iterationResult$Crit <= ObjBest) {
      # If current run better (or equal, always equal on the first run)
      # than the previous best, use that
      ObjBest <- ObjAll[i]
      Cbest <- Call[i,] # Cbest the vector of the best group labels.
      CentroidsBest <- iterationResult$Centroids
      Best <- i # Save the iteration number as the best result
    }
    
  }
  
  # Save variables to return object and assign class
  kMeansResult <- list(Cbest=Cbest, ObjBest=ObjBest, CentroidsBest=CentroidsBest, m=m, k=k, 
                       Xname=Xname, Ind=ind, Y=Y, Best=Best, Call=Call, ObjAll=ObjAll, 
                       StatusAll=StatusAll)
  class(kMeansResult) <- "kMeans"
  return(kMeansResult)
}