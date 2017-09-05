kMeansAlg <-
function(X, k = 1, max.iter = 10) {
  
  # For matrix data
  n <- nrow(X) # Number of observations
  v <- ncol(X) # Number of variables
  
  # For vector data
  if (is.null(n)) {
    n <- length(X)
  }
  
  if (is.null(v)) {
    v <- 1
  }
  
  # Check that all arguments are numeric
  if (!is.numeric(k)  | !is.numeric(max.iter)) {
    stop("k and max.iter need to be numeric.") 
  }
  
  # Max.iter and k must be positive
  if(k < 1) {
    k = 1
  }
  if(max.iter < 1) {
    max.iter = 10
  }
  
  # Did algorithm converge
  Status <- "not converged"
  
  # Implement k-means clustering (as per assignment algorithm)
    
    # Step 1. Assign clusters randomly
    Clusters <- sample(1:k, n, replace=TRUE)
    
    # Create a centroid matrix
    Centroids <- matrix(ncol=k, nrow=v)
    
    # Repeat steps 2 and 3 until max.iter
    for (i in 1:max.iter) {
      
      # ClustersEnd is the clusters vector at the end of the iteration
      # if equals to the previous vector will terminate the for loop
      ClustersEnd <- Clusters
      
      # Compute from each group the group centroid mu as the mean of the group members 
      Centroids <- apply(X, 2, tapply, Clusters, mean)
      
      # Euclidean distance of observations to the center
      Distances <- distEuclidean(X, Centroids)
      
      # For each observation assign to group with closest centroid
      Clusters <- apply(Distances, 1, which.min)
      
      # Break if no more changes, set status to true
      if (all.equal(Clusters, ClustersEnd) == TRUE) {
        Status <- "converged"
        break
      }
    }
  
  Crit <- sum(diag(crossprod(Distances))) # Sum of squares of Distances
  kMeansResult <- list(Clusters=Clusters, Centroids=Centroids, Crit=Crit, Status=Status)
  return(kMeansResult)
  
}
