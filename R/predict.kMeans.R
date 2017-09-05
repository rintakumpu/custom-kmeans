predict.kMeans <-
function(X, newData) {
  
  # Euclidean distance of new observations to the kMeans object best cluster centers
  Distances <- distEuclidean(newData, X$CentroidsBest)
  
  # Compute for each observation the Euclidean distance to all of the k 
  # centroids and assign it to the group with the closest centroid.
  Clusters <- apply(Distances, 1, which.min)
  
  return(Clusters)
  
}
