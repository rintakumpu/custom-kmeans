\name{kMeans}
\alias{kMeans}
\alias{kMeansAlg}
\alias{fitted.kMeans}
\alias{print.kMeans}
\alias{summary.kMeans}
\title{
K-Means Clustering
}
\description{
Performs k-means clustering \code{m} times on a data matrix. \code{fitted} returns a vector with the class labels of the best run.
}
\usage{
kMeans(X, k, m=10, ind, max.iter=50, ...)

## S3 method for class 'kMeans'
fitted(object)
}
\arguments{
  \item{X}{
a numeric matrix of data.
}
  \item{k}{
the desired number of clusters.
}
\item{m}{
the number of times to run the clustering algorithm. The default is 10.
}
\item{ind}{
a numeric vector of columns indicating the variables used in the clustering.
}
\item{max.iter}{
the maximum number of iterations for a single run of the clustering algorithm. The default is 50.
}
 \item{\dots}{
not used.
}
}
\details{
The matrix data given by X is clustered by the standard k-means method, also known as Lloyd-Forgy method (1957 & 1965). This method aims at minimizing the within-cluster sum of squares objective and thus assigns the clusters by the smallest Euclidean distance of observation to the cluster center. 

The Random Partition method as described by Hamerly and Elkan (2002) is used for computing the initial cluster means.
}
\value{
kMeans returns an object of class \code{kMeans} which has a print, summary, predict, plot and a fitted method. It is a list with the following components:

\item{Cbest }{the vector of the best group labels.}
\item{ObjBest }{the value of the objective function for the best solution.}
\item{CentroidsBest }{the matrix containing the centroids of the best solution.}
\item{m }{the number of repetitions.}
\item{k }{the number of groups.}
\item{Xname }{name of the data set used for the clustering.}
\item{Ind }{the value of input \code{ind}.}
\item{Y }{the data used for the clustering.}
\item{Best }{value of which of the runs was the best.}
\item{Call }{a matrix with \code{n} rows and \code{m} columns giving the cluster assignments for all \code{m} runs.}
\item{ObjAll }{a vector having the objective functions of all runs.}
\item{StatusAll }{a vector having the status from all runs.}

}
\references{

Forgy, E. W. (1965) Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics \strong{21}, 768--769.

Hamerly, G.; Elkan, C. (2002) \href{http://people.csail.mit.edu/tieu/notebook/kmeans/15_p600-hamerly.pdf}{Alternatives to the k-means algorithm that find better clusterings} (PDF). Proceedings of the eleventh international conference on Information and knowledge management (CIKM).

Lloyd, S. P. (1957, 1982) Least squares quantization in PCM. Technical Note, Bell Laboratories. Published in 1982 in IEEE Transactions on Information Theory \strong{28}, 128--137.
}

\seealso{
\code{\link{kmeans}}
}

\examples{

# Example using random data from three different populations
# with one variable

set.seed(63555)
exampleData <- matrix(nrow=90, ncol=1)
exampleData[1:30, 1] <- rnorm(30, mean=3, sd=1)
exampleData[31:60, 1] <- rnorm(30, mean=6, sd=1)
exampleData[61:90, 1] <- rnorm(30, mean=9, sd=1)
kMeansResult <- kMeans(exampleData, k=3)

kMeansResult
# K-Means clustering for iris 
# Number of runs: 10 
# Status of best run: converged

fitted(kMeansResult)
# [1] 2 1 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1
# [47] 1 1 3 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

summary(kMeansResult)
#K-Means clustering for exampleData 

#Clusters to be detected: 3 
#Cluster sizes detected: 30 28 32 

#Number of runs: 10 
#Status of best run: converged 

#Criterion value: 3522.284 
#Summary of criterion values:
#Min: 3522.284 
#Q1: 3546.504 
#Mean: 3543.455 
#Q3: 3546.504 
#Max: 3546.504

}

\keyword{ cluster }
