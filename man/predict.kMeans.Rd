\name{predict.kMeans}
\alias{predict.kMeans}
\title{
Predict Method for K-Means Clustering
}
\description{
This function assigns observations in the data matrix \code{newData} the most likeliest clusters using the best solution from a \code{kMeans} object.
}
\usage{
predict.kMeans(X, newData)
}
\arguments{
  \item{X}{
object of class \code{kMeans}.
}
  \item{newData}{
a data matrix or data frame having the same columns as the original \code{X} when \code{kMeans} was called.
}
}
\value{
Returns a vector of cluster assignments for \code{newData} based on the \code{kMeans} object.
}

\seealso{
\code{\link{kMeans}}
}
\examples{
set.seed(63555)
exampleData <- matrix(nrow=90, ncol=1)
exampleData[1:30, 1] <- rnorm(30, mean=3, sd=1)
exampleData[31:60, 1] <- rnorm(30, mean=6, sd=1)
exampleData[61:90, 1] <- rnorm(30, mean=9, sd=1)
kMeansResult <- kMeans(exampleData, k=3)

fitted(kMeansResult)
# [1] 2 1 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1
# [47] 1 1 3 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

newData <- matrix(nro=30, ncol=1)
newData[1:30, 1] <- rnorm(30, mean=6, sd=1)
predict(kMeansResult, newData)
# [1] 1 1 1 1 1 1 2 1 1 1 1 1 1 1 2 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1

}
\keyword{ cluster }