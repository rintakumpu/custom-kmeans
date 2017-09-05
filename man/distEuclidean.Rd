\name{distEuclidean}
\alias{distEuclidean}
\title{
Euclidean Distance
}
\description{
Calculates the Euclidean distance between two matrices with \code{k} columns.
}
\usage{
distEuclidean(x, y)
}
\arguments{
  \item{x}{
a matrix of size \code{n} x \code{k}.
}
  \item{y}{
a matrix of size \code{m} x \code{k}.
}
}
\value{
a distance matrix of size \code{n} x \code{m}.
}

\seealso{
\code{\link{dist}}
}
\examples{

## The function is currently defined as
function (x, y) 
{
    z <- matrix(0, nrow = nrow(x), ncol = nrow(y))
    for (k in 1:nrow(y)) {
        z[, k] <- sqrt(colSums((t(x) - y[k, ])^2))
    }
    return(z)
  }
}

\keyword{ ~array }