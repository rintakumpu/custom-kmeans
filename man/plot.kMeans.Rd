\name{plot.kMeans}
\alias{plot.kMeans}
\title{
Plot Method for K-Means Clustering
}
\description{
Plot method for objects of class "\code{kMeans}". Calls \code{\link{plot}} function to produce a scatter plot of the data values and cluster assignments provided by the given \code{kMeans} object.
}
\usage{
plot.kMeans(X, col, pch, ...)
}
\arguments{
  \item{X}{
an object of the class \code{kMeans}.
}
  \item{col}{
a specification for the plotting color.
}
  \item{pch}{
either an integer specifying a symbol or a single character to be used as the default in plotting points. See \code{\link{points}} for possible values and their interpretation. 
}
  \item{\dots}{
further arguments to be passed to or from methods.
}
}

\seealso{
\code{\link{par}}
}

\examples{
set.seed(63555)
exampleData <- matrix(nrow=90, ncol=1)
exampleData[1:30, 1] <- rnorm(30, mean=3, sd=1)
exampleData[31:60, 1] <- rnorm(30, mean=6, sd=1)
exampleData[61:90, 1] <- rnorm(30, mean=9, sd=1)
kMeansResult <- kMeans(exampleData, k=3)
plot(kMeansResult)
}

\keyword{ hplot }
\keyword{ cluster }