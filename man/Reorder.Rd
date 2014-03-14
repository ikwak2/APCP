\name{Reorder}
\alias{Reorder}
\title{variable re-ordering using endlink algorithm}
\usage{
Reorder(XX, jitter = T, ...)
}
\arguments{
  \item{X}{The data set.}

  \item{sel}{column numbers for categorical variables}
}
\value{
reordered scaled variables, repositioned factor values.
}
\description{
variable re-ordering using endlink algorithm
}
\examples{
library(MASS)
data(Cars93)
X<-Cars93[,c(3,5,7,10,25,26)]  # Type, Price, MPG.city, Drivertrain, Weight, Origin
XX<-nAlg(X)
colo<-c("red","blue","green","000022","yellow","brown")[unclass(X[,6])]
}
\seealso{
\code{\link{plotAnd}}, \code{\link{parallelAnd}}
}

