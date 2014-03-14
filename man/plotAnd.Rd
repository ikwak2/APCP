\name{plotAnd}
\alias{plotAnd}
\title{Andrews plot}
\usage{
plotAnd(X, ...)
}
\arguments{
  \item{X}{The data set.}

  \item{...}{other options that can be used in plot
  function.}
}
\value{
An Andrews plot of the data set.
}
\description{
Make an Andrews plot
}
\examples{
# example APCP of three observations of 5 dimension.
X <- rbind( c(1,2,1,2,3),
           c(2,2,2,2,2),
           c(0,1,2,1,0) )
colnames(X) <- c("A","B","C","D","E")

# An Andrews plot of the data set X.
plotAnd(X, ylim=c(-5,7))

# An Andrws type parallel coordinate plot(APCP) of the data set X.
parallelAnd(X, center=F, scale=F, ylim=c(-1,5) )

# Andrews plot of iris data set.
data(iris)
coco <- c("red","blue","green")[unclass(iris[,5])]
plotAnd(iris[,1:4],ylim=c(-3,15),col=coco)

# APCP of iris data set.
parallelAnd(iris[,1:4],center=F,scale=F,ylim=c(-3,15),col=coco)
}
\seealso{
\code{\link{parallelAnd}}
}

