\name{parallelAnd}
\alias{parallelAnd}
\title{Andrews type parallel coordinate plot}
\usage{
parallelAnd(K, center = T, scale = T, color = "black", ylim, tcex = 1,
  ...)
}
\arguments{
  \item{K}{The data set.}

  \item{center}{Make the data centered if center = TRUE}

  \item{scale}{Do scaling the data set if scale = TRUE}

  \item{color}{color vector for each observations.}

  \item{ylim}{usuall ylim of the plot function. The limit
  of y axis.}

  \item{tcex}{The colname size. They will be texted at the
  plot.}

  \item{...}{other options that can be used in plot
  function.}
}
\value{
An Andrews type parallel coordinate plot of the data set.
}
\description{
Make an Andrews type parallel coordinate plot
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
\code{\link{plotAnd}}
}

