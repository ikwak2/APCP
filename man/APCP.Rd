\name{APCP}
\alias{APCP}
\title{Plot Andrews type parallel coordinate plot}
\usage{
APCP(Xrr, center = T, scale = T, tcex = 1, ylim, ...)
}
\arguments{
  \item{Xrr}{The list object from Reorder function.}

  \item{center}{Make the data centered if center = TRUE}

  \item{scale}{Do scaling the data set if scale = TRUE}

  \item{ylim}{usuall ylim of the plot function. The limit
  of y axis.}

  \item{tcex}{The colname size. They will be texted at the
  plot.}

  \item{...}{other plotting options}
}
\value{
Andrews type parallel coordinate plot
}
\description{
Plot Andrews type parallel coordinate plot. It considers
categorical variables as well.
}
\examples{
library(MASS)
data(Cars93)
X<-Cars93[,c(3,5,7,10,25,26)]  # Type, Price, MPG.city, Drivertrain, Weight, Origin
XX<-nAlg(X)
colo<-c("red","blue","green","000022","yellow","brown")[unclass(X[,6])]
Xrr <- Reorder(XX, jitter=T, amount=.1)
APCP(Xrr,col=colo, ylim=c(-3,5),xlab="t",ylab="f(t)", main="Cars93 : Shortest Path, PCP version of Andrews' plot" )
}
\seealso{
\code{\link{plotAnd}}, \code{\link{parallelAnd}}
}

