\name{parallelAnd}
\alias{parallelAnd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Andrews type parallel coordinate plot
}
\description{
Make an Andrews type parallel coordinate plot
}
\usage{
parallelAnd(K, center = T, scale = T, color = "black", ylim, tcex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{
    Data set.
}
  \item{center}{
    Make the data centered if center = TRUE.
}
  \item{scale}{
    Do scaling the data set if scale = TRUE.
}
  \item{color}{
    A color vector for each observations.
}
  \item{ylim}{
    usuall ylim of the plot function. The limit of y axis.
}
  \item{tcex}{
    The colname size. They will be texted at the plot.
}
  \item{\dots}{
    You can use other graphical options that are used in plot function.
}
}

\value{
  An Andrews type parallel coordinate plot
}
\references{
Kwak and Huh(2008) Andrews Plots for Extended Uses, The Korean Communications in Statistics.
}
\author{
Il-Youp Kwak
}

\seealso{
 \code{\link{plotAnd}}
}
\examples{

# example APCP of three observations of 5 dimension.
X <- rbind( c(1,2,1,2,3),
            c(2,2,2,2,2),
            c(0,1,2,1,0) )
colnames(X) <- c("A","B","C","D","E")
parallelAnd(X, center=F, scale=F, ylim=c(-1,5) )

# Andrews plot of iris data set.
data(iris)
coco <- c("red","blue","green")[unclass(iris[,5])]
plotAnd(iris[,1:4],ylim=c(-3,15),col=coco)

# APCP of iris data set.
parallelAnd(iris[,1:4],center=F,scale=F,ylim=c(-3,15),col=coco)


}

\keyword{ graphics }
