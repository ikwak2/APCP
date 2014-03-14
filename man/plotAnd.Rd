\name{plotAnd}
\alias{plotAnd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Andrews plot
}
\description{
  Make an Andrews plot
}
\usage{
plotAnd(X, color = "black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
    The data set.
}
  \item{color}{
    color vector for each observation(row).
}
  \item{\dots}{
    other options that can be used in plot function.
}
}
\value{
  An Andrews plot of the data set.
}
\references{
  Kwak and Huh(2008) Andrews Plots for Extended Uses, The Korean Communications in Statistics.
}
\author{
  Il-Youp Kwak.
}

\seealso{
\code{\link{parallelAnd}}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphics }
