\name{tht}
\alias{tht}
\title{Andrews path function}
\usage{
tht(p, t)
}
\arguments{
  \item{p}{The dimension.}

  \item{t}{The time point.}
}
\value{
p-vector of Andrews path function at time t
}
\description{
Make an Andrews path function = c(1/sqrt(2), sin(t),
cos(t), ... , sin(k*t), cos(k*t))
}
\examples{
# 5-vector of Andrews path at time 4
tht(5,4)
}
\seealso{
\code{\link{plotAnd}}, \code{\link{parallelAnd}}
}

