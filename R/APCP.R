#' Plot Andrews type parallel coordinate plot
#'
#' Plot Andrews type parallel coordinate plot. It considers categorical variables as well.
#'
#' @param Xrr The list object from Reorder function.
#'
#' @param center Make the data centered if center = TRUE
#'
#' @param scale Do scaling the data set if scale = TRUE
#'
#' @param ylim usuall ylim of the plot function. The limit of y axis.
#'
#' @param color color of each individual
#'
#' @param tcex The colname size. They will be texted at the plot.
#'
#' @param ... other plotting options
#'
#' @export
#' @return Andrews type parallel coordinate plot
#'
#' @examples
#'
#' library(MASS)
#' data(Cars93)
#' X<-Cars93[,c(3,5,7,10,25,26)]  # Type, Price, MPG.city, Drivertrain, Weight, Origin
#' XX<-nAlg(X)
#' colo<-c("red","blue","green","000022","yellow","brown")[unclass(X[,6])]
#' Xrr <- Reorder(XX, jitter=T, amount=.1)
#' APCP(Xrr,col=colo, ylim=c(-3,5),xlab="t",ylab="f(t)", main="Cars93 : Shortest Path, PCP version of Andrews' plot" )
#'
#' @seealso \code{\link{plotAnd}}, \code{\link{parallelAnd}}

APCP <- function(Xrr,center=T ,scale=T, tcex=1 ,ylim, color, ...)
{
    K<-Xrr$Dat
    Facnames<-Xrr$Facnames;

    p<-ncol(K); n<-nrow(K); k<-1;

    if(missing(color))
        color = rep("black",n)

    if(p/2 == round(p/2))
    {
        XX <- matrix(0,n,(p+1))
        XX[,2:(p+1)] <- as.matrix(K[,1:p])
        X <- scale(XX, center=center, scale=scale)
        X[is.na(X)]<-0
        p <- ncol(X)
    }
    else
    {
        X <- scale(K, center=center, scale=scale)
        k <- 0
    }

    t<-seq(0,2*pi, 2*pi/p)[1:p]
    V <- matrix(0,p,p) ; for(i in 1:p) V[,i] <- tht(p,t[i])
    names1<-colnames(K)
    Y<-as.matrix(X)%*%t(V)/(p/2)

    plotAnd(Y,ylim=ylim, color = color, ...)

    for (i in ifelse(k==0,1,2):p)
    {
        abline(v=t[i], lty="dotted")
        text(t[i],ylim[1],names1[i-k],cex=tcex)
    }

    colo2<-rep(c("red","blue","green","000022","yellow","brown","003319","black"),3)
    for(i in Xrr$rsel)
    {
        bb<-Facnames[[i]]
        for (j in 1: length(bb))
        {
            text(t[i+k], bb[j], rownames(bb)[j], col=colo2[j], cex=1.5)
        }
    }
}
