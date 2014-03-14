#' Andrews type parallel coordinate plot
#'
#' Make an Andrews type parallel coordinate plot
#'
#' @param K The data set.
#'
#' @param center Make the data centered if center = TRUE
#'
#' @param scale Do scaling the data set if scale = TRUE
#'
#' @param ylim usuall ylim of the plot function. The limit of y axis.
#'
#' @param tcex The colname size. They will be texted at the plot.
#'
#' @param ... other options that can be used in plot function.
#'
#'
#' @export
#' @return An Andrews type parallel coordinate plot of the data set.
#'
#' @examples
#' # example APCP of three observations of 5 dimension.
#' X <- rbind( c(1,2,1,2,3),
#'            c(2,2,2,2,2),
#'            c(0,1,2,1,0) )
#'colnames(X) <- c("A","B","C","D","E")
#'
#' # An Andrews plot of the data set X.
#' plotAnd(X, ylim=c(-5,7))
#'
#' # An Andrws type parallel coordinate plot(APCP) of the data set X.
#' parallelAnd(X, center=F, scale=F, ylim=c(-1,5) )
#'
#' # Andrews plot of iris data set.
#' data(iris)
#' coco <- c("red","blue","green")[unclass(iris[,5])]
#' plotAnd(iris[,1:4],ylim=c(-3,15),col=coco)
#'
#' # APCP of iris data set.
#' parallelAnd(iris[,1:4],center=F,scale=F,ylim=c(-3,15),col=coco)
#'
#'
#' @seealso \code{\link{plotAnd}}


parallelAnd <-
function(K, center=T ,scale=T, ylim,tcex=1,...)
{
    p<-ncol(K); n<-nrow(K); k<-1;
    if(p/2 == round(p/2))
    {
        XX <- matrix(0,n,(p+1))
        XX[,2:(p+1)] <- as.matrix(K[,1:p])
        X <- scale(XX, center=center, scale=scale)
        X[is.na(X)]<-0
        p <- ncol(X)
    }
    else  # 변수개수 홀수
    {
        X <- scale(K, center=center, scale=scale)
        k <- 0
    }

    t<-seq(0,2*pi, 2*pi/p)[1:p]
    V <- matrix(0,p,p) ; for(i in 1:p) V[,i] <- tht(p,t[i])
    names1<-colnames(K)
    Y<-as.matrix(X)%*%t(V)/(p/2)

    plotAnd(Y,ylim=ylim,...)
      for (i in ifelse(k==0,1,2):p)
             {
                 abline(v=t[i], lty="dotted")
                 text(t[i],ylim[1],names1[i-k],cex=tcex)
             }
}

