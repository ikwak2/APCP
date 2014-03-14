#' Andrews plot
#'
#' Make an Andrews plot
#'
#' @param X The data set.
#'
#' @param ... other options that can be used in plot function.
#'
#' @export
#' @return An Andrews plot of the data set.
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
#' @seealso \code{\link{parallelAnd}}


plotAnd <-
function(X, ...)
{
    v <- 0;
    n <- nrow(X); p <- ncol(X)
    Andfunc <- function (t)
    {
        v <- v + as.numeric(1/sqrt(2)*X[i,1])
        for(j in 2:p)
        {
            if( ceiling(j/2) > j/2 )  v <- v + as.numeric(X[i,j]*cos(ceiling((j-1)/2)*t))
            else v <- v + as.numeric(X[i,j]*sin(ceiling((j-1)/2)*t))
        }
        return(v)
    }
    if (length(color)==n)
    {
        for(i in 1:n)
        {
            plot(Andfunc,0,2*pi,col=color[i],xlim=c(0,2*pi),...)
            if(i!=n) par(new=T)
        }
    }
    else
    {
        for(i in 1:n)
        {
            plot(Andfunc,0,2*pi,col=color,xlim=c(0,2*pi),...)
            if(i!=n) par(new=T)
        }
    }
}

