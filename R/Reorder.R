#' variable re-ordering using endlink algorithm
#'
#' variable re-ordering using endlink algorithm
#'
#' @param X The data set.
#'
#' @param sel column numbers for categorical variables
#'
#' @export
#' @return reordered scaled variables, repositioned factor values.
#'
#' @examples
#'
#' library(MASS)
#' data(Cars93)
#' X<-Cars93[,c(3,5,7,10,25,26)]  # Type, Price, MPG.city, Drivertrain, Weight, Origin
#' XX<-nAlg(X)
#' colo<-c("red","blue","green","000022","yellow","brown")[unclass(X[,6])]
#'
#'
#' @seealso \code{\link{plotAnd}}, \code{\link{parallelAnd}}



Reorder<-function(XX,jitter=T, ...)
{

    p <- ncol(X)
    sel <- NULL;
    unsel <- NULL;
    for (i in 1:p) {
        if(class(X[[i]]) == "factor"  )
            sel <- c(sel, i)
        else
            unsel <- c(unsel, i)
    }


    RElist<-NULL;
    Ord<-XX$Ord
    RElist$Dat <- XX$Dat[,Ord]
    rsel<-NULL;

    for(i in 1:length(sel))
    {
        RElist$Facnames[[which(Ord==sel[i])]] <- XX$Facnames[[sel[i]]]
        rsel<-cbind(rsel,which(Ord==sel[i]))
    }

    if (jitter==T) for(i in rsel)  RElist$Dat[,i] <- jitter(RElist$Dat[,i], ...)

    RElist$rsel<-rsel

    return(RElist)
}
