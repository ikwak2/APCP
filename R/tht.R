#' Andrews path function
#'
#' Make an Andrews path function
#'
#' @param p The dimension.
#'
#' @param t The time point.
#'
#' @export
#' @return p-vector of Andrews path function at time t
#'
#' @examples
#' # 5-vector of Andrews path at time 4
#' tht(5,4)
#' @seealso \code{\link{plotAnd}}, \code{\link{parallelAnd}}

tht <-
function(p,t)
{
    v <- 1/sqrt(2)
    l <- ceiling((p-1)/2)
    if(l!=0) for(l in 1:l)
             {
                  v[1+(2*l-1)] <- sin(l*t)
                  v[1+2*l] <- cos(l*t)
             }
    return(v)
}

