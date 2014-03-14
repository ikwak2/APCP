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

