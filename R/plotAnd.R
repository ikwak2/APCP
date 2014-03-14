plotAnd <-
function(X,color="black",...)
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

