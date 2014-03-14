dumdum2 <- function(X,sel)
{
    p<-ncol(X); n<-nrow(X); ZZ<- NULL;
    for(i in sel)
    {
        tt <- as.matrix(table(as.matrix(X[,i])))
        Z <- matrix(0,n,k<-length(tt))
        for(j in 1:k) Z[,j] <- X[,i]==rownames(tt)[j]
        colnames(Z) <- rownames(tt)
        ZZ[i]<-list(Z)
    }
    return(ZZ)
}
