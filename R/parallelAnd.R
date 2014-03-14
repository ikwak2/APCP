parallelAnd <-
function(K, center=T ,scale=T,color="black", ylim,tcex=1,...)
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

    plotAnd(Y,ylim=ylim,color=color,...)
      for (i in ifelse(k==0,1,2):p)
             {
                 abline(v=t[i], lty="dotted")
                 text(t[i],ylim[1],names1[i-k],cex=tcex)
             }
}

