qCorr<- function(X,sel)
{
    n<-nrow(X); p<-ncol(X)
    ZZ<-dumdum2(X,sel)
    Corr<-matrix(-2,p,p)

    for(i in 1:(p-1))
    {
        for(j in (i+1):p)
        {
            if(((sum(i==sel)==1) + (sum(j==sel)==1))==0)   # ∏µŒ ø¨º”«¸
            {
                Corr[i,j]=t(X[,i])%*%X[,j]/(n-1)
            }
            else if(((sum(i==sel)==1) + (sum(j==sel)==1))==1)    # 1∞≥ ø¨º”«¸, 1∞≥ π¸¡÷«¸
            {
                if(sum(i==sel)==1)
                {
                    x<-X[,j]; Z<-ZZ[[i]];
                }
                else
                {
                    x<-X[,i]; Z<-ZZ[[j]];
                }
                lamb<-1/2*sqrt(t(x)%*%Z%*%solve(t(Z)%*%Z)%*%t(Z)%*%x)
                cc<-as.numeric(1/(2*lamb))*solve(t(Z)%*%Z)%*%t(Z)%*%x
                Corr[i,j]=t(x)%*%Z%*%cc/sqrt(n-1)
            }
            else if(((sum(i==sel)==1) + (sum(j==sel)==1))==2)    # ∏µŒ π¸¡÷«¸
            {
                Z1<-ZZ[[i]]; Z2<-ZZ[[j]]
                Dr<-t(Z1)%*%Z1; Dc<-t(Z2)%*%Z2
                Fdn<-t(Z1)%*%Z2
                G <- sqrt(solve(Dr))%*%Fdn%*%sqrt(solve(Dc))
                svd.G <- svd(G)
                Corr[i,j]=svd.G$d[2]
           }
        }
    }
    return(Corr)
}
