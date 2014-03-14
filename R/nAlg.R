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



## PCP for categorical data using Endlink alg
nAlg<- function(X)
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

    for (i in unsel)
        X[,i]<-scale(X[,i])

    if(is.null(sel)) {
        cate = FALSE
    } else {
        sel2 <- sel
        cate = TRUE
    }

    n<-nrow(X)
    if (cate!=F)  ZZ<-dumdum2(X,sel)
    if (cate==F)
    {
        Corr<-cor(X);
        for(i in 1:(p-1))   for(j in (i+1):p)   Corr[j,i]<--2
        diag(Corr)<--2
        sel<-0
    }  else  Corr<-qCorr(X,sel)
    ll<-matrix(0,p,2)
    Pt<-matrix(0,2,p*p)
    pp<-1
    Facnames<-NULL;

    while(max(Corr)!=-2)
    {
        Pt[,pp]<-c(which(apply(Corr==max(Corr),1,sum)==1),which(apply(Corr==max(Corr),2,sum)==1))

        if( max((aa<-ll[Pt[1,pp],]==0)) && max((bb<-ll[Pt[2,pp],]==0)) && Pt[1,pp]!=0)         {
            if (ll[Pt[1,pp],1]!=0 && ll[Pt[2,pp],1]!=0 )
            {
                ll2<-ll;
                if(aa[1]==1) k1=1 else k1=2
                if(bb[1]==1) k2=1 else k2=2

                ll2[Pt[1,pp],k1]=Pt[2,pp]
                ll2[Pt[2,pp],k2]=Pt[1,pp]

                pos<-Pt[1,pp]
                bef<-ll2[pos,1]
                savee<-c(bef,pos)
                i=3
                while(pos!=0 && i<=(p+1))
                {
                    savee[i]<-ifelse(ll2[pos,1]==bef,ll2[pos,2],ll2[pos,1])
                    bef<-pos
                    pos<-savee[i]
                    i<-i+1
                }
                if (pos==0) ll<-ll2
            } else
            {
            if(aa[1]==1) k1=1 else k1=2
            if(bb[1]==1) k2=1 else k2=2

            ll[Pt[1,pp],k1]=Pt[2,pp]
            ll[Pt[2,pp],k2]=Pt[1,pp]
            }
        }

        if(((sum(Pt[1,pp]==sel)==1) + (sum(Pt[2,pp]==sel)==1))==1)
        {
            if(sum(Pt[1,pp]==sel)==1) num<-c(Pt[2,pp],Pt[1,pp]) else num <- c(Pt[1,pp],Pt[2,pp])

            x<-X[,num[1]]; Z<-ZZ[[num[2]]];
            lamb<-1/2*sqrt(t(x)%*%Z%*%solve(t(Z)%*%Z)%*%t(Z)%*%x)
            cc<-as.numeric(1/(2*lamb))*solve(t(Z)%*%Z)%*%t(Z)%*%x

            X[,num[2]] <- Z%*%cc*sqrt(n-1)

            pp2<-ncol(Z)
            ppp<-diag(pp2)%*%cc*sqrt(n-1)
            rownames(ppp)<-colnames(Z)
            Facnames[num[2]]<-list(ppp)

            sel<-sel[-which((sel==num[2])==1)]

            Corr<-qCorr(X,sel)
            for(j in 1:pp) Corr[Pt[1,j],Pt[2,j]]=-2
        } else if(((sum(Pt[1,pp]==sel)==1) + (sum(Pt[2,pp]==sel)==1))==2)
        {
            Z1<-ZZ[[Pt[1,pp]]]; Z2<-ZZ[[Pt[2,pp]]]
            Dr<-t(Z1)%*%Z1; Dc<-t(Z2)%*%Z2
            Fdn<-t(Z1)%*%Z2
            G <- sqrt(solve(Dr))%*%Fdn%*%sqrt(solve(Dc))
            svd.G <- svd(G)
            x <- sqrt(solve(Dr))%*%svd.G$u[,2]*sqrt(n-1)
            y <- sqrt(solve(Dc))%*%svd.G$v[,2]*sqrt(n-1)

            X[,Pt[1,pp]] <- Z1%*%x;
            X[,Pt[2,pp]] <- Z2%*%y;

            pp2<-ncol(Z1) ; pp3<-ncol(Z2)
            ppp1<-diag(pp2)%*%x; ppp2<-diag(pp3)%*%y;
            rownames(ppp1)<-colnames(Z1); rownames(ppp2)<-colnames(Z2)
            Facnames[Pt[1,pp]]<-list(ppp1); Facnames[Pt[2,pp]]<-list(ppp2);

            sel<-sel[-c(which((sel==Pt[1,pp])==1),which((sel==Pt[2,pp])==1))]

            Corr<-qCorr(X,sel)
            for(j in 1:pp) Corr[Pt[1,j],Pt[2,j]]=-2
        } else
        {
            Corr[Pt[1,pp],Pt[2,pp]]=-2
        }

        pp <- pp+1
    }

    ## Endlink algorithm
    i<-1;
    while(ll[i,2]!=0)  i <- i+1
    bef<-i
    pos<-ll[i,1]

    savee<-c(bef,pos)
    i=3
    while(ll[pos,2]!=0 && ll[pos,1]!=0)
    {
        savee[i]<-ifelse(ll[pos,1]==bef,ll[pos,2],ll[pos,1])
        bef<-pos
        pos<-savee[i]
        i<-i+1
    }

    list(Dat=X,Facnames=Facnames, Ord=savee)
}
