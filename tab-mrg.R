library(praznik)
library(parallel)
readRDS('../data/morphine.RDS')->A

decalibrate<-function(x)
 x*3

test<-function(X,seed,f,doBaseline=TRUE){
 if(!missing(seed)) set.seed(seed) else seed<-NULL

 #Split the set in, about, half
 (1:nrow(X))%in%sample(nrow(X),nrow(X)/2)->m
 X[!m,]->X1
 X[m,]->X2

 #Introduce a monotonic transformation
 !sapply(X2,is.factor)->alter
 for(e in which(alter)) X2[,e]<-decalibrate(X2[,e])

 rbind(kTransform(X1),kTransform(X2))->XkM
 kTransform(rbind(X1,X2))->XkT
 kTransform(X)->Xk

 list(
  baseline=if(doBaseline) f(Xk) else NULL,
  naive=f(XkT),
  merged=f(XkM),
  seed=seed
 )
}

assess<-function(X,Y,case="Unk"){
 X$Y<-Y
 seeds<-1:100
 mclapply(seeds,function(seed) 
  test(
   X,
   seed=seed,
   f=function(x) miScores(x[,-ncol(x)],x$Y,threads=1),
   doBaseline=seed==1
  ),
  mc.cores=parallel::detectCores()
 )->W
 sapply(W,'[[','naive')->naive
 sapply(W,'[[','merged')->merged
 W[[1]]$baseline->baseline
 reduce<-function(x) cor(x,baseline,method="spearman")[,1]
 data.frame(
  Seed=seeds,
  Correlation=c(reduce(merged),reduce(naive)),
  Merged=rep(c("Transformed merge","Naive merge"),each=length(seeds)),
  Case=case
 )
}

assessMorphine<-function()
 rbind(
  assess(A[,1:90],A$yUSV,"USV"),
  assess(A[,1:90],A$yMorph,"Morphine"),
  assess(A[,1:90],A$yWithdrawal,"Withdrawal")
 )

doAll<-function(){
  assessMorphine()->Q
  tapply(Q$Correlation,Q[,3:4],function(x){
   x<-x*100
   sprintf(
    "%0.1f%% [%0.1f%%-%0.1f%%]",
    median(x),
    quantile(x,.25),
    quantile(x,.75)
   )
  })->tab
  tab
}

if(!interactive()) write.csv(doAll()[2:1,c(2,1,3)],"tab-mrg.csv")
