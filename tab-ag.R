library(praznik)
library(randomForest)
library(pspearman)
library(reshape2)

jcc<-function(p,y) sum(y&p)/sum(y|p)
jccs<-function(score,y)
 sapply(sort(score,dec=TRUE),function(s) jcc(score>=s,y))

readRDS('morphine.RDS')->Q
X<-Q[,1:90]
Y<-Q[,91:93]

statSelector<-function(X,Y,pv=0.05){
 pp<-if(is.factor(Y)||is.logical(Y)){
  Y<-factor(Y)
  stopifnot(length(levels(Y))==2)
  y<-Y==levels(Y)[1]
  sapply(X,function(x) suppressWarnings(wilcox.test(x[y],x[!y]))$p.value)
 }else{
  stopifnot(is.numeric(Y))
  sapply(X,function(x) suppressWarnings(spearman.test(x,Y))$p.value)
 }
 pp[is.na(pp)]<-1
 setNames(p.adjust(pp)<pv,names(X))
}

makeAns<-function(){
 ans<-list()

 setNames(lapply(names(Y),
  function(nY) statSelector(X,Y[[nY]])),
  names(Y))->ans$sd
 setNames(lapply(names(Y),
  function(nY){set.seed(7);importance(randomForest(X,Y[[nY]],importance=TRUE),1)[,1]}),
  names(Y))->ans$rfi

 for(bins in c(3,5)){
  for(wr in c(TRUE,FALSE)){
   rr<-if(wr) rank else function(x) x
   data.frame(apply(X,2,function(x) cut(rr(x),bins),simplify=FALSE))->Xc
   Yc<-Y
   Yc$yUSV<-cut(rr(Yc$yUSV),bins)

   setNames(lapply(names(Y),
    function(nY) miScores(Xc,Yc[[nY]])),
    names(Y))->ans[[sprintf("mi%s%scut",ifelse(wr,'r',''),bins)]]
  }
 }

 setNames(lapply(names(Y),
  function(nY) miScores(kTransform(X),kTransform(Y[[nY]]))),
  names(Y))->ans$mikt

 flopAns<-setNames(lapply(names(ans[[1]]),function(n) as.data.frame(lapply(ans,'[[',n))),names(ans[[1]]))
 flopAns
}

makeStats<-function(Ans){
 ans<-list()
 lapply(Ans,function(Z) sapply(Z[,-1],jccs,Z[,1]))->ans$j_stat
 ans
}

flattenStats<-function(S){
 do.call(rbind,lapply(c("j_stat"),function(Sn){
  SS<-S[[Sn]]
  ans<-do.call(rbind,lapply(names(SS),function(SSn){
   data.frame(Method=colnames(SS[[SSn]]),Agreement=apply(SS[[SSn]],2,max))->ans
   ans$Decision<-SSn
   ans
  }))
  ans$Baseline<-"Non-parametric"
  ans
 }))->ans
 ans$Decision<-factor(
  as.character(ans$Decision),
  levels=c("yUSV","yMorph","yWithdrawal"),
  labels=c("USV","Morphine","Withdrawal")
 )
 ans$Method<-ans$Score<-factor(
  as.character(ans$Method),
  levels=c("mikt","mi3cut","mi5cut","mir3cut","mir5cut","rfi"),
  labels=c(
   "Kendall transformation",
   "Three equal-width bins",
   "Five equal-width bins",
   "Three equal-frequency bins",
   "Five equal-frequency bins",
   "Random Forest importance"
  )
 )
 ans
}

tabStats<-function(S){
 dcast(flattenStats(S),Score~Decision,value.var="Agreement")->A
 rownames(A)<-A$Score
 A$Score<-NULL
 A
}


doAll<-function()
 tabStats(makeStats(makeAns()))

if(!interactive()) write.csv(doAll(),"tab-ag.csv")
