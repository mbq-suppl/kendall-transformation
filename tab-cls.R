library(praznik)
library(randomForest)
library(parallel)

auroc<-function(score,cls)
 mean(rank(score)[cls]-1:sum(cls))/sum(!cls)

readRDS('../data/morphine.RDS')->Q

#Which variables are predictors?
preds<-!grepl("^y",names(Q))

splitInTwo<-function(x,y){
 #Returns mask, where TRUE is for training
 (1:nrow(x))%in%sample(nrow(x),nrow(x),replace=TRUE)->m
}

kt_rforest_prob<-function(X,Y,Xp,...)
 predict(randomForest(X,droplevels(Y),...),Xp,type="prob")[,">"]

rg_rforest<-function(X,Y,Xp,...) 
 if(!is.factor(Y))
  predict(randomForest(X,Y,...),Xp) else 
  predict(randomForest(X,Y,...),Xp,type="prob")[,levels(Y)[2]]

coerce_num_or_bin<-function(y){
 if(is.numeric(y)) return(y)
 if(is.factor(y)) return(y==levels(y)[1])
 stop("Unsupported")
}

ttkt<-function(x,y,model,...){
 splitInTwo(x,y)->m

 coerce_num_or_bin(y)->y

 model(
  kTransform(x[m,]),
  kTransform(y[m]),
  kTransform(x[!m,]),
  ...)->ans

 kInverse(ans)->yp
 if(diff(range(rank(yp)))==0) return(0)
 if(!is.logical(y)) 
  c(cor=cor(rank(y[!m]),rank(yp)))
 else
  c(auroc=auroc(rank(yp),y[!m]))
}

tt<-function(x,y,model,...){
 splitInTwo(x,y)->m
 model(x[m,],y[m],x[!m,],...)->yp

 if(!is.factor(y))
  c(cor=cor(rank(y[!m]),rank(yp)))
 else
  c(auroc=auroc(rank(yp),y[!m]==levels(y)[2]))
}

doOne<-function(seed=17,yn="yUSV"){
 set.seed(seed)
 ttkt(Q[,preds],(Q[,yn]),kt_rforest_prob)->sco_kt

 set.seed(seed)
 tt(Q[,preds],(Q[,yn]),rg_rforest)->sco_o

 rbind(
  data.frame(seed=seed,mod="kt_rforest_prob",score=sco_kt,yn=yn),
  data.frame(seed=seed,mod="rg_rforest",score=sco_o,yn=yn)
 )
}

calculateAll<-function(){
 #if(file.exists("tab-cls-cache.RDS")) return(readRDS("tab-cls-cache.RDS"))
 yn<-c("yMorph","yUSV","yWithdrawal")
 do.call(rbind,mclapply(1:100,
  function(seed) do.call(rbind,lapply(yn,function(yn) doOne(seed,yn))),
 mc.cores=parallel::detectCores()))->ans
 #saveRDS(ans,file="tab-cls-cache.RDS",compress="xz")
 ans
}

prcMedIqr<-function(x){
 try({
  x<-x*100
  sprintf(
   "%0.1f%% [%0.1f%%-%0.1f%%]",
   median(x),
   quantile(x,.25),
   quantile(x,.75)
  )
 },silent=TRUE)->ans
 if(inherits(ans,"try-error")) {
  print(x)
  return('-')
 }
 ans
}

tab<-function(Z){
 tapply(Z$score,Z[,c("mod","yn")],prcMedIqr)->tab
 data.frame(tab)->tab
 c(
  yMorph="Morphine [AUROC]",
  yUSV="USV [Spearman cc.]",
  yWithdrawal="Withdrawal [AUROC]"
 )[names(tab)]->names(tab)
 c(
  kt_rforest_prob="Transformed data",
  rg_rforest="Original data"
 )[rownames(tab)]->rownames(tab)
 tab[,c(2,1,3)]->tab
 tab
}

doAll<-function() tab(calculateAll())

if(!interactive()) write.csv(doAll(),"tab-cls.csv")
