library(praznik)
library(mvtnorm)
library(rmi)
library(ggplot2)

makeCor<-function(cor,n=100){
 rmvnorm(n,sigma=matrix(c(1,cor,cor,1),2))->x
 list(x=x[,1],y=x[,2])
}

cor2i<-function(r) -.5*log(1-r*r)

tau2mi<-function(tau)
 ifelse(
  abs(tau)!=1,
  tau*log(sqrt((1+tau)/(1-tau)))+log(sqrt(1-tau^2)),
  log(2)
 )

rho2mi<-function(rho)
 -log(sqrt(1-rho^2))

doOne<-function(cor=0.5,n=15,seed=round(17+1000*cor+n),tries=10){
 #Simulations for interesting values of n
 message("Calculating for cor=",cor," n=",n)
 cor->cor
 n->n
 seed->seed
 tries->tries

 set.seed(seed)
 lapply(1:tries,function(._) makeCor(cor,n))->xs

 res<-list()
 sapply(xs,function(x) tau2mi(cor(x$x,x$y,method="kendall")))->res$`Kendall trans.`
 sapply(xs,function(x) rho2mi(cor(x$x,x$y)))->res$`Pearson cor.`
 sapply(xs,function(x) miScores(data.frame(x=cut(x$x,3)),cut(x$y,3)))->res$`Three bins`
 sapply(xs,function(x) miScores(data.frame(x=cut(x$x,5)),cut(x$y,5)))->res$`Five bins`
 sapply(xs,function(x)  knn_mi(cbind(x$x,x$y),c(1,1),options=list(method="KSG2",k=5)))->res$`KSG`
 if(n>10) sapply(xs,function(x)  knn_mi(cbind(x$x,x$y),c(1,1),options=list(method="LNC",k=5,alpha=0.65)))->res$`LNC`

 data.frame(
  Method=names(res),
  mn2=sapply(res,quantile,.05),
  mn=sapply(res,quantile,.25),
  MI=sapply(res,median),
  mx=sapply(res,quantile,.75),
  mx2=sapply(res,quantile,.95)
 )->ans
 ans$Correlation<-cor
 ans$Samples<-n
 ans$seed<-seed
 ans$tries<-tries

 ans
}

doTheoryOne<-function(cor=0.5){
 res<-list()
 
 if(cor>0){
  #For these methods, there is no analytical solution for bivariate
  # normal distribution, hence we use simulation with N=5000
  set.seed(1)
  makeCor(cor,5000)->s
  cor(s$x,s$y,method="kendall")->tau
  tau2mi(tau)->res$`Kendall trans.`
  miScores(data.frame(x=cut(s$x,3)),cut(s$y,3))->res$`Three bins`
  miScores(data.frame(x=cut(s$x,5)),cut(s$y,5))->res$`Five bins`
 }else{
  #Independence
  0->res$`Kendall trans.`
  0->res$`Three bins`
  0->res$`Five bins`
 }

 #These methods shall yield differential entropy solution
 rho2mi(cor)->res$`Pearson cor.`
 rho2mi(cor)->res$`KSG`
 rho2mi(cor)->res$`LNC`

 data.frame(
  Method=names(res),
  MI=sapply(res,head,1),
  Correlation=cor
 )
 
}

logsc<-function(from,to,len=10) round(exp(seq(log(from),log(to),len=len)))

doAll<-function(cor=c(0,.5,.9,.99),n=logsc(10,200,len=40),tries=100)
 do.call(rbind,mapply(doOne,cor=rep(cor,each=length(n)),n=rep(n,times=length(cor)),tries=tries,SIMPLIFY=FALSE))

doTheoryAll<-function(Q)
 do.call(rbind,lapply(unique(Q$Correlation),doTheoryOne))


plotAll<-function(Q,Qt){
 Q$Method<-factor(Q$Method,levels=c("Three bins","Five bins","Kendall trans.","Pearson cor.","KSG","LNC"))
 Q$Correlation<-sprintf("r=%s",Q$Correlation)
 Qt$Method<-factor(Qt$Method,levels=c("Three bins","Five bins","Kendall trans.","Pearson cor.","KSG","LNC"))
 Qt$Correlation<-sprintf("r=%s",Qt$Correlation)
 
 ggplot(Q,aes(x=Samples,y=MI))+
  geom_ribbon(col="black",fill="gray90",size=0.1,aes(ymin=mn2,ymax=mx2))+
  geom_ribbon(col="black",size=0.1,fill="gray50",aes(ymin=mn,ymax=mx))+
  geom_hline(aes(yintercept=MI),col="red",data=Qt,size=0.2)+
  geom_line()+
  ylab("Mutual information [nats]")+
  facet_grid(Method~Correlation,scales="free")+scale_x_log10()
}

makeFigure<-function(){
 doAll()->Q
 doTheoryAll(Q)->Qt
 ggsave("fig-biv.pdf",width=8,height=7,plot=plotAll(Q,Qt))
}

if(!interactive()) makeFigure()
