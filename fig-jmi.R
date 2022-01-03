library(praznik)
library(ggplot2)

fa<-function(a,b,l) a*l+b*(1-l)
fx<-function(a,b,l) pmax(a*l,b*(1-l))

makeCase<-function(f=fa,n=20,lp=30){
 do.call(rbind,parallel::mclapply(seq(0,1,len=lp),function(lam){
  A<-kTransform(runif(n)->a)
  B<-kTransform(runif(n)->b)
  C<-kTransform(runif(n)->c)
  AB<-factor(paste(A,B))
  Y<-kTransform(f(a,b,lam))
  miScores(data.frame(A=A,B=B,AB=AB),Y,threads=1)->z
  cmiScores(data.frame(A=A,C=C),B,Y,threads=1)->z2

  data.frame(
   Score=c("I(A;Y)","I(B;Y)","I(A,B;Y)","I(A;B|Y)","I(A;C|Y)"),
   λ=lam,
   Value=c(z,z2)
  )
 },mc.cores=16))->ans
 ans$ff<-fixff(paste(deparse(body(f)),collapse=" "))
 ans$n<-n
 ans
}

fixff<-function(ff){
 ff<-gsub('\\*','',ff)
 ff<-gsub(' ','',ff)
 ff<-gsub('l','λ',ff)
 ff<-gsub('pmax','max',ff)
 ff<-gsub('^','y=',ff)
 ff
}

makeAll<-function()
 rbind(
  makeCase(n=200,f=fa,lp=201),
  makeCase(n=200,f=fx,lp=201)
 )


plotAll<-function(Q)
 ggplot(Q,aes(x=λ,y=Value,col=Score))+
  facet_wrap(~ff)+
  geom_line()+
  theme(legend.position="bottom")+
  ylab("Value [nats]")

makeFigure<-function(){
 makeAll()->Q
 ggsave("fig-jmi.pdf",device=cairo_pdf,width=8,height=5,plot=plotAll(Q))
}

if(!interactive()) makeFigure()
