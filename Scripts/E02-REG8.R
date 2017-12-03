#setwd("~/ASSIGNATURES/200641 - MODELS LINEALS I LINEALS GENERALITZATS")
dd<-read.csv2("./REG8.csv")
p<-2
library(car)
library(HH)

for (reg in 1:8){
  print(reg)
  Y<-dd[dd$REG==reg,"Y"]
  X<-dd[dd$REG==reg,"X"]
  n<-length(X)
  scatterplot(X,Y,smooth=F)
  m<-lm(Y~X)
  plot(ci.plot(m))
  print(summary(m))
  oldpar <- par(mfrow=c(1,2))
   plot(X,resid(m))  #o rstudent(m) , h=c(-2,0,2)
   abline(h=0,lty=2)
   plot(X,dffits(m))
   abline(h=c(-2*sqrt(p/n),0,2*sqrt(p/n)),lty=2)
   plot(m,ask=F)
  par(oldpar)
}