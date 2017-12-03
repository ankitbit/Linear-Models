library(car)
dades <- read.csv2("dcopa.csv")
dades$RP<-dades$PB/dades$PT
dades$LDCopa<-log(dades$DCopa)
dades$LRP<-log(dades$RP)
dades$LPT<-log(dades$PT)
dades$LHT<-log(dades$HT)
dades$LE<-log(dades$E)

summary(modAc<-lm(DCopa~I(PB/PT)+PT+HT+E,dades))
summary(modA<-lm(DCopa~RP+PT+HT+E,dades))


plot(predict(modA),resid(modA),pch=3)
abline(h=0,lty=2)

plot(modA,ask=F)


plot(rstudent(modA),pch=3)
abline(h=c(-3,-2,0,2,3),lty=2)

summary(modBc<-lm(log(DCopa)~log(PB/PT)+log(PT)+log(HT)+log(E),dades))
summary(modB<-lm(LDCopa~LRP+LPT+LHT+LE,dades))

plot(predict(modB),resid(modB),pch=3)
abline(h=0,lty=2)

plot(modB,ask=F)

plot(rstudent(modB),pch=3)
abline(h=c(-3,-2,0,2,3),lty=2)



dp0<-data.frame(PT=c(0.4,0.64),PB=c(0.6,0.9),HT=c(2.3,2.8),E=10)
dpb<-data.frame(LPT=c(0.4,0.64),LRP=log(c(0.6,0.9)/c(0.4,0.64)),LHT=log(c(2.3,2.8)),LE=log(10))

exp(predict(modB,dpb,interval="prediction",level=0.9))
exp(predict(modBc,dp0,interval="prediction",level=0.9))

predict(modAc,dp0,interval="prediction",level=0.9)

