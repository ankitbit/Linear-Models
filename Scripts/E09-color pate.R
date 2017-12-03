dd<-read.csv2("PATE.csv")
library(car)
library(lsmeans)
library(tables)
dd$per<-as.factor(dd$per)
dd$pate<-as.factor(dd$pate)

names(dd)
head(dd)

tabular(pate~color*((n=1)+mean+sd),dd)
tabular(pate~mean*color*per,dd)

summary(mcol<-lm(color~pate+per,dd))
summary(mcol0<-lm(color~pate,dd))
Anova(mcol,ty=3)
Anova(mcol0,ty=3)
anova(mcol)
anova(mcol0)
lsmeans(mcol,~pate)
lsmeans(mcol0,~pate)
cld(lsmeans(mcol,~pate),Letters=letters,reversed=T)
cld(lsmeans(mcol0,~pate),Letters=letters,reversed=T)

cld(lsmeans(mcol,~pate),Letters=letters,reversed=T,alpha=0.01)

plot(predict(mcol),rstandard(mcol))
abline(h=c(-2,0,2),lty=2)

plot(as.vector(dd$pate),rstudent(mcol))
abline(h=c(-2,0,2),lty=2)
qqPlot(mcol)
qqPlot(mcol,simulate=F,envelope=F)

plot(mcol,ask=F)

# Opcional Matrices
ddo<-dd[order(dd$pate),]
mcol<-lm(color~pate+per,ddo,x=T)
mcol$x
lm(color~pate+per,ddo,x=T,contrasts=list(per="contr.sum",dd="contr.sum"))$x

for (v in names(dd)[c(-1,-2)]){
  print(v)
  m1<-lm(as.formula(paste0(v,"~per+pate")),data=dd)
  plot(m1,ask=F)
  m11<-lm(as.formula(paste0(v,"~pate")),data=dd)
  print(anova(m1))
  print(anova(m11))
  print(cld(lsmeans(m1,~pate)))
  print(cld(lsmeans(m11,~pate)))}

for (v in names(dd)[c(-1,-2)]){
  print(v)
  m1<-lm(as.formula(paste0(v,"~per+pate")),data=dd)
  print(anova(m1))}
