
dd<-read.csv2("GMD.csv")
library(car)
library(lsmeans)
library(tables)
library("RcmdrMisc")

View(dd)

```{r checking_factor_variable}
is.factor(dd$DOSI)
dd$DOSI<-as.factor(dd$DOSI)
``
tabular(DOSI~Y*((n=1)+mean+sd),dd)

with(dd, plotMeans(Y, DOSI, error.bars="conf.int",level=0.95, connect=TRUE))

mod<-lm(Y~DOSI,dd)

summary(mod)

anova(mod)
Anova(mod,ty=3)
lsm<-lsmeans(mod,~DOSI)
lsm
pairs(lsm)
cld(lsm,alpha=0.01)
plot(lsm,level=0.99)
confint(lsm,level=0.99)

plot(predict(mod),resid(mod))
abline(h=0,lty=2)

plot(rstudent(mod))
abline(h=c(-2,0,2),lty=2)

plot(mod,ask=F)

predict(mod)
resid(mod)
hatvalues(mod)
dffits(mod)

leveneTest(mod)
leveneTest(Y~DOSI,dd)
bartlett.test(Y~DOSI,dd)
