#options(contrasts = c("contr.helmert", "contr.treatment"))
dd<-read.csv2("GMD.csv")
library(car)
library(lsmeans)
is.factor(dd$DOSI)
dd$DOSI<-as.factor(dd$DOSI)


(mod0<-lm(Y~0+DOSI,x=T,dd))
(mod1<-lm(Y~DOSI,x=T,dd,contrasts=list(DOSI="contr.treatment")))
(mod2<-lm(Y~DOSI,x=T,dd,contrasts=list(DOSI="contr.sum")))
(mod3<-lm(Y~DOSI,x=T,dd,contrasts=list(DOSI="contr.helmert")))
(mod4<-lm(Y~DOSI,x=T,dd,contrasts=list(DOSI="contr.SAS")))

(lsmeans(mod0,~DOSI))
(lsmeans(mod1,~DOSI))
(lsmeans(mod2,~DOSI))
(lsmeans(mod3,~DOSI))
(lsmeans(mod4,~DOSI))

mod0$x
mod1$x
mod2$x
mod3$x
mod4$x

contr.treatment(5)
contr.sum(5)
contr.helmert(5)
contr.SAS(5)
