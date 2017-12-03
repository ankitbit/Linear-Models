
options(contrasts = c("contr.helmert", "contr.treatment"))

dd<-read.csv2("PRAC2F.csv")
dd$F1<-as.factor(dd$F1)
dd$F2<-as.factor(dd$F2)

library(RcmdrMisc)
library(lsmeans)
library(tables)
tabular( (mean)*V1*(F1+1) ~ (F2+1) ,data=dd)
tabular( (1)*V1*(F1+1) ~ (F2+1) ,data=dd)
with(dd, plotMeans(V1, F2, F1, error.bars="conf.int", level=0.95, connect=TRUE))
with(dd, plotMeans(V1, F1, F2, error.bars="conf.int", level=0.95, connect=TRUE))

m1<-lm(V1~F1*F2,data=dd)
summary(m1)
anova(m1)
Anova(m1,ty=3)
cld(lsmeans(m1,~F1*F2),Letters=letters,reversed=T)
cld(lsmeans(m1,~F1|F2),Letters=letters,reversed=T)
cld(lsmeans(m1,~F2|F1),Letters=letters,reversed=T)

cld(lsmeans(m1,~F1),Letters=letters,reversed=T)
cld(lsmeans(m1,~F2),Letters=letters,reversed=T)

plot(fitted(m1),resid(m1))
abline(h=0,lt=2)

mad<-lm(V1~F1+F2,data=dd)
summary(mad)
anova(m1)
anova(mad)
Anova(mad,ty=3)
cld(lsmeans(mad,~F1),Letters=letters,reversed=T)
cld(lsmeans(mad,~F2),Letters=letters,reversed=T)

plot(fitted(mad),resid(mad))
abline(h=0,lt=2)

man<-lm(V1~F1+F1:F2,data=dd)
summary(man)
anova(m1)
anova(man)
Anova(man,ty=3)
cld(lsmeans(man,~F1),Letters=letters,reversed=T)
cld(lsmeans(man,~F1:F2),Letters=letters,reversed=T)

plot(fitted(man),resid(man))
abline(h=0,lt=2)

