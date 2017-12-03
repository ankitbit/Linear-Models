options(contrasts = c("contr.helmert", "contr.treatment"))
dd <- read.csv2("FORMATGES.csv")
library(tables)
library(lsmeans)
library(car)
library(RcmdrMisc)

tabular((R_VACA+R_CABRA+R_OVELLA)*(TERMIC+1)~(CaCl2+1)*mean,dd)
tabular((R_VACA+R_CABRA+R_OVELLA)*(TERMIC)~(CaCl2)*sd,dd)
tabular((R_VACA+R_CABRA+R_OVELLA)*(TERMIC+1)~(CaCl2+1)*(n=1),dd)
tabular(TERMIC~R_VACA*CaCl2*mean,dd)

with(dd, plotMeans(R_VACA, TERMIC, CaCl2, error.bars="conf.int", level=0.95, connect=TRUE))
with(dd, plotMeans(R_VACA, CaCl2, TERMIC, error.bars="conf.int", level=0.95, connect=TRUE))

with(dd, plotMeans(R_CABRA, TERMIC, CaCl2, error.bars="conf.int", level=0.95, connect=TRUE))
with(dd, plotMeans(R_CABRA, CaCl2, TERMIC, error.bars="conf.int", level=0.95, connect=TRUE))

with(dd, plotMeans(R_OVELLA, TERMIC, CaCl2, error.bars="conf.int", level=0.95, connect=TRUE))
with(dd, plotMeans(R_OVELLA, CaCl2, TERMIC, error.bars="conf.int", level=0.95, connect=TRUE))

#====================================
mv<-lm(R_VACA~TERMIC*CaCl2,dd)
summary(mv)
anova(mv)
Anova(mv,t=3)
cld(lsmeans(mv,~TERMIC*CaCl2),Letters=letters,reversed=T)
cld(lsmeans(mv,~TERMIC|CaCl2),Letters=letters,reversed=T)
cld(lsmeans(mv,~CaCl2|TERMIC),Letters=letters,reversed=T)
cld(lsmeans(mv,~CaCl2),Letters=letters,reversed=T)
cld(lsmeans(mv,~TERMIC),Letters=letters,reversed=T)

plot(predict(mv),resid(mv))
abline(h=0,lty=2)
plot(1:16,rstudent(mv))
abline(h=c(-2,0,2),lty=2)

plot(mv,ask=F)

#====================================
mc<-lm(R_CABRA~TERMIC*CaCl2,dd)
anova(mc)
Anova(mc,t=3)
summary(mc)

cld(lsmeans(mc,~TERMIC*CaCl2),Letters=letters,reversed=T,alpha=0.1)
#cld(lsmeans(mc,~TERMIC|CaCl2),Letters=letters,reversed=T)
#cld(lsmeans(mc,~CaCl2|TERMIC),Letters=letters,reversed=T)

mc0<-lm(R_CABRA~TERMIC,dd)
anova(mc0)

#====================================
mo<-lm(R_OVELLA~TERMIC*CaCl2,dd)
summary(mo)
anova(mo)
Anova(mo,t=3)
cld(lsmeans(mo,~TERMIC*CaCl2),Letters=letters,reversed=T)
cld(lsmeans(mo,~TERMIC|CaCl2),Letters=letters,reversed=T)
cld(lsmeans(mo,~CaCl2|TERMIC),Letters=letters,reversed=T)
cld(lsmeans(mo,~CaCl2),Letters=letters,reversed=T)
cld(lsmeans(mo,~TERMIC),Letters=letters,reversed=T)

mo0<-lm(R_OVELLA~TERMIC+CaCl2,dd)
anova(mo0)
cld(lsmeans(mo0,~CaCl2),Letters=letters,reversed=T)
cld(lsmeans(mo0,~TERMIC),Letters=letters,reversed=T)

