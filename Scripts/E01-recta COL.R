#setwd("~/ASSIGNATURES/200641 - MODELS LINEALS I LINEALS GENERALITZATS")
COL <- read.csv2("./COL.csv")
p<-2
n<-dim(COL)[1]
library(car)
library(HH)


# MODELO
mod<-lm(C~P,COL)

# RESUMEN DEL MODELO
summary(mod)

#Calculos opcionales: Intervalos de confiança de los parametros
confint(mod,level=0.99)

#Calculos opcionales: SS1 Test de los parametros con ordenacions predeterminada
anova(mod)

#Nota: SS3, los tests (F) siempre coinciden con los del resumen (t), F=t^2
Anova(mod,ty=3)


#Gr�fica con bandas de confianza i predicción
ci.plot(mod)

# Diagnostico: TENDENCIAS
plot(predict(mod),resid(mod))
abline(h=0,lty=2)

# Diagnostico: OUTLIERS (rstudent)
plot(rstandard(mod))
abline(h=c(-2,0,2),lty=2)

plot(rstudent(mod),main="rstudent")
abline(h=c(-2,0,2),lty=2)

# Diagnostico: LEVERAGE
plot(hatvalues(mod))
abline(h=c(0,2*mean(hatvalues(mod))),lty=2)

# Diagnostico: INFLUENCIA (dffits)
plot(cooks.distance(mod))
abline(h=c(0,4/n),lty=2)

plot(dffits(mod),main="dffits")
abline(h=c(-2*sqrt(p/n),0,2*sqrt(p/n)),lty=2)


#Diagnosticos de R
oldpar <- par( mfrow=c(2,2))
plot(mod,ask=F)
par(oldpar)

#
sp(C~P, reg.line=lm, smooth=F, groups=COL$E, data=COL)



