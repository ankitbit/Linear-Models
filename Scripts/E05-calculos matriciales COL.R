
COL <- read.csv2("COL.csv")
p<-4
n<-dim(COL)[1]
library(car)

mod<-lm(C~P+E+H,COL)
mod0<-lm(C~1,COL)

##### write("Matriu","")

x<-as.matrix(cbind(1,COL[,c(3,1,2)]))
y<-COL$C
n<-dim(x)[1]
p<-dim(x)[2]


##### write("Paràmetres","")

(xtx<-t(x)%*%x)
(xty<-t(x)%*%y)
(xtxi<-solve(xtx))
(b<-xtxi%*%xty)

mod$coef
summary(mod)$cov

##### write("Residuals i predits","")

yp<-x%*%b
r<-y-yp
plot(yp,r)
points(predict(mod),resid(mod),col="red",pch="+")
abline(h=0,lty=2)

##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("ANOVA i SE","")
(GLM<-dim(x)[2]-1)
(GLE<-dim(x)[1]-dim(x)[2])
(SQM<-(t(yp-mean(y))%*%(yp-mean(y)))[1,1])
(sum((yp-mean(y))^2))
(SQE<-(t(y-yp)%*%(y-yp))[1,1])
sum((y-yp)^2)
(MQM<-SQM/GLM)
(MQE<-SQE/GLE)
(SE<-sqrt(MQE))
(Fcalc<-MQM/MQE)
(pvalor<-1-pf(Fcalc,GLM,GLE))
-pf(Fcalc,GLM,GLE,log=T)
SQT<-SQM+SQE
GLT<-GLM+GLE
MQT<-SQT/GLT
print(c(MQT,var(y)))
(R2<-SQM/(SQM+SQE)) # 1-SQE/SQT
(R2adj<-1-MQE/(MQT))

summary(mod)
anova(mod0,mod)

##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("Estimació paràmetres","")

(vb<-xtxi*MQE)
(vcov(mod))
(sb<-sqrt(diag(vb)))
icb<-cbind(b,b-qt(0.975,GLE)*sb,b+qt(0.975,GLE)*sb)
colnames(icb)<-c("b","inf","sup")
print(icb)
confint(mod)
(tb0<-b/sb)
(pt(-abs(tb0),GLE)*2)
summary(mod)

##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("Estimació i predicció de y","")
##### write("Per exemple, quan P=65, E=15 i H=150","")

x0<-c(1,65,15,150)
(yp0<-(x0%*%b)[1,1])
(syp0<-sqrt((x0%*%vb%*%x0)[1,1]))
(icyp0<-c(yp0-qt(0.975,GLE)*syp0,yp0+qt(0.975,GLE)*syp0))
(predict(mod, data.frame(P=65,E=15, H=150, row.names="x0"), interval="confidence", level=.95, se.fit=TRUE))
(rpyp0<-c(yp0-qt(0.975,GLE)*sqrt(MQE+syp0^2),yp0+qt(0.975,GLE)*sqrt(MQE+syp0^2)))
(predict(mod, data.frame(P=65,E=15, H=150, row.names="x0"), interval="prediction", level=.95))
##### write("test, per exemple yp0=200","")
(t0<-(yp0-200)/syp0)
(2*pt(-abs(t0),GLE))

##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("SS1 i SS3","")

tb0^2
(summary(mod)$coef[,3])^2

anova(mod)
Anova(mod,ty=3)

##### write("SS1","")

anova(mod)
mod10<-lm(C~P,COL)
mod20<-lm(C~P+E,COL)
#anova(mod0,mod10)
sum((predict(mod0)-predict(mod10))^2)
#anova(mod10,mod20)
sum((predict(mod10)-predict(mod20))^2)
#anova(mod20,mod)
sum((predict(mod20)-predict(mod))^2)
sum((predict(mod)-y)^2)

##### write("SS3","")

modnc<-lm(C~0+P+E+H,COL)
mod1<-lm(C~E+H,COL)
mod2<-lm(C~P+H,COL)
mod3<-lm(C~P+E,COL) #és el mateix que mod30
Anova(mod,ty=3)
#anova(mod1,mod)
#anova(mod2,mod)
#anova(mod3,mod)
sum((predict(modnc)-predict(mod))^2)
sum((predict(mod1)-predict(mod))^2)
sum((predict(mod2)-predict(mod))^2)
sum((predict(mod3)-predict(mod))^2)
sum((y-predict(mod))^2)



##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("residuals estandaritzats, valors hat i distància de Cooks","")

hat<-x%*%xtxi%*%t(x)
vary<-hat*MQE
varr<-MQE-vary
rs<-r/sqrt(diag(varr))
plot(1:n,rs,main="Residuals estandaritzats")
points(1:n,rstandard(mod),col="red",pch="+")
abline(h=c(-2,0,2),lty=2)

plot(1:n,diag(hat),main="valors hat")
points(1:n,hatvalues(mod),col="red",pch="+")
abline(h=c(0,2*mean(diag(hat))),lty=2)

cd<-rs^2/(p)*diag(hat)/(1-diag(hat))
plot(1:n,cd,main="distància de Cooks")
points(1:n,cooks.distance(mod),col="red",pch="+")
abline(h=c(0,4/n),lty=2)

##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("residuals studentitzats, dffits i dfbetes","")
##### write("comprovarem matricialment només per una dada, per exemple la 82, i parcialment","")

no<-82
modno<-lm(C~P+E+H,COL[-no,])
SEno<-summary(modno)$sigma
c(rstudent(mod)[no],rstandard(mod)[no]*SE/SEno)
plot(1:n,rstudent(mod),main="residuals studentitzats")
abline(h=c(-2,0,2),lty=2)
xno<-x[no,]
c(dffits(mod)[no],(mod$fitted.values[no]-xno%*%modno$coef)/sqrt(hatvalues(mod)[no])/SEno)
plot(1:n,dffits(mod),main="dffits")
abline(h=c(-2*sqrt(p/n),0,2*sqrt(p/n)),lty=2)
rbind(dfbetas(mod)[no,],t(b-modno$coef)/(sqrt(diag(vcov(mod)))/SE*SEno))
plot(1:n,dfbetas(mod)[,1],main="dfbetas int")
abline(h=c(-2/sqrt(n),0,2/sqrt(n)),lty=2)
plot(1:n,dfbetas(mod)[,2],main="dfbetas P")
abline(h=c(-2/sqrt(n),0,2/sqrt(n)),lty=2)
plot(1:n,dfbetas(mod)[,3],main="dfbetas E")
abline(h=c(-2/sqrt(n),0,2/sqrt(n)),lty=2)
plot(1:n,dfbetas(mod)[,4],main="dfbetas H")
abline(h=c(-2/sqrt(n),0,2/sqrt(n)),lty=2)

##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("colinealitat: tolerància i VIF","")

(tol<-c(P=1-summary(lm(P~E+H,COL))$r.squared,E=1-summary(lm(E~P+H,COL))$r.squared,
       H=1-summary(lm(H~P+E,COL))$r.squared))
(cbind(vif=vif(mod),"1/tol"=1/tol))

##### write("","")
##### write("__________________________________________________________","")
##### write("","")
##### write("canvis lineals","")

c<-diag(1,4,4)
c[1,2]<-10
c[4,2]<--.5
COL$EP<-COL$P-0.5*COL$H+10
modc<-lm(C~EP+E+H,COL)
summary(modc)
summary(mod)
anova(modc,mod)
vif(modc)
vif(mod)
summary(lm(C~I(P+10*H)+E+H,COL))
summary(lm(C~I(P-mean(P))+I(E-mean(E))+I(H-mean(H)),COL))
confint(lm(C~I(P-mean(P))+I(E-mean(E))+I(H-mean(H)),COL))
confint(mod)
summary(lm(C~I(P-0.5*H+10)+I(E-15)+I(H-150),COL))
confint(lm(C~I(P-0.5*H+10)+I(E-15)+I(H-150),COL))
summary(lm(C~I(P-0.5*H+10)+I(E-15),COL))


