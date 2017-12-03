
X<-state.x77[,1:3]
Y<-state.x77[,4]


beta<-solve(t(X)%*%X)%*%t(X)%*%Y
plot(beta)

state.x77<-data.frame(state.x77)
lg<-lm(state.x77[4,]~state.x77[,1]+state.x77[,2]+state.x77[,3])

plot(lm(state.x77$Life.Exp ~ state.x77$Population+state.x77$Income+state.x77$Illiteracy, # regression formula
   data= state.x77))



