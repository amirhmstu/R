# R #############################################
####################################################Regression analysis instructions in R
algo
regres
choose()
Reg2<-read.table(D,h=T)
Reg2
head(Reg2)
attach(Reg2)
matplot(y,cbind(x1,x2,x3,x4,x5))
library(car)
scatterplotMatrix(cbind(y,x1,x2,x3,x4,x5))
pairs(cbind(y,x1,x2,x3,x4,x5))
M<-lm(y~x1+x2+x3+x4+x5)
M
summary(M)
anova(M)
##monasebat model
R<-resid(M)
yhat<-fitted(M)
plot(y,yhat)
abline(lsfit(y,yhat),col=2)
Ru<-rstudent(M)
Rs<-rstandard(M)
r<-sqrt(abs(Rs))
par(mfrow=c(2,3))
plot(x1,Ru,col=1)
plot(x2,Ru,col=2)
plot(x3,Ru,col=3)
plot(x4,Ru,col=4)
plot(x5,Ru,col=5) 
par(mfrow=c(2,3))
plot(x1,Rs,col=1)
plot(x2,Rs,col=2)
plot(x3,Rs,col=3)
plot(x4,Rs,col=4)
plot(x5,Rs,col=5)
par(mfrow=c(2,3))
plot(x1,r,col=1)
plot(x2,r,col=2)
plot(x3,r,col=3)

plot(x4,r,col=4)
plot(x5,r,col=5)
par(mfrow=c(2,2))
plot(M)
abline(v=2*6/51,lty=2)
##kefayat model
qqnorm(R,col=2)
qqline(R)
P=shapiro.test(R)
text(-1,5,paste("p.value=",round(P$p.value,2)))
############################################################
Not completed yet ... continues

*********************************************************Coming Soon.........................................******
