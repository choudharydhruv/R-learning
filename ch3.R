library(MASS)
library(ISLR)
### Simple linear regression
names(Boston)
pairs(Boston) #produces scatterplot matrix of all the variables
?Boston
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red",lwd=3) #line width and color of line
names(fit1)
confint(fit1)
#Plot fitted vs residuals and studentized residuals
plot(fit1$fitted, fit1$residuals) #OR
plot(fit1$fitted, residuals(fit1))

plot(rstudent(fit1) ~ fit1$fitted) #OR
plot(rstudent(fit1) ~ predict(fit1))

#Leverage statistic
plot(hatvalues(fit1))
which.max(hatvalues(fit1)) # which.max returns index of highest leverage statistic

plot(rstudent(fit1)~hatvalues(fit1)) # plots the studentized residuals vs the leverage statistics

#Alrenatively we can divide window in four parts and 
plot(fit1)

predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")

### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston)
summary(fit3)

summary(fit3)$r.sq #gives us Rsquared
summary(fit3)$sigma # gives us the RSE.

library(DAAG)
vif(fit3) # variance inflation factors for all the variables
par(mfrow=c(2,2))
plot(fit3)
fit4=update(fit3,~.-age-indus)
summary(fit4)
### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6)
anova(fit1,fit6) #anova can be used to compare two models, H0 is that models fit equally well, 
#Ha is that they dont

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
#pch plotting character
#points is used to plot a function
points(lstat,fitted(fit6),col="red",pch=20)
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)
###Qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)
###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
#... arguments get passed as is to the plot function inside
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)


#Plotting pairwise scatter plots
library(ggplot2)
library(GGally)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(Carseats[,(1:4)], lower.panel=panel.cor)
#Remove the cex=cex.cor*r if you want same size for all the coefficients

#Using ggpais in GGally 
require(GGally)
ggpairs(data=Carseats, columns= c(1,2,10), title="Car.data", colour = "Urban")

ggpairs(data=Carseats, columns= c(1,2,10), upper= list(continous="density"), lower = list(combo = "facetdensity"), title="Car.data", colour = "Urban")
#continuous: exactly one of ‘points’, ‘smooth’, ‘density’, ‘cor’ or ‘blank’;
#combo: exactly one of ‘box’, ‘dot’, ‘facethist’, ‘facetdensity’, ‘denstrip’ or ‘blank’;
#discrete: exactly one of ‘facetbar’,’ratio’ or ‘blank’.

ggpairs(data=Carseats, columns= c(1,2,7), colour = "ShelveLoc")