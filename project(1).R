rm(list=ls())
library(quantmod)
library(fGarch)

#(a)
getSymbols('VIIIX',from='1998-1-1',to='2017-4-7')
getSymbols('VGTSX',from='1998-1-1',to='2017-4-7')
priceviiix<-VIIIX$VIIIX.Close
pricevgtsx<-VGTSX$VGTSX.Close
retviiix   <- diff(log(priceviiix))
retvgtsx   <- diff(log(pricevgtsx)) 
retviiix  <- retviiix [-1,]
retvgtsx  <-retvgtsx [-1,]
T            <- length(retviiix)

windowlength <- 50
rollingCov   <- numeric(T)

for (i in 50:T){
  rollingCov[i] <- cov(retviiix[(i-49):i], retvgtsx[(i-49):i])
}
plot(rollingCov, type='l', col='blue')

#(b)
fit1  <- garchFit( formula = ~garch(1, 1), data = retviiix, trace = FALSE)
sigma1 <- sqrt(fit1@h.t)
retviiixstand <- retviiix/sigma1

fit2  <- garchFit( formula = ~garch(1, 1), data = retvgtsx, trace = FALSE)
sigma2 <- sqrt(fit2@h.t)
retvgtsxstand <- retvgtsx/sigma2

lambda      <- 0.94
q11         <- numeric(T)
q12         <- numeric(T)
q22         <- numeric(T)

for (i in 2:T){
  q11[i] <- (1-lambda)*retviiixstand[i-1]^2 + lambda*q11[i-1]
  q12[i] <- (1-lambda)*retviiixstand[i-1]*retvgtsxstand[i-1] + lambda*q12[i-1]
  q22[i] <- (1-lambda)*retvgtsxstand[i-1]^2 + lambda*q22[i-1]
}

exponentialCorr <- q12/sqrt(q11*q22)
plot(exponentialCorr, type='l', col='blue')


#(c)
alpha       <- 0.05
beta        <- 0.9
p11         <- numeric(T)
p12         <- numeric(T)
p22         <- numeric(T)
p11lr       <- mean(retviiixstand^2)
p12lr       <- mean(retviiixstand*retvgtsxstand)
p22lr       <- mean(retvgtsxstand^2)

for (i in 2:T){
  p11[i] <- p11lr + alpha*(retviiixstand[i-1]^2 - p11lr) + beta*(p11[i-1]-p11lr)
  p12[i] <- p12lr + alpha*(retviiixstand[i-1]*retvgtsxstand[i-1] - p12lr) + beta*(p12[i-1]-p12lr)
  p22[i] <- p22lr + alpha*(retvgtsxstand[i-1]^2 - p22lr) + beta*(p22[i-1]-p22lr)
}

GarchCorr <- p12/sqrt(p11*p22)
plot(GarchCorr, type='l', col='blue')
points(exponentialCorr, type='l', col='red')

