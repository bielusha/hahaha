#(a)
rm(list=ls())
library('fGarch')
MC <- 50000
T  <- 500
shock <- matrix(0, MC, T)
for (i in 1:T){
  shock[, i] <- rnorm(MC, 0, 1)
}
lambda      <- 0.94
ReturnMC <- matrix(NA, MC, T)
for (i in 1:MC){
  sigmapredMC <- 0.01
  for (j in 1:T){
    ReturnMC[i, j] <- sigmapredMC*shock[i, j]
    sigmapredMC <- sqrt( lambda*sigmapredMC^2+(1-lambda)*ReturnMC[i, j]^2)
  }
}


ReturnMCT <- matrix(NA, MC, T)
for (i in 1:MC){
  ReturnMCT[i, ] <- cumsum(ReturnMC[i, ])
}

esvol<-numeric(T)
for  (i in 1:T){
esvol[i]<-sqrt(var(ReturnMCT[,i]))
}

plot(esvol/(0.01*sqrt(1:T)), type='l', col='red')
plot(esvol/(0.01*sqrt(1:T))-1, type='l', col='red')


#(c)
rm(list=ls())
library('fGarch')
MC <- 50000
T  <- 500
shock <- matrix(0, MC, T)
for (i in 1:T){
  shock[, i] <- rnorm(MC, 0, 1)
}

omega <- 1e-6
alpha <- 0.05
beta  <- 0.9
sigmapred<-sqrt(omega/(1-alpha-beta))

ReturnMC <- matrix(NA, MC, T)
for (i in 1:MC){
  sigmapredMC <- sigmapred
  for (j in 1:T){
    ReturnMC[i, j] <- sigmapredMC*shock[i, j]
    sigmapredMC <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapredMC^2)
  }
}


ReturnMCT <- matrix(NA, MC, T)
for (i in 1:MC){
  ReturnMCT[i, ] <- cumsum(ReturnMC[i, ])
}

esvol<-numeric(T)
for  (i in 1:T){
  esvol[i]<-sqrt(var(ReturnMCT[,i]))
}

plot(esvol/(sigmapred*sqrt(1:T)), type='l', col='red')


#(d)
rm(list=ls())
library('fGarch')
MC <- 50000
T  <- 500
shock <- matrix(0, MC, T)
for (i in 1:T){
  shock[, i] <- rnorm(MC, 0, 1)
}

omega <- 1e-6
alpha <- 0.05
beta  <- 0.9
sigmapred<-3*sqrt(omega/(1-alpha-beta))

ReturnMC <- matrix(NA, MC, T)
for (i in 1:MC){
  sigmapredMC <- sigmapred
  for (j in 1:T){
    ReturnMC[i, j] <- sigmapredMC*shock[i, j]
    sigmapredMC <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapredMC^2)
  }
}


ReturnMCT <- matrix(NA, MC, T)
for (i in 1:MC){
  ReturnMCT[i, ] <- cumsum(ReturnMC[i, ])
}

esvol<-numeric(T)
for  (i in 1:T){
  esvol[i]<-sqrt(var(ReturnMCT[i,]))
}

plot(esvol/(sigmapred*sqrt(1:T)), type='l', col='red')

