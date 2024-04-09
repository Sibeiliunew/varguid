library("sandwich") 
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
source("VarGuid20240406.R")

n=1000
nsim=500


dat_sim=function(n,beta_real,sig,gamma_real){
  X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X
  e=rnorm(n,sd = sig)                            # e with mean=0 and sd=1
  Y= X %*% beta_real+ X %*% gamma_real *e  
  Y2=X %*% beta_real+e
  return(list(X=X, Y=Y,Y2=Y2))
}


####
sim_varguid=function(beta_real,gamma_real,sig=sig,nsim){
  MSE1=MSE2=MSE3=NULL
  for (i in 1:nsim) {
    sim=dat_sim(n,beta_real,sig = sig,gamma_real) 
    m1=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[3]]))  # OLS in normal setting
    MSE1[i]=mean((coef(m1)[-1]-beta_real)^2)
    
    m2=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[2]])) # OLS in gamma setting
    MSE2[i]=mean((coef(m2)[-1]-beta_real)^2)
    
    beta2=lmv(X=sim[[1]],Y=sim[[2]])
    MSE3[i]=mean((beta2[-1]-beta_real)^2)
  }
  return(list(MSE1=MSE1,MSE2=MSE2,MSE3=MSE3))
}


#########
p=5;p2=5
res5=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)
p=10;p2=10
res10=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)
p=20;p2=20
res20=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)
p=50;p2=50
res50=sim_varguid(beta_real=rep(-1,p),gamma_real=rep(1,p),sig=1,nsim)


par(mfrow = c(2, 2))
boxplot(res5[[1]],res5[[2]],res5[[3]],ylim=c(0,0.1),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=5")
boxplot(res10[[1]],res10[[2]],res10[[3]],ylim=c(0,0.15),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=10")
boxplot(res20[[1]],res20[[2]],res20[[3]],ylim=c(0,0.2),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=20")
boxplot(res20[[1]],res50[[2]],res50[[3]],ylim=c(0,0.3),
        names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=50")
#title("Rea")
mtext("Real beta=-1, nsim=500, n=1000", side = 3, line = -2, outer = T)

MSE_table=rbind(colMeans(as.data.frame(res5)),colMeans(as.data.frame(res10)),
                colMeans(as.data.frame(res20)),colMeans(as.data.frame(res50)))

rownames(MSE_table)=c("p=5","p=10","p=20","p=50")
MSE_table


df=data.frame (
 Beta = c("beta1", "beta2", "beta3","beta4","beta5"),
  OLS = c(0.902,0.884,0.912,0.924,0.902),
  Varguid = c(0.918,0.908,0.934,0.93,0.924)
) %>% print()

### get the bias 

