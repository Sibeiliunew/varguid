library("lmtest")
library("sandwich") 
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)


n=100
nsim=1000


dat_sim=function(n,p,p2,beta_real,sig,gamma){
  X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X
  e=rnorm(n,sd = sig)                            # e with mean=0 and sd=1
  Y= X %*% beta_real+ X[,1:p2]%*% gamma *e  
  return(list(X=X, Y=Y))
}


#######################################
var_est=function(beta_real,beta_initial,X,Y,sig){
  df=data.frame(X=X,Y=Y)
w_est <- function(gamma){
  W <- NULL
  for (i in 1:n){
    #new <- 1/((t(X[i,]^2)%*% gamma)) 
    new <- 1/((t(X[i,]^2)%*% gamma^2))
    W <- c(W,new)
  }
  W <- diag(W)
  return(W)
}

beta_est=function(gamma){
  W <- w_est(gamma)
  beta <- solve(t(X) %*% W%*%X) %*%t(X)%*% W %*%Y
  return(beta)
}

gamma_est=function(beta){
  X2 <- X^2
  gamma <- (solve(t(X2)%*% X2) %*% t(X2) %*% (Y-X%*% beta )^2) /sig 
  return(gamma)
}

beta=beta_initial
diff1 <- diff2 <- 10
gamma=gamma_est(beta)

tol <- M <-  exp(-100)
allbeta <- allgamma <- c()
while ( diff1 > tol & M < 100) { #diff2 >tol & 
  old_beta <- beta 
  old_gamma <- gamma 
  beta <- beta_est(gamma)
  gamma=gamma_est(beta) 
  diff1=sum((beta-old_beta)^2)
  M <- M + 1
}

return(beta)

}

####
sim_varguild=function(p,p2,beta_real,beta_initial,gamma,sig=sig){
  MSE1=MSE2=NULL
for (i in 1:nsim) {
  sim=dat_sim(n,p,p2,beta_real,sig = sig,gamma) 
  m1=lm(Y~.-1,data=data.frame(X=sim[[1]],Y=sim[[2]])) 
  MSE1[i]=mean((coef(m1)-beta_real)^2)
  
  beta2=var_est(beta_real,beta_initial=beta_initial,X=sim[[1]],Y=sim[[2]],sig=sig)
  MSE2[i]=mean((beta2-beta_real)^2)
}
  return(list(MSE1=MSE1,MSE2=MSE2))
}


#########
p=5;p2=5
res5=sim_varguild(p=5,p2=5,beta_real=rep(1,p),beta_initial=rep(0.5,p),gamma=rep(0.3,p),sig=1)
p=10;p2=10
res10=sim_varguild(p=10,p2=10,beta_real=rep(1,p),beta_initial=rep(0.5,p),gamma=rep(0.3,p),sig=1)
p=20;p2=20
res20=sim_varguild(p=20,p2=20,beta_real=rep(1,p),beta_initial=rep(0.5,p),gamma=rep(0.3,p),sig=1)
p=50;p2=50
res50=sim_varguild(p=50,p2=50,beta_real=rep(1,p),beta_initial=rep(0.5,p),gamma=rep(0.3,p),sig=1)

par(mfrow = c(2, 2))
boxplot(res5[[1]],res5[[2]],names=c("OLS","varGuild"),ylab='MSE',main="p=5")
boxplot(res10[[1]],res10[[2]],names=c("OLS","varGuild"),ylab='MSE',main="p=10")
boxplot(res20[[1]],res20[[2]],names=c("OLS","varGuild"),ylab='MSE',main="p=20")
boxplot(res50[[1]],res50[[2]],names=c("OLS","varGuild"),ylab='MSE',main="p=50")



##### increase the p to 50
# group some variables

n=1000  
p=50; r1=c(rep(1,10),rep(0.5,10),rep(0,10),rep(-0.5,10),rep(-1,10)) 
SE1_table=SE2_table=p_ori=p_correct=matrix(nrow = p,ncol = nsim)
for (i in 1:nsim) {
  sim=dat_sim(n,p,r1) # r1 for real 
  #lm
  m1=lm(Y1~.,sim[,-(p+2)]) ### change to p
  #bias1[[i]]=coef(m1)[-1]-r1
  # SSE1[i]=sum((coef(m1)[-1]-r1)^2)
  SE1_table[,i]=(coef(m1)[-1]-r1)^2
  #sr[,i]=summary(m1)$coefficients[-1,2]
  p_ori[,i]=summary(m1)$coefficients[-1,4]
  
  
  m2=lm(Y2~.,sim[,-(p+1)])
  #bias2[[i]]=coef(m2)[-1]-r1
  vv2 <- vcovHC(m2, type="HC1")
  SE2_table[,i]=(coeftest(m2, vcov = vv2)[-1,1]-r1)^2
  p_correct[,i]=coeftest(m2, vcov = vv2)[-1,4]
}


group=function(table,m){
  res=NULL
  for (i in 1:nrow(table)){
    num=sample.int(1000, m/nrow(table), replace = TRUE)
    res=c(res,table[i,num])}
  return(res)
}

SSE1_plot=data.frame(X1=SE1_table[1,],X2=SE1_table[2,],
                     coef_1=group(SE1_table[11:20,],m=1000),
                     coef_0.5=group(SE1_table[21:30,],m=1000),
                     coef_0=group(SE1_table[31:40,],m=1000),
                     coef_0.5minus=group(SE1_table[41:50,],m=1000)
)
boxplot(t(t(SSE1_plot)),ylab='SE',main="p=50 SE for different coefficient groups")
mtext("based on 1000 sim, 1000 samples for original scenario", side=3)

SSE2_plot=data.frame(X1=SE2_table[1,],X2=SE2_table[2,],
                     coef_2=group(SE2_table[3:10,],m=1000),
                     coef_1=group(SE2_table[11:20,],m=1000),
                     coef_0.5=group(SE2_table[21:30,],m=1000),
                     coef_0=group(SE2_table[31:40,],m=1000),
                     coef_0.5minus=group(SE2_table[41:50,],m=1000)
)
boxplot(t(t(SSE2_plot)),ylab='SE',main="p=50 SE for different coefficient groups")
mtext("based on 1000 sim, 1000 samples for white-corrected scenario", side=3)

