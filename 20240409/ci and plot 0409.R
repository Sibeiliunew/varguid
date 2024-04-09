library("sandwich") 
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
source("./VarGuid20240406.R")

n=1000
nsim=500

#### please load sim5.RDS to get all sim_dat

######### initial
p=5;p2=5
sim_5=beta_ci=est_beta=NULL
gamma_real=rep(1,p)
beta_real=c(-1,-0.5,0,0.5,1)
sig=1
X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X

Y= X %*% beta_real+ X %*% gamma_real *rnorm(n,sd = sig)   # e with mean=0 and sd=1
res5=lmv(X, Y)
for(d in 1:p){
  beta_ci[[d]]=rbind(confint(res5$obj.OLS)[d+1,],confint(res5$obj.varGuid)[d+1,])
  est_beta[[d]]=res5$beta[d+1]
}


sim_5[[1]]=list(X,Y)

#####  

for (i in 2:nsim){
  Y= X %*% beta_real+ X %*% gamma_real *rnorm(n,sd = sig)  
  res5=lmv(X, Y)
  
  
  new=NULL
  for( d in 1:p){
    new[[d]]=rbind(confint(res5$obj.OLS)[d+1,],
                   confint(res5$obj.varGuid)[d+1,])# the order: OLS, VAR
  }
  # w: 1-5
  for (w in 1:p){
    beta_ci[[w]]=rbind(beta_ci[[w]],new[[w]])
    est_beta[[w]]=c(est_beta[[w]],res5$beta[w+1])
  }
  
  sim_5[[i]]=list(X=X,Y=Y)
  
}

########
ci_res=matrix(NA,nrow=p,ncol=2)
for (i in 1:p){
  beta5=as.data.frame(beta_ci[[i]]) %>% janitor::clean_names()
  
  beta5$type=rep(c("OLS","Varguid by lm func"),nsim)
  beta5$sim=factor(rep(1:nsim, each=2))
  OLS=beta5 %>% filter(type=="OLS")
  Var=beta5 %>% filter(type=="Varguid by lm func")
  
  ci_res[i,1]=1-length(union(which(OLS$x2_5_percent >beta_real[i]), which(OLS$x97_5_percent < beta_real[i])))/nsim
  ci_res[i,2]=1-length(union(which(Var$x2_5_percent >beta_real[i]), which(Var$x97_5_percent < beta_real[i])))/nsim
  
  colnames(ci_res)=c("OLS","Varguid")
  rownames(ci_res)=c("beta1", "beta2", "beta3","beta4","beta5")
  
}

ci_res

diff=NULL
for ( i in 1:p){
  diff[[i]]=est_beta[[i]]-beta_real[i]
}

par(mfrow = c(2, 3))

for ( c in 1:p){
  boxplot(diff[[c]])
  abline(h = 0,lty = 2 )
  title(sub =paste0( "Real beta =",beta_real[i]),x=paste0("beta",c),y="Difference")
}

for (i in 1:p){
  print(mean(est_beta[[i]]))
}



