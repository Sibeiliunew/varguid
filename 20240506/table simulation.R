#source("./20240425/simulation/generate_function_simulation.R")
source("./20240425/leash2.0.2.R")
source("./20240425/VarGuid20240502.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)



nsim <- 1000

dat_sim=function(n,p,beta_real,gamma_real,corrv){
    X <- as.matrix(rnorm_multi(n = n, 
                     mu = rep(0,p),
                     sd = rep(1,p),
                     r = corrv, 
                     varnames = letters[1:p],
                     empirical = FALSE))
     e=rnorm(n,sd = 1)                            
     Y= as.matrix(X) %*% beta_real+ X %*% gamma_real *e  
     return(list(X=X, Y=Y))
}

sim_varguid=function(n,p,beta_real,gamma_real,corrv){
  SE1=SE2=vector(mode='list', p)
  dat=vector(mode='list', nsim)
  for (i in 1:nsim) {
    sim=dat_sim(n,p,beta_real,gamma_real,corrv)
    dat[[i]]=cbind(sim[[1]],sim[[2]])
    
    m2=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[2]])) # OLS 
    ols_test_breusch_pagan(m2)
    holder=(coef(m2)[-1]-beta_real)^2
    
    beta2=lmv(X=sim[[1]],Y=sim[[2]])$beta
    holder2=(beta2[-1]-beta_real)^2
    
    for(d in 1:p){
      SE1[[d]]=c(SE1[[d]],holder[d])
      SE2[[d]]=c(SE2[[d]],holder2[d])
    }
    
  }
  saveRDS(dat,paste(eval(parse(text=n)),"with",eval(parse(text=p)),"with",eval(parse(text=corrv)),".rds",sep=""))
  
  SE1=as.data.frame(do.call(cbind,SE1))
  SE2=as.data.frame(do.call(cbind,SE2))
  SE1$type=rep("OLS",nrow(SE1))
  SE2$type=rep("Varguid",nrow(SE2))
  res=rbind(SE1,SE2)
  return(res=res)
}

## n=20 ,p=10, cor=0
n=20;p=10
beta_real=c(rep(1,5),rep(0,5))
gamma_real=c(rep(1,5),rep(0,5))
corrv <- 0

sce2_1=sim_varguid(n,p,beta_real,gamma_real,corrv) # n=20 ,p=10, cor=0


