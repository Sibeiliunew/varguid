source("./20240425/simulation/generate_function_simulation.R")
source("./leash2.0.6.R")
source("./VarGuid20240626.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)
library(caTools)

nsim=1000

bias=function(b,true=1){
 return( mean(b)-true)
}

### n=20 rho=0
sce1_1$res %>% group_by(type) %>% summarize(group_mean=bias(V1))
### n=200
sce1_3$res %>% group_by(type) %>% summarize(group_mean=bias(V1))


### n=20 rho=0

round(mean(sce2_1$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce2_1$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce2_1$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce2_1$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce2_2$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce2_2$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce2_2$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce2_2$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

### n=200 rho=0

round(mean(sce2_3$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce2_3$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce2_3$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce2_3$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce2_4$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce2_4$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce2_4$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce2_4$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0
####################################
####################################

### n=20 rho=0

round(mean(sce3_1$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce3_1$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce3_1$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce3_1$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce3_2$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce3_2$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce3_2$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce3_2$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

### n=200 rho=0

round(mean(sce3_3$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce3_3$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce3_3$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce3_3$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce3_4$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce3_4$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce3_4$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce3_4$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0


##################################
##################################
### n=20 rho=0

round(mean(sce4_1$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce4_1$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce4_1$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce4_1$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce4_2$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce4_2$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce4_2$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce4_2$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

### n=200 rho=0

round(mean(sce4_3$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce4_3$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce4_3$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce4_3$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce4_4$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce4_4$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce4_4$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce4_4$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

##################################
##################################
### n=20 rho=0

round(mean(sce5_1$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce5_1$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce5_1$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce5_1$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce5_2$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce5_2$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce5_2$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce5_2$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

### n=200 rho=0

round(mean(sce5_3$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce5_3$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce5_3$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce5_3$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce5_4$res[1:1000,1:5] %>% apply(2,bias)),3) #ols beta=1
round(mean(sce5_4$res[1001:2000,1:5] %>% apply(2,bias)),3) # varguid beta=1

round(mean(sce5_4$res[1:1000,6:10] %>% apply(2,bias)),3) # ols beta=0
round(mean(sce5_4$res[1001:2000,6:10] %>% apply(2,bias)),3) # var beta=0

##############################################
########MSE


MSE1=function(b, true=1){
  return( mean((b-true)^2) )
}

MSE0=function(b, true=0){
  return( mean((b-true)^2) )
}

### n=20 rho=0
sce1_1$res %>% group_by(type) %>% summarize(group_mean=MSE1(V1))
### n=200
sce1_3$res %>% group_by(type) %>% summarize(group_mean=MSE1(V1))


### n=20 rho=0

round(mean(sce2_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce2_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce2_1$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce2_1$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce2_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce2_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce2_2$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce2_2$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

### n=200 rho=0

round(mean(sce2_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce2_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce2_3$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce2_3$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce2_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce2_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce2_4$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce2_4$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

################################
### n=20 rho=0

round(mean(sce3_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce3_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce3_1$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce3_1$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce3_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce3_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce3_2$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce3_2$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

### n=200 rho=0

round(mean(sce3_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce3_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce3_3$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce3_3$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce3_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce3_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce3_4$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce3_4$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

################################
### n=20 rho=0

round(mean(sce4_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce4_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce4_1$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce4_1$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce4_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce4_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce4_2$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce4_2$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

### n=200 rho=0

round(mean(sce4_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce4_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce4_3$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce4_3$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce4_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce4_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce4_4$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce4_4$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

################################
### n=20 rho=0

round(mean(sce5_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce5_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce5_1$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce5_1$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=20 rho=0.9
round(mean(sce5_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce5_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce5_2$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce5_2$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

### n=200 rho=0

round(mean(sce5_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce5_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce5_3$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce5_3$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#### n=200 rho=0.9
round(mean(sce5_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #ols beta=1
round(mean(sce5_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1

round(mean(sce5_4$res[1:1000,6:10] %>% apply(2,MSE0)),3) # ols beta=0
round(mean(sce5_4$res[1001:2000,6:10] %>% apply(2,MSE0)),3) # var beta=0

#########################################################################
#########################################################################
#########################################################################
###############CI table



#"MLE" "HC3" "HC0"
#names=names(res5$obj.varGuid.coef)[c(1,2,4)]


cover=function(ci,real){
 r1= ci[,1]<real
 r2=ci[,2]>real
 res_ci=NULL
 for (i in 1:length(r1)){
   if(r1[i]== T & r2[i]== T){new=TRUE}
   else{new=FALSE }
   res_ci=c(res_ci,new)}
 return(res_ci)
}

  
ci_organize=function(dat,beta_real){
  MLE_ci_1=MLE_ci_0=HC3_ci_1=HC3_ci_0=HC0_ci_1=HC0_ci_0=OLSHC3_ci_1=OLSHC0_ci_1=NULL
  OLSHC0_ci_0=OLSHC3_ci_0=NULL
  
  for ( i in 1:nsim ){
  X=as.matrix(dat[[i]][,1:(ncol(dat[[i]])-1)])
  Y=as.matrix(dat[[i]][,ncol(dat[[i]])])
  #res6=lm(Y~.,data=data.frame(X=X,Y=Y)) # OLS
  res5=lmv(X, Y)
  ### varguid
  if(ncol(X)==1){
  
  OLS=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC3")))[which(beta_real==1)+1,]
  OLSHC3_ci_1=rbind(OLSHC3_ci_1,OLS)
  OLS2=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC0")))[which(beta_real==1)+1,]
  OLSHC0_ci_1=rbind(OLSHC0_ci_1,OLS2)
  
  MLE_ci_1=rbind(MLE_ci_1,confint(res5$obj.varGuid.coef[[1]])[which(beta_real==1)+1,])
  HC3_ci_1=rbind(HC3_ci_1,confint(res5$obj.varGuid.coef[[2]])[which(beta_real==1)+1,])
  HC0_ci_1=rbind(HC0_ci_1,confint(res5$obj.varGuid.coef[[4]])[which(beta_real==1)+1,])
  }else{
    
    OLS=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC3")))[which(beta_real==1)+1,]
    OLSHC3_ci_1=rbind(OLSHC3_ci_1,OLS)
    OLS2=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC0")))[which(beta_real==1)+1,]
    OLSHC0_ci_1=rbind(OLSHC0_ci_1,OLS2)
    
    MLE_ci_1=rbind(MLE_ci_1,confint(res5$obj.varGuid.coef[[1]])[which(beta_real==1)+1,])
    HC3_ci_1=rbind(HC3_ci_1,confint(res5$obj.varGuid.coef[[2]])[which(beta_real==1)+1,])
    HC0_ci_1=rbind(HC0_ci_1,confint(res5$obj.varGuid.coef[[4]])[which(beta_real==1)+1,])
    MLE_ci_0=rbind(MLE_ci_0,confint(res5$obj.varGuid.coef[[1]])[which(beta_real==0)+1,])
    HC3_ci_0=rbind(HC3_ci_0,confint(res5$obj.varGuid.coef[[2]])[which(beta_real==0)+1,])
    HC0_ci_0=rbind(HC0_ci_0,confint(res5$obj.varGuid.coef[[4]])[which(beta_real==0)+1,])
    OLS3=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC3")))[which(beta_real==0)+1,]
    OLSHC3_ci_0=rbind(OLSHC3_ci_0,OLS3)
    OLS4=confint(coeftest(res5$obj.OLS, vcov = vcovHC(res5$obj.OLS, "HC0")))[which(beta_real==0)+1,]
    OLSHC0_ci_0=rbind(OLSHC0_ci_0,OLS4)
  }
  }
  
  if(ncol(X)==1){
    MLE_1=sum(cover(MLE_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    HC3_1=sum(cover(HC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    HC0_1=sum(cover(HC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)

    OLSHC3_1=sum(cover(OLSHC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    OLSHC0_1=sum(cover(OLSHC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)
    
    beta_1=data.frame(OLS_HC3=OLSHC3_1,OLS_HC0=OLSHC0_1,
                      MLE=MLE_1,HC3=HC3_1,HC0=HC0_1)
    return(list(beta_1=beta_1))}
  else{
  MLE_1=sum(cover(MLE_ci_1,real=1))/(length(which(beta_real==1))*nsim)
  MLE_0=sum(cover(MLE_ci_0,real=0))/(length(which(beta_real==0))*nsim)
  HC3_1=sum(cover(HC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
  HC3_0=sum(cover(HC3_ci_0,real=0))/(length(which(beta_real==0))*nsim)
  HC0_1=sum(cover(HC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)
  HC0_0=sum(cover(HC0_ci_0,real=0))/(length(which(beta_real==0))*nsim)
  
  #####
  OLSHC3_1=sum(cover(OLSHC3_ci_1,real=1))/(length(which(beta_real==1))*nsim)
  OLSHC0_1=sum(cover(OLSHC0_ci_1,real=1))/(length(which(beta_real==1))*nsim)
  OLSHC3_0=sum(cover(OLSHC3_ci_0,real=0))/(length(which(beta_real==0))*nsim)
  OLSHC0_0=sum(cover(OLSHC0_ci_0,real=0))/(length(which(beta_real==0))*nsim)
  
  
  beta_1=data.frame(OLS_HC3=OLSHC3_1,OLS_HC0=OLSHC0_1,
                    MLE=MLE_1,HC3=HC3_1,HC0=HC0_1)
  beta_0=data.frame(OLS_HC3=OLSHC3_0,OLS_HC0=OLSHC0_0,
                    MLE=MLE_0,HC3=HC3_0,HC0=HC0_0)
  return(list(beta_1=beta_1,beta_0=beta_0))}
  
}
#############
dat=readRDS("./20240601 simulated data/sce4_200with10with0.rds")
#ci_organize(dat,beta_real=1)
ci_organize(dat,beta_real=c(rep(1,5),rep(0,5)))
#ci_organize(dat,beta_real=c(rep(1,5),rep(0,10)))
######################
######################

###### re-simulate new data with same dim to get y hat



yhat=function(dat,test,lasso_status){
  rmse3 <- c()
  rmse_res3=NULL
  nsim=length(dat)
  for( i in 1:nsim){ 
    dat_sub=as.data.frame(dat[[i]])
    same_name=colnames(test[[1]])
    colnames(dat_sub[,1:(ncol(dat_sub)-1)])=same_name
    data=list(x.train = makeX(as.data.frame(dat_sub[,1:(ncol(dat_sub)-1)])),
              y.train = as.matrix(dat_sub[,ncol(dat_sub)]),
              x.test = makeX(as.data.frame(test[[1]])),
              y.test = test[[2]])
    
    colnames(data$x.train) <- colnames(data$x.test)
    
    o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = lasso_status) # , lasso = TRUE
    
    y.obj <-   tryCatch({
      ymodv(o,gamma = c(seq(0,9, length.out=4)), phi = 0.46)#, rf = FALSE)
    }, error=function(e){cat("error happens",i,"run")}) 
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse3 <- rbind(rmse3,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  
return(colMeans(as.data.frame(rmse3[,c(6,1)])) )# mean for the 1000 nsim
}
###############
dat <- readRDS("../varguid data/20240601 simulated data/sce1_20with1with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=1,
             gamma_real=NULL,corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("../20240601 simulated data/sce1_200with1with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=1,
             gamma_real=NULL,corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)









###################################################
###################################################
dat <- readRDS("./20240601 simulated data/sce2_200with10with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(1,5),rep(0,5)),corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("./20240601 simulated data/sce2_200with10with0.9.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(1,5),rep(0,5)),corrv=0.9)
yhat(dat = dat,test=test,lasso_status=FALSE)

##########################################################################
#########################################################################

dat <- readRDS("./20240601 simulated data/sce3_200with10with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=NULL,corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("./20240601 simulated data/sce3_200with10with0.9.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=NULL,corrv=0.9)
yhat(dat = dat,test=test,lasso_status=FALSE)

##########################################################################
#########################################################################

dat <- readRDS("./20240601 simulated data/sce4_200with10with0.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(0,5),rep(1,5)),corrv=0)
yhat(dat = dat,test=test,lasso_status=FALSE)


dat <- readRDS("./20240601 simulated data/sce4_200with10with0.9.rds")
test=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,5)),
             gamma_real=c(rep(0,5),rep(1,5)),corrv=0.9)
yhat(dat = dat,test=test,lasso_status=FALSE)

##########################################################################
#########################################################################


dat <- readRDS("./20240601 simulated data/sce5_20with15with0.rds")
test_sce5_20with15with0=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
                                 beta_real=c(rep(1,5),rep(0,10)),
                                 gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0)

saveRDS(test_sce5_20with15with0,"test_sce5_20with15with0.rds")

yhat(dat = dat,test=test_sce5_20with15with0,lasso_status=FALSE)


dat <- readRDS("./20240601 simulated data/sce5_20with15with0.9.rds")
test_sce5_20with15with0.9=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
                                   beta_real=c(rep(1,5),rep(0,10)),
                                   gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9)
#saveRDS(test_sce5_20with15with0.9,"test_sce5_20with15with0.9.rds")

yhat(dat = dat,test=test_sce5_20with15with0.9,lasso_status=FALSE)




dat <- readRDS("./20240601 simulated data/sce5_200with15with0.rds")
test_sce5_200with15with0=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,10)),
             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0)
#saveRDS(test_sce5_200with15with0,"test_sce5_200with15with0.rds")
yhat(dat = dat,test=test_sce5_200with15with0,lasso_status=FALSE)


dat <- readRDS("./20240601 simulated data/sce5_200with15with0.9.rds")
test_sce5_200with15with0.9=dat_sim(n=nrow(dat[[1]]),p=ncol(dat[[1]])-1,
             beta_real=c(rep(1,5),rep(0,10)),
             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,5)),corrv=0.9)

#saveRDS(test_sce5_200with15with0.9,"test_sce5_200with15with0.9.rds")
yhat(dat = dat,test=test_sce5_200with15with0.9,lasso_status=FALSE)
