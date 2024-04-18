library("sandwich") 
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
source("./20240412/VarGuid20240412.R")
source("./20240412/leash2.0.R")

n=1000
nsim=500


# dat_sim=function(n,beta_real,sig,gamma_real){
#   X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X
#   e=rnorm(n,sd = sig)                            # e with mean=0 and sd=1
#   Y= X %*% beta_real+ X %*% gamma_real *e  
#   Y2=X %*% beta_real+e
#   return(list(X=X, Y=Y,Y2=Y2))
# }

dat_sim_related=function(n,beta_real,p,r){
  z=rnorm(n=n, mean =0, sd = 1)
  X=list()
  X=vector(mode='list', p)
  for ( i in 1:p){
  e = rnorm(n=n, mean =0, sd = 1)
  X[[i]]= z* r^i+ e }
  X=do.call(cbind,X)
  Y= X %*% beta_real+ e 
  return(list(X=X, Y=Y))
}

dat_sim_uni=function(n,beta_real,sig,gamma_real){
  X=matrix(runif(n=n*p, min =0, max = 1),nrow=n) 
  e=rnorm(n,sd = sig)                            
  Y= X %*% beta_real+ X %*% gamma_real *e  
 # Y2=X %*% beta_real+e
  return(list(X=X, Y=Y))
}

####
#sim_varguid=function(beta_real,gamma_real,sig=sig,nsim){

sim_varguid=function(beta_real,nsim,p,sig,gamma_real){
  MSE1=MSE2=list()
  MSE1=MSE2 =vector(mode='list', p)
  for (i in 1:nsim) {
    sim=dat_sim_uni(n,beta_real,sig=sig,gamma_real)
    #m1=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[3]]))  # OLS in normal setting
    #MSE1[i]=mean((coef(m1)[-1]-beta_real)^2)
    
    m2=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[2]])) # OLS 
    holder=(coef(m2)[-1]-beta_real)^2
    beta2=lmv(X=sim[[1]],Y=sim[[2]])$beta
    holder2=(beta2[-1]-beta_real)^2
    for(d in 1:p){
        MSE1[[d]]=c(MSE1[[d]],holder[d])
        MSE2[[d]]=c(MSE2[[d]],holder2[d])
      }
    }
  
  MSE1=as.data.frame(do.call(cbind,MSE1))
  MSE2=as.data.frame(do.call(cbind,MSE2))
   MSE1$type=rep("OLS",nrow(MSE1))
   MSE2$type=rep("Varguid",nrow(MSE2))
   res=rbind(MSE1,MSE2)
  return(res)
}

#########
r=0.9
p=5
res5=sim_varguid(beta_real=rep(-1,p),nsim=nsim,p=p,sig=1,gamma_real=rep(1,p))
colnames(res5)=c("beta1","beta2","beta3","beta4","beta5")
colMeans(res5[1:500,1:5])
colMeans(res5[500:1000,1:5])
#plot5=pivot_longer(res5,cols=V1:V5,   names_to='beta',values_to='Error') 
#ggplot(plot5,aes(x=beta,y=Error,color=type))+geom_boxplot()+theme_classic()

p=10
res10=sim_varguid(beta_real=rep(-1,p),nsim=nsim,p=p,sig=1,gamma_real=rep(1,p))
colnames(res10)=c("beta1","beta2","beta3","beta4","beta5","beta6","beta7","beta8","beta9","beta10")
colMeans(res10[1:500,1:10])
colMeans(res10[500:1000,1:10])
#plot10=pivot_longer(res10,cols=V1:V10, names_to='beta',values_to='Error') 
#ggplot(plot10,aes(x=beta,y=Error,color=type))+geom_boxplot()+theme_classic()

#p=20
#res20=sim_varguid(beta_real=rep(-1,p),nsim=nsim,p=p,r=r)
#p=50
#res50=sim_varguid(beta_real=rep(-1,p),nsim=nsim,p=p,r=r)


# par(mfrow = c(2, 2))
# boxplot(res5[[1]],res5[[2]],res5[[3]],ylim=c(0,0.1),
#         names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=5")
# boxplot(res10[[1]],res10[[2]],res10[[3]],ylim=c(0,0.15),
#         names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=10")
# boxplot(res20[[1]],res20[[2]],res20[[3]],ylim=c(0,0.2),
#         names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=20")
# boxplot(res20[[1]],res50[[2]],res50[[3]],ylim=c(0,0.3),
#         names=c("OLS in normal setting","OLS in gamma setting","varGuid"),ylab='MSE',main="p=50")
# #title("Rea")
# mtext("Real beta=-1, nsim=500, n=1000", side = 3, line = -2, outer = T)

par(mfrow = c(1, 2))
boxplot(res5[[1]],res5[[2]],
        names=c("OLS","varGuid"),ylab='MSE',main="p=5")
boxplot(res10[[1]],res10[[2]],
        names=c("OLS","varGuid"),ylab='MSE',main="p=10")
boxplot(res20[[1]],res20[[2]],ylim=c(0,0.2),
        names=c("OLS","varGuid"),ylab='MSE',main="p=20")
boxplot(res20[[1]],res50[[2]],ylim=c(0,0.3),
        names=c("OLS","varGuid"),ylab='MSE',main="p=50")
#title("Rea")
mtext("MSE of beta with comulticollinearity. Real beta=-1, nsim=500, n=1000", side = 3, line = -2, outer = T)

mean(res5$MSE1)
mean(res5$MSE2)

MSE_table=rbind(colMeans(as.data.frame(res5)),colMeans(as.data.frame(res10)),
                colMeans(as.data.frame(res20)),colMeans(as.data.frame(res50)))

rownames(MSE_table)=c("p=5","p=10","p=20","p=50")
MSE_table


# df=data.frame (
#  Beta = c("beta1", "beta2", "beta3","beta4","beta5"),
#   OLS = c(0.902,0.884,0.912,0.924,0.902),
#   Varguid = c(0.918,0.908,0.934,0.93,0.924)
# ) %>% print()

### get the bias 

