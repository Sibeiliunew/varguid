library("sandwich") 
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
source("VarGuid20240403.R")

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

######### for the se


p=5;p2=5
sim_5=beta_ci=NULL

dat5=dat_sim(n,beta_real=c(-1,-0.5,0,0.5,1),sig = 1,gamma_real=rep(1,p))
res5=lmv(dat5$X, dat5$Y)
se=res5$se
var2=t(rbind(res5$beta-1.96*se,res5$beta+1.96*se))
for(d in 1:p){
beta_ci[[d]]=rbind(confint(res5$obj.OLS)[d+1,],
                   confint(res5$obj.varGuid)[d+1,],
                   var2[d+1,])}

sim_5[[1]]=dat5
for (i in 2:nsim){
dat5=dat_sim(n,beta_real=c(-1,-0.5,0,0.5,1),sig = 1,gamma_real=rep(1,p))
res5=lmv(dat5$X, dat5$Y)
se=res5$se
var2=t(rbind(res5$beta-1.96*se,res5$beta+1.96*se))

new=NULL
for( d in 1:p){
new[[d]]=rbind(confint(res5$obj.OLS)[d+1,],
                   confint(res5$obj.varGuid)[d+1,],
                   var2[d+1,])# the order: OLS, VAR,hand
}
# w: 1-5
for (w in 1:p){
beta_ci[[w]]=rbind(beta_ci[[w]],new[[w]])}

sim_5[[i]]=dat5
}

##### 1

beta1=as.data.frame(beta_ci[[1]]) %>% janitor::clean_names()

beta1$type=rep(c("OLS","Varguid by lm func","Varguid by hand"),nsim)
beta1$range=seq(from=-1.32, to=0.5,length.out=nrow(beta1))
beta1$sim=factor(rep(1:nsim, each=3))
beta1_use=beta1 %>% filter(type != "Varguid by hand")
OLS=beta1_use %>% filter(type=="OLS")
Var=beta1_use %>% filter(type=="Varguid by lm func")

1-length(union(which(OLS$x2_5_percent >-1), which(OLS$x97_5_percent < -1)))/nsim
1-length(union(which(Var$x2_5_percent >-1), which(Var$x97_5_percent < -1)))/nsim

ggplot(data=beta1_use[1:30,],aes(x=sim,y=range))+theme_classic()+geom_linerange(position = position_dodge(width = 1),aes(ymin=x2_5_percent, ymax=x97_5_percent,x=sim,color=type))+
  geom_hline(yintercept = -1,linetype="dashed")+labs(title = "Beta 1 CI in 15 simulations")+xlab("each simulation")

########### 2

beta5=as.data.frame(beta_ci[[5]]) %>% janitor::clean_names()

beta5$type=rep(c("OLS","Varguid by lm func","Varguid by hand"),nsim)
beta5$range=seq(from=0.7, to=1.3,length.out=nrow(beta4))
beta5$sim=factor(rep(1:nsim, each=3))
beta5_use=beta5 %>% filter(type != "Varguid by hand")

OLS=beta5_use %>% filter(type=="OLS")
Var=beta5_use %>% filter(type=="Varguid by lm func")

1-length(union(which(OLS$x2_5_percent >1), which(OLS$x97_5_percent < 1)))/nsim
1-length(union(which(Var$x2_5_percent >1), which(Var$x97_5_percent < 1)))/nsim

ggplot(data=beta5_use[1:30,],aes(x=sim,y=range))+theme_classic()+geom_linerange(position = position_dodge(width = 1),aes(ymin=x2_5_percent, ymax=x97_5_percent,x=sim,color=type))+
  geom_hline(yintercept = 1,linetype="dashed")+labs(title = "Beta 5 CI in 15 simulations")+xlab("each simulation")

df=data.frame (
 Beta = c("beta1", "beta2", "beta3","beta4","beta5"),
  OLS = c(0.894,0.902,0.882,0.904,0.91),
  Varguid = c(0.908,0.92,0.906,0.92,0.928)
) 


