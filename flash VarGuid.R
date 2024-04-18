library(tidyverse)
library(janitor)
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
library(caTools)

source("./20240412/VarGuid20240412.R")
source("./20240412/leash2.0.R")


flash <- readRDS("flash.rds")
dat=data.frame(MSV_exp= flash$MSV_exp,MSV_insp=flash$MSV_insp,VH_exp= flash$VH_exp,VH_insp=flash$VH_insp,
                       VDP_exp= flash$VDP_exp, VDP_insp=flash$VDP_insp, TV_exp= flash$TV_exp, TV_insp=flash$TV_insp,FEV1=flash$best_pre_fev1,
                       age=flash$age,bmi=flash$bmi_new,cat_score=flash$cat_score
) %>% drop_na()

predictor=dat[,1:11]


cat_res=lm(cat_score~.,data = dat)

plot(cat_res$fitted.values,cat_res$residuals^2) 
title("The sigma^2 estimation")

plot(cat_res$fitted.values,cat_res$residuals) 
title("Residual Variance")


### bootstrap data and split data into train and test

boot_num=10
resamples =lapply(1:boot_num, function(i) dat[sample(1:nrow(dat), size=nrow(dat),replace = T),]) 
resamples=do.call(rbind.data.frame, resamples)
resamples$id=seq(1,nrow(resamples),length.out=nrow(resamples))

sample <- sample.split(resamples$id, SplitRatio = 0.75)
train  <- subset(resamples, sample == TRUE)
test   <- subset(resamples, sample == FALSE)
train.x=train[,1:11]
train.y=train$cat_score
test.x=test[,1:11]
test.y=test$cat_score
######### in simulation data
#dat=readRDS("sim5.RDS")
#X=as.data.frame(dat[[1]][1])
#Y=unlist(dat[[1]][2])
n=1000
p=5;p2=5
gamma_real=rep(1,p)
beta_real=c(-1,-0.5,0,0.5,1)
sig=1
X=matrix(rnorm(n=n*p, mean =0, sd = 1),nrow=n) # small sd of X

Y= X %*% beta_real+ X^2 %*% gamma_real *rnorm(n,sd = sig)   # e with mean=0 and sd=1
res5=lmv(X, Y)

dat=as.data.frame(cbind(X,Y))
dat$id=seq(1,nrow(dat),length.out=nrow(dat))

sample <- sample.split(dat$id, SplitRatio = 0.75)
train  <- subset(dat, sample == TRUE)
test   <- subset(dat, sample == FALSE)



########


### for the catscore the fev1 
### test the prediction
#
# ols=lm(Y~.,dat=train[,-7])
# 
# msetrain_ols=mean(ols$residuals^2) 
# yhat_ols=fitted(ols)
# msetest_ols=mean((test$Y- yhat_ols)^2) 
o=lmv(X=apply(train.x,2,as.numeric),Y=train.y)

y.obj<- ymodv(o,gamma = c(seq(0,8.56, length.out=5)), phi = 0.45)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = o$obj.varGuid$model[1:5,2:(ncol(o$obj.varGuid$model)-1)]) ## what this 1:5 means?



### train error
colMeans((apply(yhat.varGuid,2,function(x) (x-train.y)^2)))

### test error
pred <- fnpred(mod=y.obj,lmvo = o,newdata = test.x)
rmse <- c()
rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(test.y,ncol(pred)),length(test.y))-pred)^2)) )
### test error
rmse
colMeans(as.data.frame(rmse))
rmse <- as.list(as.data.frame(rmse))


# msetrain_var=mean((train$Y- apply(cbind(1,train[,1:5]),2,as.numeric) %*% beta_cat)^2) 
# msetest_var=mean((test$Y- apply(cbind(1,test[,1:5]),2,as.numeric) %*% beta_cat)^2)
# 
# res=data.frame(Type=c("Varguid","OLS"),
#                   Train_MSE=c(msetrain_var,msetrain_ols),
#                   Test_MSE=c(msetest_var,msetest_ols))


##### for the sgrq, the MSV_
sgrq_res=lm(sgrq_score~.,data = dat[,-ncol(dat)])
plot(sgrq$fitted.values,sgrq$residuals^2) 
title("The sigma^2 estimation")

plot(sgrq$fitted.values,sgrq$residuals) 
title("Residual Variance")


