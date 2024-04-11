library(tidyverse)
library(janitor)
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
library(caTools)
source("VarGuid20240406.R")

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



### for the catscore the fev1 
### test the prediction
beta_cat=lmv(X=apply(train[,1:11],2,as.numeric),Y=train$cat_score)$beta

msetrain_ols=mean(cat_res$residuals^2) 
msetest_ols=mean((test$cat_score- apply(cbind(1,test[,1:11]),2,as.numeric) %*% cat_res$coefficients)^2) 

msetrain_var=mean((train$cat_score- apply(cbind(1,train[,1:11]),2,as.numeric) %*% beta_cat)^2) 
msetest_var=mean((test$cat_score- apply(cbind(1,test[,1:11]),2,as.numeric) %*% beta_cat)^2)

res=data.frame(Type=c("Varguid","OLS"),
                  Train_MSE=c(msetrain_var,msetrain_ols),
                  Test_MSE=c(msetest_var,msetest_ols))


##### for the sgrq, the MSV_
sgrq_res=lm(sgrq_score~.,data = dat[,-ncol(dat)])
plot(sgrq$fitted.values,sgrq$residuals^2) 
title("The sigma^2 estimation")

plot(sgrq$fitted.values,sgrq$residuals) 
title("Residual Variance")


