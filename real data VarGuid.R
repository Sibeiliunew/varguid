library(tidyverse)
library(janitor)
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
library(caTools)

source("./VarGuid20240418.R")
source("./leash2.2.R")


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
####

o=lmv(X=apply(train.x,2,as.numeric),Y=train.y)

y.obj<- ymodv(o,gamma = c(seq(0,8.56, length.out=5)), phi = 0.45)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = o$obj.varGuid$model[1:5,2:(ncol(o$obj.varGuid$model)-1)]) 


### test error
pred <- fnpred(mod=y.obj,lmvo = o,newdata = test.x)
rmse <- c()
rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(test.y,ncol(pred)),length(test.y))-pred)^2)) )
### test error
rmse

##### UCI data
### 1. 
real=readxl::read_xls("../concrete+compressive+strength/Concrete_Data.xls") %>% janitor::clean_names()

sample <- sample.split(real$cement_component_1_kg_in_a_m_3_mixture, SplitRatio = 0.75)
train  <- subset(real, sample == TRUE)
test   <- subset(real, sample == FALSE)
train.x=train[,1:8]
train.y=unlist(train[,9])
test.x=test[,1:8]
test.y=unlist(test[,9])
####

o=lmv(X=apply(train.x,2,as.numeric),Y=train.y)

y.obj<- ymodv(o,gamma = c(seq(0,8.56, length.out=5)), phi = 0.45)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = o$obj.varGuid$model[1:5,2:(ncol(o$obj.varGuid$model)-1)]) 


### test error
pred <- fnpred(mod=y.obj,lmvo = o,newdata = test.x)
rmse <- c()
rmse <- rbind(rmse,sqrt(colMeans((matrix(rep(test.y,ncol(pred)),length(test.y))-pred)^2)) )
### test error
rmse


