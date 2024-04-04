library(tidyverse)
library(janitor)
library("tidyverse") 
library(MASS)
library(xtable)
library(scales)
source("VarGuid20240403.R")

flash <- readRDS("flash.rds")
dat=data.frame(MSV_exp= flash$MSV_exp,MSV_insp=flash$MSV_insp,VH_exp= flash$VH_exp,VH_insp=flash$VH_insp,
                       VDP_exp= flash$VDP_exp, VDP_insp=flash$VDP_insp, TV_exp= flash$TV_exp, TV_insp=flash$TV_insp,FEV1=flash$best_pre_fev1,
                       age=flash$age,bmi=flash$bmi_new,cat_score=flash$cat_score,sgrq_score=flash$sgrq_score
) %>% drop_na()

predictor=dat[,1:11]


cat_res=lm(cat_score~.,data = dat[,-ncol(dat)])
plot(cat_res$fitted.values,cat_res$residuals^2) 
title("The sigma^2 estimation")

plot(cat_res$fitted.values,cat_res$residuals) 
title("Residual Variance")

### for the catscore the fev1 
### test the prediction
beta_cat=lmv(X=predictor,Y=dat$cat_score)$beta
MSE_catvarguid=mean((dat$cat_score- apply(cbind(1,predictor),2,as.numeric) %*% beta_cat)^2)%>% print()

MSE_catOLS=mean(cat_res$residuals^2) %>% print()


##### for the sgrq, the MSV_
sgrq_res=lm(sgrq_score~.,data = dat[,-ncol(dat)])
plot(sgrq$fitted.values,sgrq$residuals^2) 
title("The sigma^2 estimation")

plot(sgrq$fitted.values,sgrq$residuals) 
title("Residual Variance")


