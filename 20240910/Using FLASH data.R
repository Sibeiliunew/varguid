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

flash <- read.csv("FLAS_REDCap_data_03-27-2024.csv")
lvd <- read.csv("resultLVD.csv")

flash <- merge(lvd,flash,by.x = "study_id", by.y="record_id")


dat=data.frame(scale(flash[,c("M.DF1","V.DF1")]),
                      age=flash$age,
                      #gender=factor(flash$gender-1),
                      #smoke=factor(ifelse(is.na(flash$smokingpacks), 0, 1)),
                      bmi=flash$bmi,
                      sgrq_score=flash$total_sgrq_score,
                      cat_score=flash$cat_score
) %>% janitor::clean_names()


#################
m2<- lm(cat_score~m_df1 , data = dat)
ols_test_breusch_pagan(m2)
ols_test_score(m2)
ols_test_f(m2)

summary(m2)
dat1=data.frame(X=dat$m_df1,Y=dat$sgrq_score)%>% drop_na()

m2.1<- lmv(X=dat1$X,Y=dat1$Y)
summary(m2.1$obj.varGuid)
rm(m2,m2.1,dat1)
###############
m2<- lm(sgrq_score~ v_df1, data = dat)
ols_test_breusch_pagan(m2)
ols_test_score(m2)
ols_test_f(m2)

summary(m2)
dat1=data.frame(X=dat$v_df1,Y=dat$sgrq_score)%>% drop_na()
m2.1<- lmv(X=dat1$X,Y=dat1$Y)
summary(m2.1$obj.varGuid)
rm(m2,m2.1,dat1)

###############
m2<- lm(sgrq_score~ age, data = dat)
ols_test_breusch_pagan(m2)
ols_test_score(m2)
ols_test_f(m2)

summary(m2)
dat1=data.frame(X=dat$age,Y=dat$sgrq_score)%>% drop_na()
m2.1<- lmv(X=dat1$X,Y=dat1$Y)
summary(m2.1$obj.varGuid)

rm(m2,m2.1,dat1)

###############
m2<- lm(cat_score~ bmi, data = dat)
ols_test_breusch_pagan(m2)
ols_test_score(m2)
ols_test_f(m2)

summary(m2)
dat1=data.frame(X=dat$bmi,Y=dat$cat_score)%>% drop_na()
m2.1<- lmv(X=dat1$X,Y=dat1$Y)
summary(m2.1$obj.varGuid)

rm(m2,m2.1,dat1)

#######################################################
#######################################################
#################
m2<- lm(cat_score~ m_df1+v_df1+age+bmi, data = dat)
ols_test_breusch_pagan(m2)
ols_test_score(m2)
ols_test_f(m2)

summary(m2)
dat1=dat%>% drop_na()

m2.1<- lmv(X=dat1[,1:4],Y=dat1[,6])
summary(m2.1$obj.varGuid)
rm(m2,m2.1,dat1)
###############
m2<- lm(sgrq_score~ m_df1+v_df1+age+bmi, data = dat)
ols_test_breusch_pagan(m2)
ols_test_score(m2)
ols_test_f(m2)

summary(m2)
dat1=dat%>% drop_na()
m2.1<- lmv(X=dat1[,1:4],Y=dat1[,5])
summary(m2.1$obj.varGuid)
rm(m2,m2.1,dat1)
#######################################################
#######################################################
#################
