#source("./20240425/simulation/generate_function_simulation.R")
#source("./20240425/leash2.0.2.R")
#source("./20240425/VarGuid20240502.R")
library(glmnet)
library(tidyverse)
library(MASS)
library(xtable)
library(scales)
library(faux)
library(olsrr)

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

