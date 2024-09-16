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

FLASH=readRDS("flash.data.1003.rds")

## the boxplot


### the correlation
