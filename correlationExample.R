source("VarGuid20240626.R")
source("leash2.0.6.R")

o=lmv(X=mtcars[-c(1:10),1],Y=mtcars[-c(1:10),2],lasso = FALSE)

y.obj<- ymodv(obj = o,gamma = c(seq(0,8.56, length.out=5)), phi = 0.45)

yhat.varGuid <- fnpred(mod=y.obj,
                       lmvo = o,
                       newdata = mtcars[c(1:10),1]) 


