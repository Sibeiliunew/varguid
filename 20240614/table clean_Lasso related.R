

yhat(dat = readRDS("./20240601 simulated data/sce5_20with15with0.rds"),test=test_sce5_20with15with0,lasso_status=TRUE)
yhat(dat = readRDS("./20240601 simulated data/sce5_20with15with0.9.rds"),test=test_sce5_20with15with0.9,lasso_status=TRUE)
yhat(dat = readRDS("./20240601 simulated data/sce5_200with15with0.rds"),test=test_sce5_200with15with0,lasso_status=TRUE)
yhat(dat = readRDS("./20240601 simulated data/sce5_200with15with0.9.rds"),test=test_sce5_200with15with0.9,lasso_status=TRUE)

#############################################
#############################################
yhat(dat = readRDS("./sce6_20with20with0.rds"),test=dat_sim(n=20,p=20,
                                                                 beta_real=c(rep(1,5),rep(0,15)),
                                                                 gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce6_20with20with0.9.rds"),test=dat_sim(n=20,p=20,
                                                              beta_real=c(rep(1,5),rep(0,15)),
                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9),lasso_status=TRUE)
yhat(dat = readRDS("./sce6_200with20with0.rds"),test=dat_sim(n=20,p=20,
                                                             beta_real=c(rep(1,5),rep(0,15)),
                                                             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce6_200with20with0.9.rds"),test=dat_sim(n=20,p=20,
                                                               beta_real=c(rep(1,5),rep(0,15)),
                                                               gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9),lasso_status=TRUE)
#############################################
#############################################
yhat(dat = readRDS("./sce7_20with100with0.rds"),test=dat_sim(n=20,p=100,
                                                            beta_real=c(rep(1,5),rep(0,95)),
                                                            gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce7_20with100with0.9.rds"),test=dat_sim(n=20,p=100,
                                                              beta_real=c(rep(1,5),rep(0,95)),
                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9),lasso_status=TRUE)
yhat(dat = readRDS("./sce7_200with100with0.rds"),test=dat_sim(n=20,p=100,
                                                             beta_real=c(rep(1,5),rep(0,95)),
                                                             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce7_200with100with0.9.rds"),test=dat_sim(n=20,p=100,
                                                               beta_real=c(rep(1,5),rep(0,95)),
                                                               gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9),lasso_status=TRUE)

#############################################
#############################################
yhat(dat = readRDS("./sce8_20with100with0.rds"),test=dat_sim(n=20,p=200,
                                                             beta_real=c(rep(1,5),rep(0,195)),
                                                             gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce8_20with100with0.9.rds"),test=dat_sim(n=20,p=200,
                                                               beta_real=c(rep(1,5),rep(0,195)),
                                                               gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9),lasso_status=TRUE)
yhat(dat = readRDS("./sce8_200with100with0.rds"),test=dat_sim(n=20,p=200,
                                                              beta_real=c(rep(1,5),rep(0,195)),
                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0),lasso_status=TRUE)
yhat(dat = readRDS("./sce8_200with100with0.9.rds"),test=dat_sim(n=20,p=200,
                                                                beta_real=c(rep(1,5),rep(0,195)),
                                                                gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9),lasso_status=TRUE)
#############################################
#############################################
# yhat(dat = readRDS("./sce9_20with400with0.rds"),test=dat_sim(n=20,p=400,
#                                                              beta_real=c(rep(1,5),rep(0,395)),
#                                                              gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0),lasso_status=TRUE)
# yhat(dat = readRDS("./sce9_20with400with0.9.rds"),test=dat_sim(n=20,p=400,
#                                                                beta_real=c(rep(1,5),rep(0,395)),
#                                                                gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9),lasso_status=TRUE)
# yhat(dat = readRDS("./sce9_200with400with0.rds"),test=dat_sim(n=20,p=400,
#                                                               beta_real=c(rep(1,5),rep(0,395)),
#                                                               gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0),lasso_status=TRUE)
# yhat(dat = readRDS("./sce9_200with400with0.9.rds"),test=dat_sim(n=20,p=400,
#                                                                 beta_real=c(rep(1,5),rep(0,395)),
#                                                                 gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9),lasso_status=TRUE)



#############################################
sim_varguid2=function(n,p,beta_real,gamma_real,corrv,name){
  beta_var=beta_lasso=vector(mode='list', p)
  dat=vector(mode='list', nsim)
  for (i in 1:nsim) {
    sim=dat_sim(n,p,beta_real,gamma_real,corrv) 
    dat[[i]]=cbind(sim[[1]],sim[[2]])
    #m2=lm(Y~.,data=data.frame(X=sim[[1]],Y=sim[[2]])) # LASSO
    #cvfit <- cv.glmnet(x=sim[[1]], y=sim[[2]])
    #holder=coef(cvfit, s = "lambda.min")[-1]
    #holder=coef(m2)[-1]
  
    v2=lmv(X=sim[[1]],Y=sim[[2]],lasso = TRUE)
    beta2=v2$beta
    holder2=beta2[-1]
    holder=coef(v2$obj.lasso)[-1]
    
    for(d in 1:p){
      beta_lasso[[d]]=c(beta_lasso[[d]],holder[d])
      beta_var[[d]]=c(beta_var[[d]],holder2[d])
    }
  }
  saveRDS(dat,paste(eval(name),"_",eval(parse(text=n)),"with",eval(parse(text=p)),"with",eval(parse(text=corrv)),".rds",sep=""))
  
  beta_lasso=as.data.frame(do.call(cbind,beta_lasso))
  beta_var=as.data.frame(do.call(cbind,beta_var))
  beta_lasso$type=rep("LASSO",nrow(beta_lasso))
  beta_var$type=rep("Varguid_lasso",nrow(beta_var))
  res=rbind(beta_lasso,beta_var)
  
  return(list(res=res))
}

sce6_1=sim_varguid2(n=20,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0,name="sce6") 
 round(mean(sce6_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
 round(mean(sce6_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
 round(mean(sce6_1$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
 round(mean(sce6_1$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0


sce6_2=sim_varguid2(n=20,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9,name="sce6") 
 round(mean(sce6_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
 round(mean(sce6_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
 round(mean(sce6_2$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
 round(mean(sce6_2$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0



sce6_3=sim_varguid2(n=200,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0,name="sce6") 
round(mean(sce6_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce6_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce6_3$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce6_3$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0


sce6_4=sim_varguid2(n=200,p=20,beta_real=c(rep(1,5),rep(0,15)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,10)),corrv=0.9,name="sce6") 
round(mean(sce6_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce6_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce6_4$res[1:1000,6:20] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce6_4$res[1001:2000,6:20] %>% apply(2,MSE0)),3) # var beta=0
##############
sce7_1=sim_varguid2(n=20,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0,name="sce7")
round(mean(sce7_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_1$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_1$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0

sce7_2=sim_varguid2(n=20,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9,name="sce7") 
round(mean(sce7_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_2$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_2$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0

sce7_3=sim_varguid2(n=200,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0,name="sce7") 

round(mean(sce7_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_3$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_3$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0


sce7_4=sim_varguid2(n=200,p=100,beta_real=c(rep(1,5),rep(0,95)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,90)),corrv=0.9,name="sce7") 
round(mean(sce7_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce7_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce7_4$res[1:1000,6:100] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce7_4$res[1001:2000,6:100] %>% apply(2,MSE0)),3) # var beta=0

##############
##############
sce8_1=sim_varguid2(n=20,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0,name="sce8")
round(mean(sce8_1$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_1$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_1$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_1$res[1001:2000,2:100] %>% apply(2,MSE0)),3) # var beta=0
sce8_2=sim_varguid2(n=20,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9,name="sce8") 

round(mean(sce8_2$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_2$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_2$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_2$res[1001:2000,2:100] %>% apply(2,MSE0)),3) # var beta=0


sce8_3=sim_varguid2(n=200,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0,name="sce8") 
round(mean(sce8_3$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_3$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_3$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_3$res[1001:2000,2:100] %>% apply(2,MSE0)),3) # var beta=0

sce8_4=sim_varguid2(n=200,p=200,beta_real=c(rep(1,5),rep(0,195)),
                    gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,190)),corrv=0.9,name="sce8") 
round(mean(sce8_4$res[1:1000,1:5] %>% apply(2,MSE1)),3) #LASSO beta=1
round(mean(sce8_4$res[1001:2000,1:5] %>% apply(2,MSE1)),3) # varguid beta=1
round(mean(sce8_4$res[1:1000,6:200] %>% apply(2,MSE0)),3) # LASSO beta=0
round(mean(sce8_4$res[1001:2000,2:100] %>% apply(2,MSE0)),3) # var beta=0
##################
# sce9_1=sim_varguid2(n=20,p=400,beta_real=c(rep(1,5),rep(0,395)),
#                     gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0,name="sce9")
# sce9_2=sim_varguid2(n=20,p=400,beta_real=c(rep(1,5),rep(0,390)),
#                     gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9,name="sce9") 
# 
# sce9_3=sim_varguid2(n=200,p=400,beta_real=c(rep(1,5),rep(0,395)),
#                     gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0,name="sce9") 
# 
# sce9_4=sim_varguid2(n=200,p=400,beta_real=c(rep(1,5),rep(0,395)),
#                     gamma_real=c(rep(0,4),c(0,1,2,3,4,5),rep(0,390)),corrv=0.9,name="sce9") 