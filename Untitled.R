p=5;n=5000;r1=rep(2,p);r2=c(2,-2)
# real value

tol=exp(-10)
dat_sim=function(n,p,r1){
  X=matrix(rnorm(n=n*p, mean =2, sd = .2),nrow=n) # small sd of X
  e=rnorm(n,sd = .5)
  Y1=X %*% r1+e  
  X2=X[,1:2]
  Y2= X %*% r1+ X2%*% r2 *e  
  return(list(X=X, Y=Y2,X2=X2))
}

data=dat_sim(n,p,r1)
X=data[[1]]
Y=data[[2]]
X2=data[[3]]
df=cbind(X,X2,Y)

beta1_est=function(beta2){
  p1=p2=0;
  for(i in 1:n){  p1=p1+X[i,] %*% t(X[i,])/((X2[i,1] * beta2[1] + X2[i,2]*beta2[2])^2)
  p2= p2+ Y[i]*X[i,] / ((X2[i,1] * beta2[1] + X2[i,2]*beta2[2])^2) }
  beta1=solve(p1) %*% p2
  return(beta1)
}
beta1_est(r2)## enter the real value to check, should return all 1


score=function(beta2){
  s1=s2=0
  for (i in 1:n ) {
    s1=s1+X2[i,]/c(crossprod(X2[i,],beta2))
    s2=s2+ X2[i,]/(c(crossprod(X2[i,],beta2))^3) *(Y[i]-c(crossprod(X[i,],beta1)))^2
  }
  return((s1-s2)/n)                               
}

guess=r2# initial guess of r2
beta1=beta1_est(guess)
beta2=multiroot(score,guess)$root
old=c(rep(2,p),guess)
diff=crossprod(c(beta1,beta2)-old)

##above run the first time
## below do the loop
while ( diff >tol) {
  old=c(beta1,beta2)# use the first loop beta1 and beta2 to have the old version
  beta1=beta1_est(beta2) # update beta1
  beta2=multiroot(score,beta2)$root # 
  diff=sum((c(beta1,beta2)-old)^2)
}

beta2

# ### do the bootstrap 
# 
# # resample=function(dat){
# #   ind=sample(1:n, size=n,replace = T)
# #   return(dat[ind,])
# # }
# boot_num=100
# resamples =lapply(1:boot_num, function(i) df[sample(1:n, size=n,replace = T),])
# 
# res_beta1=res_beta2=NULL
# 
# for ( i in 1:boot_num){
#   beta1=beta2=NULL
#   data=resamples[[i]]
#   X=data[,1:p]
#   X2=data[,c(p+1,p+2)]
#   Y=data[,p+3]
#   
#   guess=c(0,2)# initial guess of r2
#   beta1=beta1_est(guess)
#   beta2=multiroot(score,guess)$root
#   old=c(rep(2,p),guess)
#   diff=crossprod(c(beta1,beta2)-old)
#   
#   ##above run the first time
#   ## below do the loop
#   while ( diff >tol) {
#     old=c(beta1,beta2)# use the first loop beta1 and beta2 to have the old version
#     beta1=beta1_est(beta2) # update beta1
#     beta2=multiroot(score,beta2)$root #  may change the setting?
#     diff=sum((c(beta1,beta2)-old)^2)
#   }
#   res_beta1=cbind(res_beta1,beta1)
#   res_beta2=cbind(res_beta2,beta2)
# }
# 
# apply(res_beta1,1,mean)
# apply(res_beta2,1,mean)
# 
# 
