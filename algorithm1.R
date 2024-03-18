
###
p=5;n=1000;r1=rep(1,p);r2=rep(0.3,p);sig=1# real value
p2=5

tol=exp(-10)
dat_sim=function(n,p,p2,r1,sig){
  X=matrix(rnorm(n=n*p, mean =0, sd = .2),nrow=n) # small sd of X
  e=rnorm(n,sd = sig)
  Y1=X %*% r1+e  
  X2=X[,1:p2]
  Y2= X %*% r1+ X2%*% r2 *e  
  return(list(X=X, Y=Y2,X2=X2))
}

data=dat_sim(n,p,p2,r1,sig = sig)
X=data[[1]]
Y=data[[2]]
X2=data[[3]]
df=data.frame(X,Y=Y)
######################################################## from Dr. Lu's pdf

beta_est2=function(gamma){
  W=NULL
  for (i in 1:n){
    new=1/abs((t(X[i,])%*% gamma))
    W=c(W,new)
  }
  W=diag(W)
  beta=solve(t(X) %*% t(W)%*%W%*%X) %*%t(X)%*% t(W)%*%W %*%Y
  return(beta)
}


sig_est=function(beta,gamma){
  sig=0
  for (i in 1:n){
    numerator=(Y[i]-c(crossprod(X[i,],beta)))^2
    denumerator=(t(X[i,]) %*% gamma)^2
    sig=sig+numerator/denumerator
  }
  return(sig/(n-2*p))
}

gamma_est1=function(beta,sig){
  gamma=(solve(t(X)%*% X) %*% t(X) %*% abs(Y-X%*% beta ))
  gamma=abs(gamma)/as.numeric(sig)
  return(gamma)
}

#gamma0=rep(0.2,p) # initial
beta=coef(lm(Y~.-1,data=df))
sig = summary(lm(Y~.-1,data=df))$sigma
sum(abs(beta-rep(1,5))) # the results from OLS

gamma=gamma_est1(beta,sig=sig) #rep(0.3,5)#
diff1=diff2=10

tol = 0.01
while ( diff1 >tol & diff2 >tol ) {
  old_beta=beta
  old_gamma=gamma
  beta=beta_est2(gamma) 
  sig=sqrt(sig_est(beta,gamma))
  gamma=gamma_est1(beta,sig=sig) 
  diff1=sum((beta-old_beta)^2)
  diff2=sum((gamma-old_gamma)^2)
}
beta
sum(abs(beta-rep(1,5)))
gamma
sig

####################################################################################
####################################################################################
########### from Sibei calculation
gamma_est2=function(beta,sig){
  gamma_square=(1/sig^2) %*% solve(t(X)%*% X) %*% (Y-X%*% beta)^2
  return(sqrt(gamma_square))
}

gamma0=rep(0.7,p) # initial
beta=beta_est2(gamma0)
gamma=gamma_est2(beta,sig=1)
diff1=diff2=10

while ( diff1 >tol & diff1 >tol ) {
  old_beta=beta
  old_gamma=gamma
  beta=beta_est2(gamma) 
  gamma=gamma_est2(beta,sig=1) 
  diff1=sum((beta-old_beta)^2)
  diff2=sum((gamma-old_gamma)^2)
}
beta
gamma
sig=sqrt(sig_est(beta,gamma))
sig


