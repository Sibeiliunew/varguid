source("./20240425/simulation/generate_function_simulation.R")
source("./20240425/leash2.0.2.R")
source("./20240425/leash2.0.2.R")
####################################################################
###   
###  Simulation data: simulate 10 datasets from the generate_function_simulation.R
### there are 12 simulations there: we don't use "lm" and "lmi"
###   n = 500, d = 15 for low p case
###   n = 100, d = 200 for high p case
####################################################################
## data settings for low p
n <- 500
d <- 15
corrv <- c(0, .9)[2]


simnames <- c("cobra2",    "cobra8",    "friedman1", "friedman3", "inx1",      
              "inx2", "inx3",     # "lm",        "lmi",       
              "lmi2",      
              "sup",       "sup2")   

# you can use a number or a name to generate a specific simulation
#simo <- simulation.sum[[1]](n=n, d=d, corrv = corrv)
## another way
#simo <- simulation.sum[[simnames[i]]](n=n, d=d, corrv = corrv)$dta

rmse5 <- c()
rmse_res5=NULL

for (c in 1:10){
  for( i in 1:10){
  print(i)
  simo <- simulation.sum[[simnames[c]]](n=n, d=d, corrv = corrv)$dta
  trn <- sample.split(1:n, SplitRatio = 0.75)
  train  <- subset(simo, trn==TRUE)
  test   <- subset(simo, trn==FALSE)
  
  data=list(x.train = makeX(train[,1:(ncol(simo)-1)]),
            y.train = train[,ncol(simo)],
            x.test = makeX(test[,1:(ncol(simo)-1)]),
            y.test = test[,ncol(simo)])
  
  o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = FALSE) # , lasso = TRUE
  y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
  
  
  pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
  
  rmse5 <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res5[[c]]=colMeans(as.data.frame(rmse5))
}

table5=do.call("rbind",rmse_res5)
table5

##### larger p

n <- 100
d <- 200
corrv <- c(0, .9)[2]
rmse6 <- c()
rmse_res6=NULL

for (c in 1:10){
  for( i in 1:10){
    print(i)
    simo <- simulation.sum[[simnames[c]]](n=n, d=d, corrv = corrv)$dta
    trn <- sample.split(1:n, SplitRatio = 0.75)
    train  <- subset(simo, trn==TRUE)
    test   <- subset(simo, trn==FALSE)
    data=list(x.train = makeX(train[,1:(ncol(simo)-1)]),
              y.train = train[,ncol(simo)],
              x.test = makeX(test[,1:(ncol(simo)-1)]),
              y.test = test[,ncol(simo)])
    
    o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = TRUE) # , lasso = TRUE
    y.obj <-ymodv(obj = o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
    
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse6 <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res6[[c]]=colMeans(as.data.frame(rmse6))
}

table6=do.call("rbind",rmse_res6)
table6


#### for larger p real data
#####

library(datamicroarray)
data('subramanian', package = 'datamicroarray')
dat=subramanian

## use lasso to find the outcome: the continuous outcome will be the most important gene for 
## predicting the classification outcome

library(glmnet)
#summary(dat$x[,1:10])
dat$x <- scale(dat$x)
dat$x <- makeX(as.data.frame(dat$x))
cv_model <- cv.glmnet(dat$x,dat$y , family = "binomial") # binomial

best_lambda <- cv_model$lambda.min
o <- glmnet(dat$x,dat$y , alpha = 1, lambda = best_lambda, family = "binomial")
yindex <- which.max(abs(as.vector(o$beta)))

c=10
realDat[[c]] <- data.frame(y = dat$x[,yindex ],dat$x[,-yindex ])
names(realDat)[c] <- 'subramanian'


##############

#corrv <- c(0, .9)[1]
#rmse4 <- c()
#rmse_res4=NULL

for (c in 1:10){
  real <- realDat[[c]]
  folds=createFolds(1:nrow(real), k = 10)
  for( i in 1:10){
    print(i)
    #trn <- sample.split(1:nrow(real), SplitRatio = 0.75)
    train  <- real[-folds[[i]],]
    test   <- real[folds[[i]],]
    data=list(x.train = makeX(train[,1:(ncol(real)-1)]),
              y.train = train[,ncol(real)],
              x.test = makeX(test[,1:(ncol(real)-1)]),
              y.test = test[,ncol(real)])
    
    o <- lmv(X =as.matrix(data$x.train) , Y = unlist(data$y.train), lasso = TRUE) # , lasso = TRUE
    y.obj <-ymodv(o,gamma = c(seq(0,9, length.out=5)), phi = 0.46)#, rf = FALSE)
    
    
    pred <- fnpred(mod=y.obj,lmvo = o,newdata = data$x.test)
    
    rmse4 <- rbind(rmse,sqrt(colMeans((matrix(rep(data$y.test,ncol(pred)),length(data$y.test))-pred)^2)) )
  }
  rmse_res4[[c]]=colMeans(as.data.frame(rmse4))
}

table4=do.call("rbind",rmse_res4)
table4



