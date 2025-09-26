#######################################
#     Functions for estimation #
#######################################

expit <- function(x){
  1/(exp(-x)+1)
}

##### transferable + partial transferable###
data.generate<-function(cov.type=2,
                        M=5, # number of source
                        Ah=1, #number of similar source data
                        h=20,
                        n.target=500, 
                        n.test=200,
                        n.source=rep(300, M),
                        p=50,
                        Gamma=-1,
                        Corr=0.3,
                        supp=30, # support if each class
                        sig.strength=0.5){
  si<-sample(c(-1,1), supp, replace = TRUE)
  theta <- c(rep(sig.strength,supp)*si, rep(0,p-supp-1))  
  
  
  Target<-list(y=NULL,x=NULL)
  Test<-list(y=NULL,x=NULL)
  
  if(cov.type==1){
    sigmax<-diag(rep(1,p-1))
  }else if(cov.type==2){
    sigmax<-outer(1:(p-1), 1:(p-1), function(x, y) {
      Corr^(abs(x - y))})
  }else if(cov.type==3){
    sigmax  <- matrix(corr, p-1, p-1) + diag(1-corr, p-1)
  }
  X  <- rmvnorm(n.target, rep(0,p-1), sigmax)
  meanTreat <- expit(Gamma+X%*%theta)
  Treat <- rbinom(n.target, 1, meanTreat)
  Target$x<- X
  Target$y<-Treat
  
  X  <- rmvnorm(n.test, rep(0,p-1), sigmax)
  meanTreat <- expit(Gamma+X%*%theta)
  Treat <- rbinom(n.test, 1, meanTreat)
  Test$x<- X
  Test$y<-Treat
  
  Source<- sapply(1:M, function(m) {
    if(m>Ah){
      index<-c(1:(2*supp), sample((2*supp+1):(p-1),supp))  
      thetaM<-theta
      thetaM[index] <-thetaM[index]+ h/length(index) * sample(c(-1, 1), size = length(index), replace = TRUE)
      index1<-c((supp+1):(2*supp), sample((2*supp+1):(p-1),2*supp))
      thetaM[index1]<-sig.strength+thetaM[index1]
    }else{
      index<-c(1:(2*supp), sample((2*supp+1):(p-1),supp))
        thetaM<-theta
      thetaM[index] <-thetaM[index]+ h/length(index) * sample(c(-1, 1), size = length(index), replace = TRUE)
    }
    X<- rmvnorm(n.source[m], rep(0,p-1), sigmax)
    meanTreat <- expit(Gamma+X%*%thetaM)
    Treat <- rbinom(n.source[m], 1, meanTreat)
    list(x=X,y=Treat)
  },simplify = FALSE)
  
  return(list(Target=Target, Source=Source, Test=Test, theta=c(Gamma,theta)))    
}



#####  partial transferable+ non-transferable ###
data.generate1<-function(cov.type=2,
                        M=5, # number of source
                        Ah=1, #number of similar source data
                        h=20,
                        n.target=500, 
                        n.test=200,
                        n.source=rep(300, M),
                        p=50,
                        Gamma=-1,
                        Corr=0.3,
                        supp=30, # support if each class
                        sig.strength=0.5){
  si<-sample(c(-1,1), supp, replace = TRUE)
  theta <- c(rep(sig.strength,supp)*si, rep(0,p-supp-1))  
  
  
  Target<-list(y=NULL,x=NULL)
  Test<-list(y=NULL,x=NULL)
  
  if(cov.type==1){
    sigmax<-diag(rep(1,p-1))
  }else if(cov.type==2){
    sigmax<-outer(1:(p-1), 1:(p-1), function(x, y) {
      Corr^(abs(x - y))})
  }else if(cov.type==3){
    sigmax  <- matrix(corr, p-1, p-1) + diag(1-corr, p-1)
  }
  X  <- rmvnorm(n.target, rep(0,p-1), sigmax)
  meanTreat <- expit(Gamma+X%*%theta)
  Treat <- rbinom(n.target, 1, meanTreat)
  Target$x<- X
  Target$y<-Treat
  
  X  <- rmvnorm(n.test, rep(0,p-1), sigmax)
  meanTreat <- expit(Gamma+X%*%theta)
  Treat <- rbinom(n.test, 1, meanTreat)
  Test$x<- X
  Test$y<-Treat
  
  Source<- sapply(1:M, function(m) {
    if(m>Ah){
      index<-c(1:(2*supp), sample((2*supp+1):(p-1),supp))  
      thetaM<-rep(0,p-1)
      thetaM[index] <-thetaM[index]+ h/length(index) * sample(c(-1, 1), size = length(index), replace = TRUE)
      index1<-c((supp+1):(2*supp), sample((2*supp+1):(p-1),2*supp))
      thetaM[index1]<-sig.strength+thetaM[index1]
    }else{
      index<-c(1:(2*supp), sample((2*supp+1):(p-1),supp))  
      thetaM<-theta
      thetaM[index] <-thetaM[index]+ h/length(index) * sample(c(-1, 1), size = length(index), replace = TRUE)
      index1<-c((supp+1):(2*supp), sample((2*supp+1):(p-1),2*supp))
      thetaM[index1]<-sig.strength+thetaM[index1]
    }
    X<- rmvnorm(n.source[m], rep(0,p-1), sigmax)
    meanTreat <- expit(Gamma+X%*%thetaM)
    Treat <- rbinom(n.source[m], 1, meanTreat)
    list(x=X,y=Treat)
  },simplify = FALSE)
  
  return(list(Target=Target, Source=Source, Test=Test, theta=c(Gamma,theta)))    
}




split.data<-function(Target){
  n<-length(Target$y)
  
  index<-sample(c(1:n), floor(n/2))
  
  Target.s<-Target.e<-list(y=NULL,x=NULL,siteID=NULL)
  Target.s$y<-Target$y[index]
  Target.s$siteID<-Target$siteID[index]
  Target.s$x<-Target$x[index,]
  
  Target.e$y<-Target$y[-index]
  Target.e$siteID<-Target$siteID[-index]
  Target.e$x<-Target$x[-index,]
  
  return(list(Target.s=Target.s,Target.e=Target.e))
}

compare.metric<-function(theta,theta.true,Test){
  mse<-mean((theta-theta.true)^2)
  x<-Test$x
  y<-Test$y
  pred<-expit(theta[1]+x%*%theta[-1])
  CE<-sum(I(pred>0.5)*I(y==0)+I(pred<=0.5)*I(y==1))/length(y)
  auc<-roc(y, pred)$auc[1]
  return(c(mse,auc,CE))
}

compare.metric.tpr<-function(theta,theta.true,Test){
  mse<-mean((theta-theta.true)^2)
  x<-Test$x
  y<-Test$y
  pred<-expit(theta[1]+x%*%theta[-1])
  CE<-sum(I(pred>0.5)*I(y==0)+I(pred<=0.5)*I(y==1))/length(y)
  auc<-roc(y, pred)$auc[1]
  tpr<-sum(I(theta[-1]!=0)*I(theta.true[-1]!=0))/sum(I(theta.true[-1]!=0))
  fpr<-sum(I(theta[-1]!=0)*I(theta.true[-1]==0))/sum(I(theta.true[-1]==0))
  return(c(mse,auc,CE,tpr,fpr))
}


local_es<-function(Target,family=c("binomial")){
  
  y.t<-Target$y
  x.t<-Target$x
  fit0 <- cv.glmnet(x.t, y.t, family = family)
  lambda<-fit0$lambda.min
  fit0 <- glmnet(x.t, y.t,  family = family,lambda=lambda)
  beta<-as.vector(coef(fit0))
  return(beta)
}  

local_es_multi<-function(Target,family=c("binomial"),sca=c(0.25,0.5,1,2,4)){
  
  y.t<-Target$y
  x.t<-Target$x
  fit0 <- cv.glmnet(x.t, y.t, family = family)
  lambda<-fit0$lambda.min
  beta<-NULL
  for (a in sca) {
    fit0 <- glmnet(x.t, y.t,  family = family,lambda=a*lambda)
    beta<-rbind(beta,as.vector(coef(fit0)))
  }
  return(beta)
}  


pool_TL1 <- function(Target,
                     Source,
                     family=c("binomial"),
                     support){
  y.t<-Target$y
  x.t<-Target$x
  p<-ncol(x.t)+1
  
  M<-length(Source)
  
  y.s<-x.s<-NULL
  offset.all<-rep(0,length(y.t))
  
  for (m in 1:M) {
    source.m<-Source[[m]] 
    x.sm<-source.m$x
    y.sm<-source.m$y
    y.s<-c(y.s,y.sm)
    x.s<-rbind(x.s,x.sm)
    
    fit0 <- cv.glmnet(x.sm, y.sm,  family = family)
    lambda<-fit0$lambda.min
    fit0 <- glmnet(x.sm, y.sm,  family = family,lambda=lambda)
    beta<-as.vector(coef(fit0))
    beta.c<-numeric(p)
    beta.c[-c(support+1)]<-beta[-c(support+1)]
    
    offset<-x.t[,support]%*%beta[support+1]
    fit0 <- cv.glmnet(x.t[,support], y.t,  family = family,offset = offset)
    lambda<-fit0$lambda.min
    fit0 <- glmnet(x.t[,support], y.t,  family = family,lambda=lambda, offset = offset)
    delta.m<-beta.c
    delta.m[c(support+1)]<- -as.vector(coef(fit0))[-1]
    offset.all<-c(offset.all,delta.m[1]+x.sm%*%delta.m[-1])
  }
  
  x.all<-rbind(x.t,x.s)
  y.all<-c(y.t,y.s)
  
  fit0 <- cv.glmnet(x.all, y.all,  family = family, offset = offset.all)

  beta.se<-coef(fit0, s = "lambda.1se")
  beta.min<-coef(fit0, s = "lambda.min")
  
  # fit0 <- glmnet(x.all, y.all,  family = family, offset = offset.all)
  # beta.se<-as.vector(coef(fit0))
  # 
  # fit0 <- glmnet(x.all, y.all,  family = family,lambda=lambda.min, offset = offset.all)
  # beta.min<-as.vector(coef(fit0))
  
  return(list(beta.se=beta.se,
              beta.min=beta.min))
}



pool_TL <- function(Target,
                    Source,
                    family=c("binomial"),
                    support){
  y.t<-Target$y
  x.t<-Target$x
  p<-ncol(x.t)+1
  
  M<-length(Source)
  
  y.s<-x.s<-NULL
  offset.all<-rep(0,length(y.t))
  
  for (m in 1:M) {
  source.m<-Source[[m]] 
  x.sm<-source.m$x
  y.sm<-source.m$y
  y.s<-c(y.s,y.sm)
  x.s<-rbind(x.s,x.sm)
  
  fit0 <- cv.glmnet(x.sm, y.sm,  family = family)
  lambda<-fit0$lambda.min
  fit0 <- glmnet(x.sm, y.sm,  family = family,lambda=lambda)
  beta<-as.vector(coef(fit0))
  beta.c<-numeric(p)
  beta.c[-c(support+1)]<-beta[-c(support+1)]
  
  offset<-x.t[,support]%*%beta[support+1]
  fit0 <- cv.glmnet(x.t, y.t,  family = family,offset = offset)
  lambda<-fit0$lambda.min
  fit0 <- glmnet(x.t, y.t,  family = family,lambda=lambda, offset = offset)
  delta.m<-beta.c-as.vector(coef(fit0))
  
  offset.all<-c(offset.all,delta.m[1]+x.sm%*%delta.m[-1])
  }
  
  x.all<-rbind(x.t,x.s)
  y.all<-c(y.t,y.s)
  
  fit0 <- cv.glmnet(x.all, y.all,  family = family,offset = offset.all)
  lambda.se<-fit0$lambda.1se
  lambda.min<-fit0$lambda.min
  fit0 <- glmnet(x.all, y.all,  family = family,lambda=lambda.se, offset = offset.all)
  beta.se<-as.vector(coef(fit0))
  
  fit0 <- glmnet(x.all, y.all,  family = family,lambda=lambda.min, offset = offset.all)
  beta.min<-as.vector(coef(fit0))
  
  return(list(beta.se=beta.se,
              beta.min=beta.min))
}





