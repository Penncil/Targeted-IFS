library(glmtrans)
library(glmnet)
library(mvtnorm)
library(pROC)
source("Functions_GLM[1].R")
#######generate data######
compare.method<-function(data.train){
  
  Target<-data.tran$Target
  Source<-data.tran$Source
  Test<-data.tran$Test
  theta.true<-data.tran$theta
  
  sca<-c(0.25,0.5,1,2,4)
  ####local lasso using target only
  beta.local<-local_es_multi(Target,family=c("binomial"),sca)
  
  re.local<-re.pool.min<-NULL
  for(i in 1:length(sca)){
  re.local<-rbind(re.local, compare.metric.tpr(beta.local[i,],theta.true,Test))
  support<-which(beta.local[i,-1]!=0)
  
  ####pool
  
  # fit1<-pool_TL1(Target,Source,family=c("binomial"),support=1:supp)
  # beta.pool.oracle<-fit1$beta.min
  # re.pool.oracle<-compare.metric(beta.pool.oracle,theta.true,Test)
  # re.pool.oracle # oracle estimator using our algorithm 1
  
  fit<-pool_TL(Target,Source,family=c("binomial"),support)
  beta.pool.min<-fit$beta.min
  re.pool.min<-rbind(re.pool.min,compare.metric.tpr(beta.pool.min,theta.true,Test))
    # pretrans estimator using our algorithm 2
  }
  
  fit<-pool_TL(Target,Source,family=c("binomial"),support=1:(p-1))
  beta.pool.all<-fit$beta.min
  re.pool.all<-compare.metric.tpr(beta.pool.all,theta.true,Test)
  re.pool.all
  
  result<-rbind(re.local,re.pool.min,re.pool.all)
  return(result)
}




setwd("~/simulation_result")   # Save the results to the simulation_result file (or specify your own directory)
###########simulation 
Ah<-0
n.target <-300 #sample size in each site
n.test<-300
n.source=rep(600, M)
p=500
supp<-20
M<-6

results<-NULL

Re<-100

re<-1 

  while(re<=Re) {
    
    data.tran<-data.generate(cov.type=2,
                             M=M, # number of source
                             Ah=Ah, #number of similar source
                             h=10,
                             n.target=n.target, 
                             n.test = n.test,
                             n.source=n.source,
                             p=p,
                             Gamma=-1,
                             Corr=0.3,
                             supp=supp, # support if each class
                             sig.strength=1)
    
    
    tryCatch({
      fit.all <-compare.method(data.train)
      fit.all<-cbind(fit.all,re)
      results<-rbind(results,fit.all)
      
      cat("At repnum:", re, "\n")
      re<-re+1
    }, error=function(e){
      cat("At repnum:", re, "Error:", conditionMessage(e), "\n")}) 
  }
  

saveRDS(results,file="Sensitive_n300p500_A10_partial.rds")

