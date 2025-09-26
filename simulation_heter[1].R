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
  
 
  ####local lasso using target only
  beta.local<-local_es(Target,family=c("binomial"))
  re.local<-compare.metric(beta.local,theta.true,Test)
  re.local
  support<-which(beta.local[-1]!=0)
  
  ####pool
  
  fit1<-pool_TL1(Target,Source,family=c("binomial"),support=1:supp)
  beta.pool.oracle<-fit1$beta.min
  re.pool.oracle<-compare.metric(beta.pool.oracle,theta.true,Test)
  re.pool.oracle # oracle estimator using our algorithm 1
  
  fit<-pool_TL(Target,Source,family=c("binomial"),support)
  beta.pool.min<-fit$beta.min
  re.pool.min<-compare.metric(beta.pool.min,theta.true,Test)
  re.pool.min # pretrans estimator using our algorithm 2
  
  fit<-pool_TL(Target,Source,family=c("binomial"),support=1:(p-1))
  beta.pool.all<-fit$beta.min
  re.pool.all<-compare.metric(beta.pool.all,theta.true,Test)
  re.pool.all # pretrans estimator using our algorithm 2 under all features
  
  #### all
  fit <- glmtrans(Target, Source, family = "binomial"
                           ,transfer.source.id = "all") 
  beta.all<-fit$beta
  re.all<-compare.metric(beta.all,theta.true,Test)
  re.all ## Feng yang's method without detection of source (all sources are included)
  
  #### auto
  fit <- glmtrans(Target, Source, family = "binomial"
                  ,transfer.source.id = "auto") 
  beta.auto<-fit$beta
  re.auto<-compare.metric(beta.auto,theta.true,Test)
  ## Feng yang's method with detection of imformative source
    
  result<-rbind(re.local,re.pool.oracle,re.pool.min,re.pool.all,re.all,re.auto)
  return(result)
}


setwd("~/simulation_result")   # Save the results to the simulation_result file (or specify your own directory)

###########simulation (i)+(ii), h=10
M=5
n.target <-300 #sample size in each site
n.test<-300
n.source=rep(600, M)
p=500
supp<-20

results<-NULL

Re<-100

for (Ah in 0:M) {
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
      fit.all<-cbind(fit.all,Ah)
      results<-rbind(results,fit.all)
      
      cat("At repnum:", re, "\n")
      re<-re+1
    }, error=function(e){
      cat("At repnum:", re, "Error:", conditionMessage(e), "\n")}) 
  }
  
}


##################
saveRDS(results,file="adap_n300p500_Ah10_heter.rds")





###########simulation (ii)+(iii), h=10
M=5
n.target <-300 #sample size in each site
n.test<-300
n.source=rep(600, M)
p=500
supp<-20

results<-NULL

Re<-100

for (Ah in 0:M) {
  re<-1 
  while(re<=Re) {
    
    data.tran<-data.generate1(cov.type=2,
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
      fit.all<-cbind(fit.all,Ah)
      results<-rbind(results,fit.all)
      
      cat("At repnum:", re, "\n")
      re<-re+1
    }, error=function(e){
      cat("At repnum:", re, "Error:", conditionMessage(e), "\n")}) 
  }
  
}


##################
saveRDS(results,file="adap_n300p500_A10_heter.rds")



###########simulation (ii) only, h=10
Ah<-0
n.target <-300 #sample size in each site
n.test<-300
p=500
supp<-20

results<-NULL

Re<-100

for (M in c(2,4,6,8,10,12)) {
  re<-1 
  n.source=rep(600, M)
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
      fit.all<-cbind(fit.all,M)
      results<-rbind(results,fit.all)
      
      cat("At repnum:", re, "\n")
      re<-re+1
    }, error=function(e){
      cat("At repnum:", re, "Error:", conditionMessage(e), "\n")}) 
  }
  
}

#############

saveRDS(results,file="adap_n300p500_M10_heter.rds")





###########simulation (i)+(ii), h=20
M=5
n.target <-300 #sample size in each site
n.test<-300
n.source=rep(600, M)
p=500
supp<-20

results<-NULL

Re<-100

for (Ah in 0:M) {
  re<-1 
  while(re<=Re) {
    
    data.tran<-data.generate(cov.type=2,
                             M=M, # number of source
                             Ah=Ah, #number of similar source
                             h=20,
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
      fit.all<-cbind(fit.all,Ah)
      results<-rbind(results,fit.all)
      
      cat("At repnum:", re, "\n")
      re<-re+1
    }, error=function(e){
      cat("At repnum:", re, "Error:", conditionMessage(e), "\n")}) 
  }
  
}


##################
saveRDS(results,file="adap_n300p500_Ah20_heter.rds")





###########simulation (ii)+(iii), h=20
M=5
n.target <-300 #sample size in each site
n.test<-300
n.source=rep(600, M)
p=500
supp<-20

results<-NULL

Re<-100

for (Ah in 0:M) {
  re<-1 
  while(re<=Re) {
    
    data.tran<-data.generate1(cov.type=2,
                              M=M, # number of source
                              Ah=Ah, #number of similar source
                              h=20,
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
      fit.all<-cbind(fit.all,Ah)
      results<-rbind(results,fit.all)
      
      cat("At repnum:", re, "\n")
      re<-re+1
    }, error=function(e){
      cat("At repnum:", re, "Error:", conditionMessage(e), "\n")}) 
  }
  
}


##################
saveRDS(results,file="adap_n300p500_A20_heter.rds")



###########simulation (ii) only, h=20
Ah<-0
n.target <-300 #sample size in each site
n.test<-300
p=500
supp<-20

results<-NULL

Re<-100

for (M in c(2,4,6,8,10,12)) {
  re<-1 
  n.source=rep(600, M)
  while(re<=Re) {
    
    data.tran<-data.generate(cov.type=2,
                             M=M, # number of source
                             Ah=Ah, #number of similar source
                             h=20,
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
      fit.all<-cbind(fit.all,M)
      results<-rbind(results,fit.all)
      
      cat("At repnum:", re, "\n")
      re<-re+1
    }, error=function(e){
      cat("At repnum:", re, "Error:", conditionMessage(e), "\n")}) 
  }
  
}

#############

saveRDS(results,file="adap_n300p500_M20_heter.rds")



