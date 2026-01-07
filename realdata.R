rm(list = ls())
library(glmnet)
library(glmtrans)
library(pROC)
library(ggplot2)
library(reshape2)
source("Functions_GLM_real_data.R")
setwd("~/simulation_result")  # Read the simulated sample dataset from the simulation_result file (or from the directory you specified)
real_data <- readRDS("sample_data.rds")
outcome_list <- real_data$outcome_list
covariate_list <- real_data$covariate_list
site_size <- real_data$site_size
random_seed <- real_data$random_seed
random_rng <- real_data$random_rng
random_seed1 <- real_data$random_seed1
random_rng1 <- real_data$random_rng1

# the third site is the target
Source <- as.list(rep(1, 3))

Source[[1]] <- list(y = outcome_list[[1]], x = as.matrix(covariate_list[[1]]))
Source[[2]] <- list(y = outcome_list[[2]], x = as.matrix(covariate_list[[2]]))
Source[[3]] <- list(y = outcome_list[[4]], x = as.matrix(covariate_list[[4]]))

Target <- list(y=NULL,x=NULL)
Test <- list(y=NULL,x=NULL)
target_size <- site_size[3]
do.call(RNGkind, as.list(random_rng))
.Random.seed <- random_seed
testing_index <- sample(1:target_size, size = ceiling(0.4 * target_size))
Target$x <- as.matrix(covariate_list[[3]][-testing_index, ]) 
Target$y <- outcome_list[[3]][-testing_index]

Test$x <- as.matrix(covariate_list[[3]][testing_index, ]) 
Test$y <- outcome_list[[3]][testing_index]

p <- ncol(covariate_list[[3]])

####local lasso using target only
beta.local <- local_es(Target, family = c("binomial"))
re.local<-compare.metric(beta.local,Test)
local_pred <- re.local$pred
y_truth <- re.local$y
support <- which(beta.local[-1]!=0)

####Per Trans
fit <- pool_TL(Target, Source, family=c("binomial"), support)
beta.pool.min <- fit$beta.min
beta.pool.min1 <- beta.pool.min
re.pool.min <- compare.metric(beta.pool.min, Test)
per_pred <- re.pool.min$pred

####Trans all
do.call(RNGkind, as.list(random_rng1))
.Random.seed <- random_seed1
fit<-pool_TL(Target, Source, family=c("binomial"), support = 1:(p-1))
beta.pool.all <- fit$beta.min
re.pool.all <- compare.metric(beta.pool.all, Test)
per_all_pred <- re.pool.all$pred

#### Ah-Trans
fit <- glmtrans(Target, Source, family = "binomial", transfer.source.id = "auto") 
beta.auto <- fit$beta
re.auto <- compare.metric(beta.auto,Test)
feng_pred <- re.auto$pred

error_local <- mean(I(local_pred > 0.5) * I(y_truth == 0) + I(local_pred <= 0.5) * I(y_truth == 1))
auc_local <- auc(roc(response = y_truth, predictor = local_pred)) 

error_per <- mean(I(per_pred > 0.5) * I(y_truth == 0) + I(per_pred <= 0.5) * I(y_truth == 1))
auc_per <- auc(roc(response = y_truth, predictor = per_pred)) 

error_per_all <- mean(I(per_all_pred > 0.5) * I(y_truth == 0) + I(per_all_pred <= 0.5) * I(y_truth == 1))
auc_per_all <- auc(roc(response = y_truth, predictor = per_all_pred)) 

error_feng <- mean(I(feng_pred > 0.5) * I(y_truth == 0) + I(feng_pred <= 0.5) * I(y_truth == 1))
auc_feng <- auc(roc(response = y_truth, predictor = feng_pred)) 

AUC_res <- c(auc_local, auc_per, auc_per_all, auc_feng)
err_res <- c(error_local, error_per, error_per_all, error_feng)

point_est <- list(AUC = AUC_res, error = err_res)

#######################Bootstrap for variance estimation
num_replicate <- 200
boot_res <- as.list(rep(NA, num_replicate))
for (boot in seq_len(num_replicate)) {
  boot_res[[boot]] <- tryCatch(bootstrap_fun(boot = boot, outcome_list = outcome_list,
                                             covariate_list = covariate_list,
                                             random_seed = random_seed,
                                             random_rng = random_rng),
                               error = function(e) NA)
  print(boot)
}

saveRDS(point_est, file = "point_est.rds")
saveRDS(boot_res, file = "boot_res.rds")


