rm(list = ls())
library(glmnet)
library(glmtrans)
library(pROC)
library(ggplot2)
library(reshape2)
source("Functions_GLM_real_data.R")
my_seed <- 38
real_data <- readRDS("real_data.rds")
outcome_list <- real_data$outcome_list
covariate_list <- real_data$covariate_list

point_est <- point_est_AUC(outcome_list = outcome_list,
                           covariate_list = covariate_list,
                           random_seed = my_seed)

num_replicate <- 200
boot_res <- as.list(rep(NA, num_replicate))
for (boot in seq_len(num_replicate)) {
  boot_res[[boot]] <- tryCatch(bootstrap_fun(boot = boot, outcome_list = outcome_list,
                                             covariate_list = covariate_list,
                                             random_seed = my_seed),
                               error = function(e) NA)
  print(boot)
}