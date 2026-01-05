rm(list = ls())
library(glmnet)
library(glmtrans)
library(pROC)
library(ggplot2)
library(reshape2)

setwd("~/simulation_result")  # Read the simulation results from the simulation_result file (or from the directory you specified)
point_est <- readRDS("point_est.rds")
AUC_point <- point_est$AUC
err_point <- point_est$error

boot_res <- readRDS("boot_res.rds")
AUC_boot <- c()
error_boot <- c()
for(i in 1:num_replicate){
  if(length(boot_res[[i]]) > 1){
    AUC_boot <- rbind(AUC_boot, boot_res[[i]][[2]])
    error_boot <- rbind(error_boot, boot_res[[i]][[3]])
  }
}

AUC_sd <- apply(AUC_mat, MARGIN = 2, sd)
err_sd <- apply(error_mat, MARGIN = 2, sd)


AUC_upper <- AUC_point + qnorm(0.975) * AUC_sd
AUC_lower <- AUC_point - qnorm(0.975) * AUC_sd

err_upper <- err_point + qnorm(0.975) * err_sd
err_lower <- err_point - qnorm(0.975) * err_sd

AUC_res <- rbind(AUC_lower, AUC_point, AUC_upper)
err_res <- rbind(err_lower, err_point, err_upper)




# ---- palette (exactly as requested) ----
method_cols <- c("Targeted-IFS"     = "#8C3333",
                 "Targeted-IFS-all" = "#D2691E",
                 "Ah-Trans"         = "#7A9D54",
                 "Local"            = "#FFB000")

# ---- method order ----
my_order <- c("Local", "Ah-Trans", "Targeted-IFS-all", "Targeted-IFS")

# ========== AUC forest plot ==========
df_auc <- mat_to_forest_df(AUC_res)
p_auc <- plot_forest_refined(df_auc, xlab = "AUC")
print(p_auc)

# ========== Error-rate forest plot ==========
df_err <- mat_to_forest_df(err_res)
p_err <- plot_forest_refined(df_err, xlab = "Classification error rate")
print(p_err)



# ========== To reproduce point_est and boot_res, please first contact the authors to get the real dataset "real_data.rds", and then run
# source("Functions_GLM_real_data.R")
# my_seed <- 38
# real_data <- readRDS("real_data.rds")
# outcome_list <- real_data$outcome_list
# covariate_list <- real_data$covariate_list
# 
# point_est <- point_est_AUC(outcome_list = outcome_list,
#                            covariate_list = covariate_list,
#                            random_seed = my_seed)
# 
# num_replicate <- 200
# boot_res <- as.list(rep(NA, num_replicate))
# for (boot in seq_len(num_replicate)) {
#   boot_res[[boot]] <- tryCatch(bootstrap_fun(boot = boot, outcome_list = outcome_list,
#                                              covariate_list = covariate_list,
#                                              random_seed = my_seed),
#                                error = function(e) NA)
#   print(boot)
# }