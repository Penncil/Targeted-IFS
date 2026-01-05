#######################################
#     Functions for estimation #
#######################################

expit <- function(x){
  1/(exp(-x)+1)
}

compare.metric<-function(theta, Test){
  x <- Test$x
  y <- Test$y
  pred <- expit(theta[1] + x%*%theta[-1])
  return(list(pred = as.numeric(pred), y = y))
}

local_es<-function(Target,family = c("binomial")){
  y.t <- Target$y
  x.t <- Target$x
  fit0 <- cv.glmnet(x.t, y.t, family = family)
  beta <- as.vector(coef(fit0, s = "lambda.min"))
  return(beta)
}  




pool_TL <- function(Target,
                    Source,
                    family = c("binomial"),
                    support){
  y.t <- Target$y
  x.t <- Target$x
  p <- ncol(x.t) + 1
  
  M <- length(Source)
  
  y.s <- NULL
  x.s <- NULL
  offset.all <- rep(0, length(y.t))
  
  for (m in 1:M) {
    source.m <- Source[[m]] 
    x.sm <- source.m$x
    y.sm <- source.m$y
    y.s <- c(y.s, y.sm)
    x.s <- rbind(x.s, x.sm)
    
    fit0 <- cv.glmnet(x.sm, y.sm, family = family)
    beta <- as.vector(coef(fit0, s = "lambda.min")) 
    beta.S <- beta
    beta.Sc <- beta
    beta.S[-c(support + 1)] <- 0
    beta.Sc[c(support + 1)] <- 0
    
    offset <- x.t %*% beta.S[-1]
    fit_cv <- cv.glmnet(x.t, y.t, family = family, offset = offset)
    delta.m <- as.vector(coef(fit_cv, s = "lambda.min")) - beta.Sc
    delta.m <- -delta.m
    
    offset.all <- c(offset.all, delta.m[1] + x.sm %*% delta.m[-1])
  }
  
  x.all <- rbind(x.t, x.s)
  y.all <- c(y.t, y.s)
  
  fit_cv <- cv.glmnet(x.all, y.all, family = family, offset = offset.all)
  beta.min <- as.vector(coef(fit_cv, s = "lambda.min"))
  
  return(list(beta.min = beta.min))
}



point_est_AUC <- function(outcome_list, covariate_list, random_seed){
  set.seed(random_seed)
  site_size <- rep(0, 4)
  for(i in 1:4){
    site_size[i] <- length(outcome_list[[i]])
  }
  
  
  # the third site is the target
  Source <- as.list(rep(1, 3))
  
  Source[[1]] <- list(y = outcome_list[[1]], x = as.matrix(covariate_list[[1]]))
  Source[[2]] <- list(y = outcome_list[[2]], x = as.matrix(covariate_list[[2]]))
  Source[[3]] <- list(y = outcome_list[[4]], x = as.matrix(covariate_list[[4]]))
  
  Target <- list(y=NULL,x=NULL)
  Test <- list(y=NULL,x=NULL)
  target_size <- site_size[3]
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
  return(list(AUC = AUC_res, error = err_res))
}


bootstrap_fun <- function(boot, covariate_list = c(), outcome_list = c(), random_seed = c()){
  ####prepare for sampling
  original_covariates <- as.matrix(covariate_list[[3]]) 
  original_outcome <- outcome_list[[3]]
  p <- ncol(original_covariates)
  target_size <- length(original_outcome)
  
  # the third site is the target
  Source <- as.list(rep(1, 3))
  
  Source[[1]] <- list(y = outcome_list[[1]], x = as.matrix(covariate_list[[1]]))
  Source[[2]] <- list(y = outcome_list[[2]], x = as.matrix(covariate_list[[2]]))
  Source[[3]] <- list(y = outcome_list[[4]], x = as.matrix(covariate_list[[4]]))
  
  set.seed(random_seed)
  testing_index <- sample(1:target_size, size = ceiling(0.4 * target_size))
  
  set.seed(random_seed*1000 + boot)
  target_idx <- sample(1:target_size, size = target_size, replace = TRUE)
  target_covariates <- original_covariates[target_idx, ]
  target_outcome <- original_outcome[target_idx]
  
  
  Target <- list(y=NULL,x=NULL)
  Test <- list(y=NULL,x=NULL)
  
  Target$x <- target_covariates[-testing_index, ] 
  Target$y <- target_outcome[-testing_index]
  
  Test$x <- target_covariates[testing_index, ] 
  Test$y <- target_outcome[testing_index]
  
  ####local lasso using target only
  beta.local <- local_es(Target, family = c("binomial"))
  re.local<-compare.metric(beta.local,Test)
  local_pred <- re.local$pred
  y_truth <- re.local$y
  support <- which(beta.local[-1]!=0)
  # as.numeric(support_true)
  
  if(length(support) > 1){
    ####Per Trans
    fit <- pool_TL(Target, Source, family=c("binomial"), support)
    beta.pool.min <- fit$beta.min
    re.pool.min <- compare.metric(beta.pool.min, Test)
    per_pred <- re.pool.min$pred
    
    ####Trans all
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
    total_res <- list(boot, AUC_res, err_res)
  }else{
    total_res <- boot
  }
  return(total_res)
}


# ---- helper: 3x4 matrix -> df ----
mat_to_forest_df <- function(M,
                             methods = c("Local", "Targeted-IFS", "Targeted-IFS-all", "Ah-Trans")) {
  stopifnot(is.matrix(M), nrow(M) == 3, ncol(M) == 4)
  
  colnames(M) <- methods
  data.frame(
    Method = factor(methods, levels = methods),
    lower  = as.numeric(M[1, ]),
    point  = as.numeric(M[2, ]),
    upper  = as.numeric(M[3, ])
  )
}



# ---- helper: forest plot ----
plot_forest_refined <- function(df, xlab, x_breaks = waiver(), x_limits = NULL) {
  stopifnot(all(c("Method", "lower", "point", "upper") %in% names(df)))
  
  df$Method <- factor(df$Method, levels = my_order)
  
  ggplot(df, aes(y = Method, x = point, color = Method)) +
    geom_errorbarh(
      aes(xmin = lower, xmax = upper),
      height = 0.22,
      linewidth = 0.9
    ) +
    geom_point(size = 2.8) +
    scale_color_manual(values = method_cols, drop = FALSE) +
    # avoid x-axis text being too close to boundary (add left/right padding)
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      expand = expansion(mult = c(0.04, 0.08))
    ) +
    labs(x = xlab, y = NULL) +
    coord_cartesian(clip = "off") +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      
      axis.title.x = element_text(size = 13),
      axis.text.x  = element_text(size = 13),
      axis.text.y  = element_text(size = 13, angle = 30, hjust = 1),
      
      # extra margins to prevent clipping in Viewer / saving
      plot.margin = margin(t = 8, r = 18, b = 10, l = 10)
    )
}

