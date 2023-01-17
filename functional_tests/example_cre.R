set.seed(2021)

# Set Experiment Parameter
n_rules <- 4
sample_size <- 2000
effect_size <- 2
confounding <- "no"
ite_estimator_dis <- "tlearner"
ite_estimator_inf <- "tlearner"
pfer <- n_rules/(effect_size+1)

# Set Method and Hyper Parameters
method_params <- list(ratio_dis = 0.5,
                      ite_method_dis = ite_estimator_dis,
                      ps_method_dis = "SL.xgboost",
                      oreg_method_dis = "SL.xgboost",
                      ite_method_inf = ite_estimator_inf,
                      ps_method_inf = "SL.xgboost",
                      oreg_method_inf = "SL.xgboost")

hyper_params <- list(intervention_vars = NULL,
                     offset = NULL,
                     ntrees_rf = 50,
                     ntrees_gbm = 50,
                     node_size = 20,
                     max_nodes = 8,
                     max_depth = 3,
                     t_decay = 0.025,
                     t_ext = 0.01,
                     t_corr = 1,
                     t_pvalue = 0.05,
                     replace = TRUE,
                     stability_selection = TRUE,
                     cutoff = 0.8,
                     pfer = pfer,
                     penalty_rl = 1)

# Set Ground Truth
{
  if (n_rules==1) {
    dr <- c("x1>0.5 & x2<=0.5")
    em <- c("x1","x2")
  } else if (n_rules==2) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5")
    em <- c("x1","x2","x5","x6")
  } else if (n_rules==3) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5", "x4>0.5")
    em <- c("x1","x2","x5","x6","x4")
  } else if (n_rules==4) {
    dr <- c("x1>0.5 & x2<=0.5", "x5>0.5 & x6<=0.5",
            "x4>0.5", "x5<=0.5 & x7>0.5 & x8<=0.5")
    em <- c("x1","x2","x5","x6","x4","x7","x8")
  } else {
    stop(paste("Synthtic dataset with", n_rules,"rules has not been",
               "implemented yet. Available 'n_rules' options: {1,2,3,4}."))
  }
}

# Generate Dataset
dataset <- generate_cre_dataset(n = sample_size,
                                rho = 0,
                                p = 10,
                                effect_size = effect_size,
                                n_rules = n_rules,
                                binary_covariates = TRUE,
                                binary_outcome = FALSE,
                                confounding = confounding)
y <- dataset[["y"]]
z <- dataset[["z"]]
X <- dataset[["X"]]
ite <- dataset[["ite"]]
X_names <- colnames(X)

result <- cre(y, z, X, method_params, hyper_params)
summary(result)
plot(result)

# Discovery
dr_pred <- result$CATE$Rule[result$CATE$Rule %in% "(BATE)" == FALSE]
metrics_dr <- evaluate(dr,dr_pred)
print(paste("Decision Rules:  ",
            "IoU=",round(metrics_dr$IoU,2),
            ", Recall=",round(metrics_dr$recall,2),
            ", Precision=",round(metrics_dr$precision,2),
            sep=""))

em_pred <- extract_effect_modifiers(dr_pred, X_names)
metrics_em <- evaluate(em,em_pred)
print(paste("Effect Modifiers:  ",
            "IoU=",round(metrics_em$IoU,2),
            ", Recall=",round(metrics_em$recall,2),
            ", Precision=",round(metrics_em$precision,2),
            sep=""))

#Estimation
rmse <- sqrt(mean((ite - result$ite_pred)^2))
print(paste("RMSE: ", rmse))
bias <- mean((ite - result$ite_pred))
print(paste("Bias: ", bias))