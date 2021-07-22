# test_that("CATE Estimation Runs Correctly", {
#   # Generate sample data
#   dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = FALSE, seed = 2021)
#   y <- dataset_cont[["y"]]
#   z <- dataset_cont[["z"]]
#   X <- dataset_cont[["X"]]
#   ratio_dis <- 0.25
#   ite_method_dis <- "xbart"
#   ite_method_inf <- "xbcf"
#   include_ps_dis <- "TRUE"
#   include_ps_inf <- NA
#   ntrees_rf <- 100
#   ntrees_gbm <- 50
#   min_nodes <- 20
#   max_nodes <- 5
#   t <- 0.025
#   q <- 0.8
#   rules_method <- NA
#
#   # Check for binary outcome
#   binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
#
#   # Step 1: Split data
#   X <- as.matrix(X)
#   y <- as.matrix(y)
#   z <- as.matrix(z)
#   subgroups <- split_data(y, z, X, ratio_dis)
#   discovery <- subgroups[[1]]
#   inference <- subgroups[[2]]
#
#   # Generate y, z, and X for discovery and inference data
#   y_dis <- discovery[,1]
#   z_dis <- discovery[,2]
#   X_dis <- discovery[,3:ncol(discovery)]
#
#   y_inf <- inference[,1]
#   z_inf <- inference[,2]
#   X_inf <- inference[,3:ncol(discovery)]
#
#   ###### Discovery ######
#
#   # Step 2: Estimate ITE
#   ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis, include_ps_dis, binary)
#   ite_dis <- ite_list_dis[["ite"]]
#   ite_std_dis <- ite_list_dis[["ite_std"]]
#
#   # Step 3: Generate rules list
#   initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm, min_nodes, max_nodes)
#
#   # Step 4: Generate rules matrix
#   rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
#   rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
#   rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
#   rules_list_dis <- rules_all_dis[["rules_list"]]
#
#   # Step 5: Select important rules
#   select_rules_dis <- select_causal_rules(rules_matrix_std_dis, rules_list_dis, ite_std_dis, binary, q, rules_method)
#   select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in% select_rules_dis)]
#   select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in% select_rules_dis)]
#
#   ###### Inference ######
#
#   # Step 2: Estimate ITE
#   ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf, include_ps_inf, binary)
#   ite_inf <- ite_list_inf[["ite"]]
#   ite_std_inf <- ite_list_inf[["ite_std"]]
#
#   # Step 6: Estimate CATE
#   rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1], ncol = length(select_rules_dis))
#   for (i in 1:length(select_rules_dis)) {
#     rules_matrix_inf[eval(parse(text = select_rules_dis[i]), list(X = X_inf)), i] <- 1
#   }
#   cate_inf <- estimate_cate(ite_inf, rules_matrix_inf, select_rules_dis)
#
#
#   ###### Run Tests ######
#
#   # Incorrect inputs
#   expect_error(estimate_cate(ite_inf = "test", rules_matrix_inf, select_rules_dis))
#   expect_error(estimate_cate(ite_inf, rules_matrix_inf = "test", select_rules_dis))
#   expect_error(estimate_cate(ite_inf, rules_matrix_inf, select_rules_dis = "test"))
#
#   # Correct outputs
#   cate_inf <- estimate_cate(ite_inf, rules_matrix_inf, select_rules_dis)
#   expect_true(class(cate_inf) == "data.frame")
#   expect_identical(names(cate_inf), c("Rule", "Model_Coef", "CATE", "PVal", "CI_lower", "CI_upper"))
#   expect_true(cate_inf[1,1] == "(Intercept)")
# })
