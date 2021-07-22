# test_that("CATE Estimation Runs Correctly", {
#   # Generate sample data
#   dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = FALSE, seed = 2021)
#   y <- dataset_cont[["y"]]
#   z <- dataset_cont[["z"]]
#   X <- dataset_cont[["X"]]
#   ite_method <- "xbart"
#   include_ps <- "TRUE"
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
#
#   ###### Discovery ######
#
#   # Step 2: Estimate ITE
#   ite_list <- estimate_ite(y, z, X, ite_method, include_ps, binary)
#   ite <- ite_list[["ite"]]
#   ite_std <- ite_list[["ite_std"]]
#
#   # Step 3: Generate rules list
#   initial_rules <- generate_rules(X, ite_std, ntrees_rf, ntrees_gbm, min_nodes, max_nodes)
#
#   # Step 4: Generate rules matrix
#   rules_all <- generate_rules_matrix(X, initial_rules, t)
#   rules_matrix <- rules_all[["rules_matrix"]]
#   rules_matrix_std <- rules_all[["rules_matrix_std"]]
#   rules_list <- rules_all[["rules_list"]]
#
#   ###### Run Tests ######
#
#   # Incorrect inputs
#   expect_error(select_causal_rules(rules_matrix_std = "test", rules_list, ite_std, binary, q, rules_method))
#   expect_error(select_causal_rules(rules_matrix_std, rules_list, ite_std = "test", binary, q, rules_method))
#   expect_error(select_causal_rules(rules_matrix_std, rules_list, ite_std, binary = "test", q, rules_method))
#
#   # Correct outputs
#   select_rules <- select_causal_rules(rules_matrix_std, rules_list, ite_std, binary, q, rules_method)
#   expect_true(class(select_rules) == "character")
# })
