# test_that("Rules Extracted Correctly", {
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
#   ###### Run Tests ######
#
#   # Incorrect inputs
#   expect_error(generate_rules_matrix(X = "test", initial_rules, t))
#   expect_error(generate_rules_matrix(X, initial_rules = NA, t))
#   expect_error(generate_rules_matrix(X, initial_rules, t = "test"))
#
#   # Correct outputs
#   rules_all <- generate_rules_matrix(X, initial_rules, t)
#   expect_true(length(rules_all) == 3)
#   expect_identical(class(rules_all[[1]]), c("matrix", "array"))
#   expect_identical(class(rules_all[[2]]), c("matrix", "array"))
#   expect_true(class(rules_all[[3]]) == "character")
#   expect_true(nrow(rules_all[[1]]) == nrow(rules_all[[2]]))
#   expect_true(ncol(rules_all[[1]]) == ncol(rules_all[[2]]))
#   expect_true(ncol(rules_all[[2]]) == length(rules_all[[3]]))
# })
