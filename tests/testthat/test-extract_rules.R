# test_that("Rules Extracted Correctly", {
#   # Generate sample data
#   dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = FALSE, seed = 2021)
#   y <- dataset_cont[["y"]]
#   z <- dataset_cont[["z"]]
#   X <- dataset_cont[["X"]]
#   ite_method <- "xbart"
#   include_ps <- "TRUE"
#   ntrees <- 100
#   min_nodes <- 20
#   max_nodes <- 5
#
#   # Check for binary outcome
#   binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)
#
#   # Step 1: Split data
#   X <- as.matrix(X)
#   y <- as.matrix(y)
#   z <- as.matrix(z)
#
#   # Step 2: Estimate ITE
#   ite_list <- estimate_ite(y, z, X, ite_method, include_ps, binary)
#   ite <- ite_list[["ite"]]
#   ite_std <- ite_list[["ite_std"]]
#
#   # Set parameters
#   N <- dim(X)[1]
#   sf <- min(1, (11 * sqrt(N) + 1) / N)
#   mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
#   # Random Forest
#   forest <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
#                                                         sampsize = sf * N,
#                                                         replace = FALSE,
#                                                         ntree = 1, maxnodes = mn,
#                                                         nodesize = min_nodes))
#   for(i in 2:ntrees) {
#     mn <- 2 + floor(stats::rexp(1, 1 / (max_nodes - 2)))
#     model1_RF <- suppressWarnings(randomForest::randomForest(x = X, y = ite_std,
#                                                              sampsize = sf * N,
#                                                              replace = FALSE,
#                                                              ntree = 1, maxnodes = mn,
#                                                              nodesize = min_nodes))
#     forest <- randomForest::combine(forest, model1_RF)
#   }
#   treelist <- inTrees::RF2List(forest)
#
#   ###### Run Tests ######
#
#   # Incorrect inputs
#   expect_error(extract_rules(treelist = "test", X, ntrees, ite_std))
#   expect_error(extract_rules(treelist, X = "test", ntrees, ite_std))
#   expect_error(extract_rules(treelist, X, ntrees = NA, ite_std))
#   expect_error(extract_rules(treelist, X, ntrees, ite_std = "test"))
#
#   # Correct outputs
#   rules_RF <- extract_rules(treelist, X, ntrees, ite_std)
#   expect_true(class(rules_RF) == "character")
# })
