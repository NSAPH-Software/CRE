# test_that("split_data works as expected.", {
#   # Generate sample data
#   dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = FALSE, seed = 2021)
#   y <- dataset_cont[["y"]]
#   z <- dataset_cont[["z"]]
#   X <- dataset_cont[["X"]]
#   ratio_dis <- 0.25
#
#   # Step 1: Split data
#   X <- as.matrix(X)
#   y <- as.matrix(y)
#   z <- as.matrix(z)
#
#   # Incorrect data inputs
#   expect_error(split_data(y, z, X, ratio_dis = NA))
#   expect_error(split_data(y, z, X, ratio_dis = 2))
#
#   # Correct outputs
#   subgroups <- split_data(y, z, X, ratio_dis)
#   expect_true(length(subgroups) == 2)
#   expect_identical(class(subgroups[[1]]), c("matrix", "array"))
#   expect_identical(class(subgroups[[2]]), c("matrix", "array"))
#   expect_true(nrow(subgroups[[1]]) == ratio_dis * nrow(X))
#   expect_true(nrow(subgroups[[2]]) == (1 - ratio_dis) * nrow(X))
# })
