# test_that("BCF ITE Estimated Correctly", {
#   # Generate sample data
#   dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = FALSE, seed = 2021)
#   y <- dataset_cont[["y"]]
#   z <- dataset_cont[["z"]]
#   X <- dataset_cont[["X"]]
#
#   # Incorrect data inputs
#   expect_error(estimate_ite_bcf(y = "test", z, X))
#   expect_error(estimate_ite_bcf(y, z = "test", X))
#   expect_error(estimate_ite_bcf(y, z, X = "test"))
#
#   # Correct outputs
#   ite_result <- estimate_ite_bcf(y, z, X)
#   expect_true(length(ite_result) == length(y))
#   expect_true(class(ite_result) == "numeric")
# })
#
