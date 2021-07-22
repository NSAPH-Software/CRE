# test_that("CF ITE Estimated Correctly", {
#   # Generate sample data
#   dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = FALSE, seed = 2021)
#   y <- dataset_cont[["y"]]
#   z <- dataset_cont[["z"]]
#   X <- dataset_cont[["X"]]
#   include_ps <- TRUE
#
#   # Incorrect data inputs
#   expect_error(estimate_ite_cf(y = "test", z, X, include_ps))
#   expect_error(estimate_ite_cf(y, z = "test", X, include_ps))
#   expect_error(estimate_ite_cf(y, z, X = "test", include_ps))
#   expect_error(estimate_ite_cf(y, z, X, include_ps = "test"))
#   expect_error(estimate_ite_cf(y, z, X, include_ps = NA))
#
#   # Correct outputs
#   ite_result <- estimate_ite_cf(y, z, X, include_ps)
#   expect_true(length(ite_result) == length(y))
#   expect_true(class(ite_result) == "numeric")
# })
