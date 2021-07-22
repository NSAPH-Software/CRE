# test_that("ITE Estimated Correctly", {
#   # Generate sample data
#   dataset_cont <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, effect_size = 0.5, binary = TRUE, seed = 2021)
#   y <- dataset_cont[["y"]]
#   z <- dataset_cont[["z"]]
#   X <- dataset_cont[["X"]]
#   ite_method <- "xbart"
#   include_ps <- TRUE
#   binary <- TRUE
#
#   # Incorrect data inputs
#   expect_error(estimate_ite(y = "test", z, X, ite_method, include_ps, binary))
#   expect_error(estimate_ite(y, z = "test", X, ite_method, include_ps, binary))
#   expect_error(estimate_ite(y, z, X = "test", ite_method, include_ps, binary))
#
#   # Incorrect ite_method input
#   expect_error(estimate_ite(y, z, X, ite_method = "test", include_ps, binary))
#
#   # Incorrect include_ps input
#   expect_error(estimate_ite(y, z, X, ite_method, include_ps = "test", binary))
#
#   # Incorrect binary input
#   expect_error(estimate_ite(y, z, X, ite_method, include_ps, binary = "test"))
#
#   # Correct outputs
#   ite_result <- estimate_ite(y, z, X, ite_method, include_ps, binary)
#   expect_true(length(ite_result) == 2)
#   expect_true(class(ite_result[[1]]) == "numeric")
#   expect_true(class(ite_result[[2]]) == "numeric")
#   expect_true(length(ite_result[[1]]) == length(y))
#   expect_true(length(ite_result[[2]]) == length(y))
#   expect_true(length(unique(ite_result[[1]])) %in% c(1, 2, 3))
# })
