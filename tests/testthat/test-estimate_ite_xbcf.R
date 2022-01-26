test_that("XBCF ITE Estimated Correctly", {
  # Generate sample data
  skip_if_not_installed("XBCF")
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ps_method <- "SL.xgboost"

  # Incorrect data inputs
  expect_error(estimate_ite_xbcf(y = "test", z, X, ps_method))
  expect_error(estimate_ite_xbcf(y, z = "test", X, ps_method))
  expect_error(estimate_ite_xbcf(y, z, X = NA, ps_method))

  # Correct outputs
  ite_result <- estimate_ite_xbcf(y, z, X, ps_method)
  expect_true(length(ite_result) == 2)
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(class(ite_result[[2]]) == "numeric")
})
