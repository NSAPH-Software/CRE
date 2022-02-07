test_that("Poisson ITE Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- trunc(abs(dataset_cont[["y"]]) * 10)
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  X_names <- names(X)

  # Check with and without offset
  ite_result <- estimate_ite_poisson(y, z, X, X_names, include_offset = FALSE,
                                     offset_name = NA)
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))

  ite_result <- estimate_ite_poisson(y, z, X, X_names, include_offset = TRUE,
                                     offset_name = X_names[1])
  expect_true(class(ite_result) == "numeric")
  expect_true(length(ite_result) == length(y))
})
