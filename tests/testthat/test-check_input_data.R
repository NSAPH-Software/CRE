test_that("check_input_data works as expected.", {

  skip_on_cran()
  ds_1 <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary_outcome = FALSE)

  ds_2 <- generate_cre_dataset(n = 200, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary_outcome = FALSE)

  # invalid size
  expect_error(check_input_data(ds_1$y, ds_2$z, ds_2$X))
  expect_error(check_input_data(ds_1$y, ds_1$z, ds_2$X))

  # invalid class
  yy <- matrix(c(ds_1$y, ds_1$y), ncol = 2)
  expect_error(check_input_data(yy, ds_1$z, ds_1$X))

  zz <- matrix(c(ds_1$z, ds_1$z), ncol = 2)
  expect_error(check_input_data(ds_1$y, zz, ds_1$X))

  # invalid data type
  l_y <- sample(letters, 100, replace = TRUE)
  expect_error(check_input_data(l_y, ds_1$z, ds_1$X))

  # non-binary treatment
  z_3 <- sample(c(1, 2, 3), 100, replace = TRUE)
  expect_error(check_input_data(ds_1$y, z_3, ds_1$X))

  # non-numeric response
  y <- sample(c("a", "b", "c"), 100, replace = TRUE)
  expect_error(check_input_data(y, ds_1$z, ds_1$X))

  # non-numeric treatment
  z <- sample(c("a", "b", "c"), 100, replace = TRUE)
  expect_error(check_input_data(ds_1$y, z, ds_1$X))

  # non-matrix covariates
  X <- c(1, 2, 3)
  expect_error(check_input_data(ds_1$y, ds_1$z, X))

  # non-numeric covariates
  X <- matrix(c("a", "b", "c"))
  expect_error(check_input_data(ds_1$y, ds_1$z, X))

  # Check with ITE
  ds_3 <- generate_cre_dataset(n = 300, rho = 0, n_rules = 2, p = 10,
                               effect_size = 2, binary_outcome = FALSE)

  ite_2 <- matrix(c(ds_3$ite, ds_3$ite), ncol = 2)
  expect_error(check_input_data(ds_3$y, ds_3$z, ds_3$X, ite_2))

})
