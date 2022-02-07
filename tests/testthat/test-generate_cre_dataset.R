test_that("Synthetic Data Generated Correctly", {
  # Incorrect n input
  set.seed(2021)
  expect_error(generate_cre_dataset(n = "test", rho = 0,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary = FALSE))

  # Incorrect rho input
  expect_error(generate_cre_dataset(n = 100, rho = "test",
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary = FALSE))

  # Incorrect number of rules
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 3, p = 10, effect_size = 0.5,
                                    binary = FALSE))
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = "test", p = 10, effect_size = 0.5,
                                    binary = FALSE))

  # Incorrect effect_size input
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = "test",
                                    binary = FALSE))

  # Incorrect binary input
  expect_error(generate_cre_dataset(n = 100, rho = 0,
                                    n_rules = 2, p = 10, effect_size = 0.5,
                                    binary = "test"))

  # 2 rules
  test_data_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                         effect_size = 0.5, binary = TRUE)
  expect_true(class(test_data_cont) == "list")
  expect_true(length(test_data_cont) == 3)
  expect_true(class(test_data_cont[[1]]) == "numeric")
  expect_true(class(test_data_cont[[2]]) == "integer")
  expect_identical(class(test_data_cont[[3]]), "data.frame")
  expect_true(length(test_data_cont[[1]]) == 100)
  expect_true(length(test_data_cont[[2]]) == 100)
  expect_true(nrow(test_data_cont[[3]]) == 100)
  expect_true(ncol(test_data_cont[[3]]) == 10)

  # 4 rules
  test_data_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 4, p = 10,
                                         effect_size = 0.5, binary = TRUE)
  expect_true(class(test_data_cont) == "list")
  expect_true(length(test_data_cont) == 3)
  expect_true(class(test_data_cont[[1]]) == "numeric")
  expect_true(class(test_data_cont[[2]]) == "integer")
  expect_identical(class(test_data_cont[[3]]), "data.frame")
  expect_true(length(test_data_cont[[1]]) == 100)
  expect_true(length(test_data_cont[[2]]) == 100)
  expect_true(nrow(test_data_cont[[3]]) == 100)
  expect_true(ncol(test_data_cont[[3]]) == 10)
})
