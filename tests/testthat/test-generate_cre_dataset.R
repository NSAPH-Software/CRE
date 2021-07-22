# test_that("Synthetic Data Generated Correctly", {
#   # Incorrect n input
#   expect_error(generate_cre_dataset(n = "test", rho = 0,
#                                     n_rules = 2, effect_size = 0.5,
#                                     binary = FALSE, seed = 2021))
#
#   # Incorrect rho input
#   expect_error(generate_cre_dataset(n = 1000, rho = "test",
#                                     n_rules = 2, effect_size = 0.5,
#                                     binary = FALSE, seed = 2021))
#
#   # Incorrect number of rules
#   expect_error(generate_cre_dataset(n = 1000, rho = 0,
#                                     n_rules = 3, effect_size = 0.5,
#                                     binary = FALSE, seed = 2021))
#   expect_error(generate_cre_dataset(n = 1000, rho = 0,
#                                     n_rules = "test", effect_size = 0.5,
#                                     binary = FALSE, seed = 2021))
#
#   # Incorrect effect_size input
#   expect_error(generate_cre_dataset(n = 1000, rho = 0,
#                                     n_rules = 2, effect_size = "test",
#                                     binary = FALSE, seed = 2021))
#
#   # Incorrect binary input
#   expect_error(generate_cre_dataset(n = 1000, rho = 0,
#                                     n_rules = 2, effect_size = 0.5,
#                                     binary = "test", seed = 2021))
#
#   # Correct outputs
#   n <- 1000
#   test_data_cont <- generate_cre_dataset(n = n, rho = 0, n_rules = 2, effect_size = 0.5,
#                                          binary = TRUE, seed = 2021)
#   expect_true(class(test_data_cont) == "list")
#   expect_true(length(test_data_cont) == 3)
#   expect_true(class(test_data_cont[[1]]) == "numeric")
#   expect_true(class(test_data_cont[[2]]) == "integer")
#   expect_identical(class(test_data_cont[[3]]), c("matrix", "array"))
#   expect_true(length(test_data_cont[[1]]) == n)
#   expect_true(length(test_data_cont[[2]]) == n)
#   expect_true(nrow(test_data_cont[[3]]) == n)
#   expect_true(ncol(test_data_cont[[3]]) == 10)
# })
