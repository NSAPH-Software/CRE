test_that("Summary and Print CRE work", {
  dataset <- generate_cre_dataset(n = 1000, rho = 0, n_rules = 2, p = 10,
                                  effect_size = 2, binary = FALSE)

  cre_results <- cre(y = dataset[["y"]], z = dataset[["z"]],
                     X = as.data.frame(dataset[["X"]]), ratio_dis = 0.25,
                     ite_method_dis = "bart", include_ps_dis = TRUE,
                     ite_method_inf = "bart", include_ps_inf = TRUE,
                     ntrees_rf = 100, ntrees_gbm = 50, min_nodes = 20,
                     max_nodes = 5, t = 0.025, q = 0.8)

  # incorrect input
  expect_error(print_cre("test"))
  expect_error(summary_cre("test"))

  # run functions
  expect_output(print_cre(cre_results))
  expect_output(summary_cre(cre_results))
})
