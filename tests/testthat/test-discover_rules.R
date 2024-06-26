test_that("discover_rules works as expected!", {
  # Generate sample data
  skip_on_cran()
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 400, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))

  method_params <- list(ratio_dis = 0.5,
                        ite_method = "bart",
                        learner_ps = "SL.xgboost",
                        learner_y = "SL.xgboost")

  hyper_params <- list(intervention_vars = NULL,
                       offset = NULL,
                       ntrees = 30,
                       node_size = 20,
                       max_rules = 5,
                       max_depth = 3,
                       t_decay = 0.025,
                       t_ext = 0.025,
                       t_corr = 0.2,
                       stability_selection = "vanilla",
                       cutoff = 0.6,
                       pfer = 10,
                       B = 2,
                       subsample = 0.5)

  # Input checks
  check_input_data(y = y, z = z, X = X)
  method_params <- check_method_params(y = y, ite = NULL,
                                       params = method_params)
  hyper_params <- check_hyper_params(params = hyper_params)

  # Estimate ITE
  ite <- estimate_ite(y = y, z = z, X = X,
                ite_method = getElement(method_params, "ite_method"),
                learner_ps = getElement(method_params, "learner_ps"),
                learner_y = getElement(method_params, "learner_y"),
                offset = getElement(method_params, "offset"))

  # Generate Causal Decision Rules
  select_rules <- discover_rules(X, ite, method_params, hyper_params)
  expect_true(class(select_rules[[1]]) == "character")

  hyper_params[["effect_modifiers"]] <- X_names[c(5, 7, 8, 9)]
  select_rules <- discover_rules(X, ite, method_params, hyper_params)
  expect_true(class(select_rules[[1]]) == "character")
})
