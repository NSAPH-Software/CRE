test_that("Rules Interpreted Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ite_method <- "bart"
  include_ps <- "TRUE"
  ps_method <- "SL.xgboost"
  or_method <- NA
  ntrees_rf <- 100
  ntrees_gbm <- 50
  min_nodes <- 20
  max_nodes <- 5
  t <- 0.025
  q <- 0.8
  stability_selection <- TRUE
  pfer_val <- 0.1

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  X_names <- names(as.data.frame(X))
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  # Step 2: Estimate ITE
  ite_list <- estimate_ite(y, z, X, ite_method, include_ps, ps_method,
                           or_method, binary)
  ite <- ite_list[["ite"]]
  ite_std <- ite_list[["ite_std"]]

  # Step 3: Generate rules list
  initial_rules <- generate_rules(X, ite_std, ntrees_rf, ntrees_gbm,
                                      min_nodes, max_nodes)

  # Step 4: Generate rules matrix
  rules_all <- generate_rules_matrix(X, initial_rules, t)
  rules_matrix <- rules_all[["rules_matrix"]]
  rules_matrix_std <- rules_all[["rules_matrix_std"]]
  rules_list_dis <- rules_all[["rules_list"]]

  # Step 5: Select important rules
  select_rules <- as.character(select_causal_rules(rules_matrix_std, rules_list_dis,
                                                   ite_std, q, stability_selection,
                                                   pfer_val))

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(interpret_select_rules(select_rules, X_names = NA))

  # Correct outputs
  select_rules_interpretable <- interpret_select_rules(select_rules, X_names)
  expect_true(class(select_rules_interpretable) == "character")
  expect_true(length(select_rules_interpretable) == length(select_rules))
})
