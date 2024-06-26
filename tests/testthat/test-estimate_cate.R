test_that("CATE Estimation Runs Correctly (test 1/2)", {
  # Generate sample data
  skip_on_cran()
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method <- "aipw"
  learner_ps <- "SL.xgboost"
  learner_y <- "SL.xgboost"
  ntrees <- 100
  node_size <- 20
  max_rules <- 50
  max_depth <- 3
  t_decay <- 0.025
  t_ext <- 0.02
  t_corr <- 0
  cutoff <- 0.8
  pfer <- 0.1
  stability_selection <- "no"
  offset <- NULL
  intervention_vars <- NULL
  B <- 2
  subsample <- 0.5

  # Check for binary outcome
  binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- honest_splitting(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]

  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery$y
  z_dis <- discovery$z
  X_dis <- discovery$X

  y_inf <- inference$y
  z_inf <- inference$z
  X_inf <- inference$X

  # Estimate ITE
  ite_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method,
                               learner_ps = learner_ps,
                               learner_y = learner_y,
                               offset = offset)

  # Generate rules list
  initial_rules_dis <- generate_rules(X_dis, ite_dis,
                                      ntrees, node_size,
                                      max_rules, max_depth)

  rules_list_dis <- filter_irrelevant_rules(initial_rules_dis, X_dis,
                                            ite_dis, t_decay)

  # Generate rules matrix
  rules_matrix_dis <- generate_rules_matrix(X_dis, rules_list_dis)

  # Filter extreme rules
  rules_matrix_dis <- filter_extreme_rules(rules_matrix_dis, rules_list_dis,
                                           t_ext)
  rules_list_dis <- colnames(rules_matrix_dis)

  # Select important rules
  select_rules_dis <- as.character(select_rules(rules_matrix_dis,
                                                rules_list_dis,
                                                ite_dis,
                                                stability_selection,
                                                cutoff,
                                                pfer,
                                                B))

  # Estimate CATE
  ite_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method,
                               learner_ps = learner_ps,
                               learner_y = learner_y,
                               offset = offset)

  if (length(select_rules_dis) == 0) {
    rules_matrix_inf <- NA
    select_rules_interpretable <- c()
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
    select_rules_interpretable <- interpret_rules(select_rules_dis, X_names)
  }

  ###### Run Tests ######

  # TO DO: add test to check wrong arguments

  # Correct outputs
  cate_inf <- estimate_cate(rules_matrix_inf, select_rules_interpretable,
                            ite_inf, B, subsample)
  expect_true(class(cate_inf) == "data.frame")
})


test_that("CATE Estimation Runs Correctly (test 2/2)", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method <- "aipw"
  learner_ps <- "SL.xgboost"
  learner_y <- "SL.xgboost"
  ite_method <- "aipw"
  learner_ps <- "SL.xgboost"
  learner_y <- "SL.xgboost"
  ntrees <- 100
  node_size <- 20
  max_rules <- 5
  max_depth <- 3
  t_decay <- 0.025
  t_ext <- 0.02
  t_corr <- 0
  t_pvalue <- 0.05
  cutoff <- 0.8
  pfer <- 1
  stability_selection <- "error_control"
  offset <- NULL
  intervention_vars <- NULL
  B <- 2
  subsample <- 0.5

  # Check for binary outcome
  binary_outcome <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- honest_splitting(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]

  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery$y
  z_dis <- discovery$z
  X_dis <- discovery$X

  y_inf <- inference$y
  z_inf <- inference$z
  X_inf <- inference$X

  # No rule is selected
  select_rules_dis <- c()

  # Estimate CATE
  ite_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method,
                               learner_ps = learner_ps,
                               learner_y = learner_y,
                               offset = offset)

  if (length(select_rules_dis) == 0) {
    rules_matrix_inf <- NA
    select_rules_interpretable <- c()
  } else {
    rules_matrix_inf <- generate_rules_matrix(X_inf, select_rules_dis)
    select_rules_interpretable <- interpret_rules(select_rules_dis, X_names)
  }

  ###### Run Tests ######

  # TO DO: add test to check wrong arguments

  # Correct outputs
  cate_inf <- estimate_cate(rules_matrix_inf, select_rules_interpretable,
                            ite_inf, B, subsample)
  expect_true(class(cate_inf) == "data.frame")
})
