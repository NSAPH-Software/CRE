test_that("Rules Extracted Correctly", {
  # Generate sample data
  skip_on_cran()
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5,
                                       binary_outcome = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  ite_method <- "bart"
  learner_ps <- "SL.xgboost"
  learner_y <- NA
  ntrees <- 100
  node_size <- 20
  max_rules <- 50
  max_depth <- 3
  t_decay <- 0.025

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)

  ###### Discovery ######

  # Step 2: Estimate ITE
  ite <- estimate_ite(y, z, X, ite_method,
                           learner_ps = learner_ps,
                           learner_y = learner_y)

  # Step 3: Generate rules list
  initial_rules <- generate_rules(X, ite, ntrees, node_size,
                                  max_rules, max_depth)

  rules <- filter_irrelevant_rules(initial_rules, X, ite, t_decay)


  ###### Run Tests ######

  # Incorrect inputs
  expect_error(generate_rules_matrix(X = "test", rules))

  # Correct outputs
  rules_matrix <- generate_rules_matrix(X, rules)
  expect_identical(class(rules_matrix), c("matrix", "array"))
})
