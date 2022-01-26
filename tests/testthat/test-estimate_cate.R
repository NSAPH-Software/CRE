test_that("CATE Estimation Runs Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 2, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- as.data.frame(dataset_cont[["X"]])
  X_names <- names(as.data.frame(X))
  ratio_dis <- 0.25
  ite_method_dis <- "bart"
  include_ps_dis <- "TRUE"
  ps_method_dis <- "SL.xgboost"
  or_method_dis <- NA
  ite_method_inf <- "bart"
  include_ps_inf <- "FALSE"
  ps_method_inf <- "SL.xgboost"
  or_method_inf <- "SL.xgboost"
  ntrees_rf <- 100
  ntrees_gbm <- 50
  min_nodes <- 20
  max_nodes <- 5
  t <- 0.025
  q <- 0.8
  stability_selection <- TRUE
  pfer_val <- 0.1
  include_offset <- FALSE
  offset_name <- NA
  cate_method <- "DRLearner"
  cate_SL_library <- "SL.xgboost"
  filter_cate <- FALSE

  # Check for binary outcome
  binary <- ifelse(length(unique(y)) == 2, TRUE, FALSE)

  # Step 1: Split data
  X <- as.matrix(X)
  y <- as.matrix(y)
  z <- as.matrix(z)
  subgroups <- split_data(y, z, X, ratio_dis)
  discovery <- subgroups[[1]]
  inference <- subgroups[[2]]

  # Generate y, z, and X for discovery and inference data
  y_dis <- discovery[,1]
  z_dis <- discovery[,2]
  X_dis <- discovery[,3:ncol(discovery)]

  y_inf <- inference[,1]
  z_inf <- inference[,2]
  X_inf <- inference[,3:ncol(inference)]

  # Step 2: Estimate ITE
  ite_list_dis <- estimate_ite(y_dis, z_dis, X_dis, ite_method_dis,
                               include_ps_dis, ps_method_dis, or_method_dis,
                               binary, X_names, include_offset, offset_name)
  ite_dis <- ite_list_dis[["ite"]]
  ite_std_dis <- ite_list_dis[["ite_std"]]

  # Step 3: Generate rules list
  initial_rules_dis <- generate_rules(X_dis, ite_std_dis, ntrees_rf, ntrees_gbm,
                                      min_nodes, max_nodes)

  # Step 4: Generate rules matrix
  rules_all_dis <- generate_rules_matrix(X_dis, initial_rules_dis, t)
  rules_matrix_dis <- rules_all_dis[["rules_matrix"]]
  rules_matrix_std_dis <- rules_all_dis[["rules_matrix_std"]]
  rules_list_dis <- rules_all_dis[["rules_list"]]

  # Step 5: Select important rules
  select_rules_dis <- as.character(select_causal_rules(rules_matrix_std_dis,
                                                       rules_list_dis,
                                                       ite_std_dis, q,
                                                       stability_selection, pfer_val))
  select_rules_matrix_dis <- rules_matrix_dis[,which(rules_list_dis %in%
                                                       select_rules_dis)]
  select_rules_matrix_std_dis <- rules_matrix_std_dis[,which(rules_list_dis %in%
                                                               select_rules_dis)]
  if (length(select_rules_dis) == 0) {
    stop("No significant rules were discovered. Ending Analysis.")
  }

  # Step 6: Estimate CATE
  ite_list_inf <- estimate_ite(y_inf, z_inf, X_inf, ite_method_inf,
                               include_ps_inf, ps_method_inf, or_method_inf,
                               binary, X_names, include_offset, offset_name)
  ite_inf <- ite_list_inf[["ite"]]
  ite_std_inf <- ite_list_inf[["ite_std"]]
  sd_ite_inf <- ite_list_inf[["sd_ite"]]

  rules_matrix_inf <- matrix(0, nrow = dim(X_inf)[1],
                             ncol = length(select_rules_dis))
  for (i in 1:length(select_rules_dis)) {
    rules_matrix_inf[eval(parse(text = select_rules_dis[i]),
                          list(X = X_inf)), i] <- 1
  }
  select_rules_interpretable <- interpret_select_rules(select_rules_dis, X_names)

  ###### Run Tests ######

  # Incorrect inputs
  expect_error(estimate_cate(y_inf = "test", z_inf, X_inf, X_names,
                             include_offset, offset_name,
                             rules_matrix_inf, select_rules_interpretable,
                             cate_method, ite_inf, sd_ite_inf, cate_SL_library,
                             filter_cate))
  expect_error(estimate_cate(y_inf, z_inf = "test", X_inf, X_names,
                             include_offset, offset_name,
                             rules_matrix_inf, select_rules_interpretable,
                             cate_method, ite_inf, sd_ite_inf, cate_SL_library,
                             filter_cate))
  expect_error(estimate_cate(y_inf, z_inf, X_inf = "test", X_names,
                             include_offset, offset_name,
                             rules_matrix_inf, select_rules_interpretable,
                             cate_method, ite_inf, sd_ite_inf, cate_SL_library,
                             filter_cate))

  # Correct outputs
  cate_inf <- estimate_cate(y_inf, z_inf, X_inf, X_names, include_offset,
                            offset_name, rules_matrix_inf,
                            select_rules_interpretable, cate_method, ite_inf,
                            sd_ite_inf, cate_SL_library, filter_cate)
  expect_true(class(cate_inf) == "data.frame")
})
