test_that("CRE Runs Correctly", {
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
  include_ps_inf <- "TRUE"
  ps_method_inf <- "SL.xgboost"
  or_method_inf <- NA
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

  # Incorrect y, z, X input
  expect_error(cre(y = "test", z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z = "test", X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,  ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X = "test", ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect ratio_dis input
  expect_error(cre(y, z, X, ratio_dis = NA, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis = 2, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect ite_method input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = 0, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = "test", include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf = 0, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf = "test",
                   include_ps_inf, ps_method_inf, or_method_inf, ntrees_rf,
                   ntrees_gbm, min_nodes, max_nodes, t, q, stability_selection,
                   pfer_val, include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect ps_method input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis = 0, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf = 0, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect or_method input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = "aipw", include_ps_dis,
                   ps_method_dis, or_method_dis = 0, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf = "aipw",
                   include_ps_inf, ps_method_inf, or_method_inf = 0, ntrees_rf,
                   ntrees_gbm, min_nodes, max_nodes, t, q, stability_selection,
                   pfer_val, include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect include_ps input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis = "test",
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                   ps_method_dis, or_method_dis, ite_method_inf, include_ps_inf = "test",
                   ps_method_inf = 0, or_method_inf, ntrees_rf, ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q inputs
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf = "test", ntrees_gbm,
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm = "test",
                   min_nodes, max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm,
                   min_nodes = "test", max_nodes, t, q,
                   stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes = "test", t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t = "test", q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q = "test", stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect stability_selection, pfer_val inputs
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection = "test", pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val = "test",
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect include_offset input
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis = "poisson",
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset = "test", offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Incorrect cate_method, cate_SL_library, filter_cate inputs
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method = 0, cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method = "test", cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf = "poisson", include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method = "bart-baggr", cate_SL_library, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf = "poisson", include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method = "cf-means", cate_SL_library, filter_cate))

  # Incorrect cate_SL_library, filter_cate inputs
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library = 2, filter_cate))
  expect_error(cre(y, z, X, ratio_dis, ite_method_dis,
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate = "test"))

  # Incorrect binary inputs
  dataset_bin <- generate_cre_dataset(n = 500, rho = 0, n_rules = 2, p = 10,
                                      effect_size = 2, binary = TRUE)
  y_bin <- dataset_bin[["y"]]
  z_bin <- dataset_bin[["z"]]
  X_bin <- as.data.frame(dataset_bin[["X"]])
  expect_error(cre(y_bin, z_bin, X_bin, ratio_dis, ite_method_dis = "bcf",
                   include_ps_dis, ps_method_dis, or_method_dis,
                   ite_method_inf, include_ps_inf,
                   ps_method_inf, or_method_inf,
                   ntrees_rf, ntrees_gbm, min_nodes,
                   max_nodes, t, q, stability_selection, pfer_val,
                   include_offset, offset_name,
                   cate_method, cate_SL_library, filter_cate))

  # Correct outputs
  cre_results <- cre(y, z, X, ratio_dis, ite_method_dis, include_ps_dis,
                     ps_method_dis, or_method_dis, ite_method_inf,
                     include_ps_inf, ps_method_inf, or_method_inf,
                     ntrees_rf, ntrees_gbm, min_nodes, max_nodes, t, q,
                     stability_selection, pfer_val, include_offset, offset_name,
                     cate_method, cate_SL_library, filter_cate)
  expect_true(class(cre_results) == "cre")
})
