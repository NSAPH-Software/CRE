test_that("ITE Estimated Correctly", {
  # Generate sample data
  set.seed(2021)
  dataset_cont <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                       effect_size = 0.5, binary = FALSE)
  y <- dataset_cont[["y"]]
  z <- dataset_cont[["z"]]
  X <- dataset_cont[["X"]]
  X_names <- names(as.data.frame(X))
  include_ps <- TRUE
  ps_method <- "SL.xgboost"
  or_method <- "SL.xgboost"
  binary <- TRUE
  include_offset <- FALSE
  offset_name <- NA

  # Check methods
  ite_result <- estimate_ite(y, z, X, ite_method = "ipw", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "sipw", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "aipw", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "or", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "bart", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(class(ite_result[[3]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(length(ite_result[[3]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "xbart", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(class(ite_result[[3]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(length(ite_result[[3]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "bcf", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(class(ite_result[[3]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(length(ite_result[[3]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "xbcf", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(class(ite_result[[3]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(length(ite_result[[3]]) == length(y))

  ite_result <- estimate_ite(y, z, X, ite_method = "cf", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(class(ite_result[[3]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(length(ite_result[[3]]) == length(y))


  ite_result <- suppressWarnings(estimate_ite(abs(y), z, X, ite_method = "poisson",
                                              include_ps, ps_method, or_method,
                                              binary, X_names, include_offset,
                                              offset_name))
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))

  # Check binary outcome
  dataset_bin <- generate_cre_dataset(n = 100, rho = 0, n_rules = 2, p = 10,
                                      effect_size = 0.5, binary = TRUE)
  y <- dataset_bin[["y"]]
  z <- dataset_bin[["z"]]
  X <- dataset_bin[["X"]]
  ite_result <- estimate_ite(y, z, X, ite_method = "bcf", include_ps,
                             ps_method, or_method, binary,
                             X_names, include_offset, offset_name)
  expect_true(length(ite_result) == 3)
  expect_true(class(ite_result[[1]]) == "numeric")
  expect_true(class(ite_result[[2]]) == "numeric")
  expect_true(length(ite_result[[1]]) == length(y))
  expect_true(length(ite_result[[2]]) == length(y))
  expect_true(length(unique(ite_result[[1]])) %in% c(1, 2, 3))
})
