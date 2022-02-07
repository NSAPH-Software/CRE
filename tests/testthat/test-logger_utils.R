test_that("Logger Utils works as expected", {
  # set_logger()
  set_logger("Debug")

  # get_logger()
  log_meta <- get_logger()
  expect_true(length(log_meta) == 2)
  expect_true(class(log_meta[[1]]) == "character")
  expect_true(class(log_meta[[2]]) == "character")
})
