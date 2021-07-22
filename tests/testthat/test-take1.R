# test_that("take1 works as expected.", {
#   # Incorrect data inputs
#   expect_error(take1(len = NA))
#
#   # Correct outputs
#   sequence <- take1(10)
#   expect_true(class(sequence) == "numeric")
#   expect_true(length(sequence) == 5)
# })
