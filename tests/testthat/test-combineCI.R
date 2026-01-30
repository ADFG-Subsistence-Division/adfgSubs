test_that("combineCI returns expected result for known input", {

  est   <- c(2, 3, 4, 1, 1.5)
  lower <- c(1, 2, 3, 0.5, 1)
  upper <- c(3, 4, 5, 1.5, 2)

  result <- combineCI(est = est, lower = lower, upper = upper, method = "sum")

  expect_s3_class(result, "tbl")
  expect_named(result, c("estimate", "lower", "upper"))
  expect_length(result, 3)
  expect_equal(round(result$estimate,1), 11.5)
  expect_equal(round(result$lower, 1), 9.6)
  expect_equal(round(result$upper, 1), 13.4)

})

test_that("combineCI detects invalid input", {

  # Non-numeric input
  expect_error(combineCI("a","b","c"),
               "Arguments 'est', 'lower', and 'upper' must all be numeric vectors.")


})

test_that("combineCI errors for unsupported combine method", {
  # Unsupported combine method

  est   <- c(2, 3, 4, 1, 1.5)
  lower <- c(1, 2, 3, 0.5, 1)
  upper <- c(3, 4, 5, 1.5, 2)

  expect_error(combineCI(est, lower, upper, method = "median"),
               "should be one of")
})

test_that("combineCI errors for mismatched vector lengths", {
  est <- c(100, 200)
  lower <- c(90)
  upper <- c(110, 220)
  expect_error(combineCI(est, lower, upper),
               "Arguments 'est', 'lower', and 'upper' must all be the same length.")
})


test_that("combineCI handles NAs in 'sum' and 'mean'", {
  est <- c(NA, 200)
  lower <- c(NA, 180)
  upper <- c(NA, 220)
  conf_level <- 0.95

  result_sum <- combineCI(est, lower, upper, conf_level = conf_level, method = "sum")
  expect_equal(result_sum$estimate, 200)
  # Should ignore NA in sum, se, etc.

  result_mean <- combineCI(est, lower, upper, conf_level = conf_level, method = "mean")
  expect_equal(result_mean$estimate, 200)
  # mean of one value (ignoring NA) is the value itself
})
