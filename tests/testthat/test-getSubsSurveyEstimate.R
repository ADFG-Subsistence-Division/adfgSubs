
test_that("errors if summarize_vars is missing or empty", {
  df <- data.frame(projID=1, studyear=2020, communty="A", strata=1, commhh=10, harvest=5)
  expect_error(getSubsSurveyEstimate(df, summarize_vars = character()), "provide one or more summary")
  expect_error(getSubsSurveyEstimate(df, summarize_vars = NULL), "provide one or more summary")
})

test_that("errors if summarize_vars not in data", {
  df <- data.frame(projID=1, studyear=2020, communty="A", strata=1, commhh=10, harvest=5)
  expect_error(getSubsSurveyEstimate(df, summarize_vars = "missing_col"), "not in provided data frame")
})

test_that("errors if grouping_vars not in data", {
  df <- data.frame(projID=1, studyear=2020, communty="A", strata=1, commhh=10, harvest=5)
  wrong_groups <- c("projID", "studyear", "communitty", "strata")
  expect_error(getSubsSurveyEstimate(df, summarize_vars = "harvest", grouping_vars = wrong_groups), "not in provided data frame")
})

test_that("errors if N_col not in data", {
  df <- data.frame(projID=1, studyear=2020, communty="A", strata=1, commhh=10, harvest=5)
  expect_error(getSubsSurveyEstimate(df, summarize_vars = "harvest", N_col = "not_in_df"), "not in provided data frame")
})

test_that("returns expected summarized result", {
  # Minimal reproducible example
  df <- data.frame(
    projID = c(1,1),
    studyear = c(2020,2020),
    communty = c("A", "A"),
    strata = c(1,1),
    commhh = c(10, 10),
    harvest = c(5, 7)
  )
  # For this known case, we can check min, max, mean, etc.
  result <- getSubsSurveyEstimate(df, summarize_vars = "harvest")
  expect_true(is.data.frame(result) || inherits(result, "tbl"))
  # Check that columns exist:
  expect_true(any(grepl("harvest_mean", colnames(result))))
  # Expected min, max, mean, for this data:
  expect_equal(result$harvest_min, 5)
  expect_equal(result$harvest_max, 7)
  expect_equal(result$harvest_mean, 6)
  expect_equal(result$harvest_median, 6)
  # You could check more for other columns if you know their definitions or can compute by hand
})

test_that("addvariables = TRUE returns expected variables", {
  df <- data.frame(
    projID = rep(1, 10),
    studyear = rep(2020, 10),
    communty = rep("A", 10),
    strata = rep(1, 10),
    commhh = rep(10, 10),
    harvest = c(7, 8, 9, 10, 8, 9, 7, 10, 8, 9)
  )
  result <- getSubsSurveyEstimate(df, summarize_vars = "harvest", addvariables = TRUE)
  # With addvariables = TRUE, variables are added to the data frame
  expect_true(is.data.frame(result) || inherits(result, "tbl"))
  expect_true("harvest_mean" %in% colnames(result))
})

# Suggestion: test with multiple groups, or when variables contain NA
test_that("handles NA values properly", {
  df <- data.frame(
    projID = c(1,1),
    studyear = c(2020,2020),
    communty = c("A", "A"),
    strata = c(1,1),
    commhh = c(10,10),
    harvest = c(NA, 8)
  )
  result <- getSubsSurveyEstimate(df, summarize_vars = "harvest")
  expect_equal(result$harvest_min, 8)
  expect_equal(result$harvest_max, 8)
  expect_equal(result$harvest_mean, 8)
  expect_equal(result$harvest_median, 8)
})
