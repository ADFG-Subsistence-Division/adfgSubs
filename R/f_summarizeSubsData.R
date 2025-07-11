#' calculateCI
#'
#' @param x An estimated variable for which confidence intervals are to be created.
#' @param conf_level The desired confidence level for the estimate.
#' @param N The total number households in the community.
#'
#' @returns The lower CI bound, the upper CI bound, and the relative margin of error
#' @export
calculateCI <- function(x, conf_level = 0.95, N = NULL) {
  # Note: this code discounts the 'n' for individual missing components
  x <- na.omit(x)
  n <- length(x)
  if (n == 0) return(c(NA, NA)) # Return NA if no valid data
  # Calculate standard error with or without FPC
  stderr <- sd(x) / sqrt(n)
  if (!is.null(N) && N > n) {
    # Apply finite population correction (FPC)
    fpc <- sqrt((N - n) / (N - 1))
    stderr <- stderr * fpc
  }
  # Calculate confidence interval
  error <- qt(1 - ((1 - conf_level) / 2), df = n - 1) * stderr
  mean_x <- mean(x)
  c(mean_x - error, mean_x + error, error / mean_x)
}

#' calculateMode
#'
#' @param x A numeric variable for which the mode will be calculated.
#'
#' @returns The mode of the values in the supplied variable.
#'
#' @description Helper function
#' @export
calculateMode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' getSubsStratifiedSurveyEstimate
#'
#' @param x A list of variables for which expanded estimates, confidence intervals,
#' and relative margins of error will be calculated and summarized.
#'
#' @returns Estimates, confidence intervals, and relative margins of error
#'
#' @description Calculates variable estimates for stratified samples.
#' @export
getSubsStratifiedSurveyEstimate <- function(
    data,
    summarize_vars,
    grouping_vars = c("projID", "studyear", "communty", "strata"),
    N_col = "commhh",
    strata_col = "strata"
) {
  library(dplyr)
  library(tidyr)
  library(purrr)

  # Check inputs
  if (length(summarize_vars) == 0) stop("You must provide one or more summary variables.")
  if (!all(summarize_vars %in% names(data))) stop("One or more specified summary variables not in provided data frame.")
  if (!all(grouping_vars %in% names(data))) stop("One or more specified grouping variables not in provided data frame.")
  if (!N_col %in% names(data)) stop("The specified total 'N' value (N_col) not in provided data frame.")
  if (!strata_col %in% names(data)) stop("The specified strata column (strata_col) not in provided data frame.")

  # Calculate household-level estimates (wide)
  hh_estimates <- data %>%
    select(all_of(c(grouping_vars, N_col, summarize_vars))) %>%
    mutate(across(all_of(summarize_vars), as.numeric)) # ensure numeric

  # Convert to long format for summary
  long_hh <- hh_estimates %>%
    pivot_longer(cols = all_of(summarize_vars), names_to = "variable", values_to = "value")

  # Summarize by group, variable
  summarized <- long_hh %>%
    group_by(across(all_of(grouping_vars)), variable) %>%
    summarise(
      n = n(),
      N = first(.data[[N_col]]),
      mean = mean(value, na.rm = TRUE),
      var = var(value, na.rm = TRUE),
      se = sqrt(var(value, na.rm = TRUE) / n()),
      t_crit = qt(0.975, df = n()-1),
      margin = t_crit * se,
      ci_lower = mean - margin,
      ci_upper = mean + margin,
      rel_margin = ifelse(mean != 0, margin / abs(mean), NA_real_),
      .groups = "drop"
    )

  # Output as a list: household-level data and summary table
  return(
    household_estimates = hh_estimates,
    summary = summarized
  )
}

#' getSubsSurveyEstimate
#'
#' @param data A data frame.
#' @param summarize_vars A vector containing names of variables to be summarized.
#' @param grouping_vars A vector containing names of variables to group by;
#' one estimate is given for each combination of grouping variables.
#' @param N_col A column in the data frame with the total number of households (N).
#' @param addvariables Logical; should the columns containing descriptive statistics
#' and the estimate be returned in addition to the summarized data frame? Default is 'FALSE'.
#'
#' @returns A data frame with summarized variables.
#'
#' @description Summary data specific to subsistence the analysis design for subsistence.
#' @export
getSubsSurveyEstimate <- function(data,
                                  summarize_vars,
                                  grouping_vars=c("projID", "studyear", "communty", "strata"),
                                  N_col="commhh",
                                  addvariables=FALSE) {
  # Error trapping first.
  if (length(summarize_vars) == 0) {
    stop("You must provide one or more summary variables.")
  }
  if (!all(summarize_vars %in% names(data))) {
    stop("One or more specified summary variables not in provided data frame.")
  }
  if (!all(grouping_vars %in% names(data))) {
    stop("One or more specified grouping variables not in provided data frame.")
  }
  if (!all(N_col %in% names(data))) {
    stop("The specified total 'N' value (N_col) not in provided data frame.")
  }
  # Set up grouping and add in the weighting factor column.
  grouped_data <- data %>% group_by(across(all_of(c(grouping_vars, N_col)))) %>%
    rename("N" = one_of(N_col))
  groups <- grouped_data %>% group_keys()
  if (addvariables) {
    data <- data %>%
      group_by(across(all_of(grouping_vars))) %>%
      mutate(across(all_of(summarize_vars), list(
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        mode = ~calculateMode(.),
        est = ~ groups$N[cur_group_id()] * mean(., na.rm = TRUE),
        ci_mean_lower = ~calculateCI(., N = groups$N[cur_group_id()])[1],
        ci_mean_upper = ~calculateCI(., N = groups$N[cur_group_id()])[2],
        CIP = ~calculateCI(., N = groups$N[cur_group_id()])[3]
      ), .names = "{.col}_{.fn}")) %>%
      ungroup()
    return(data)
  }
  # Perform summarization with the provided N values
  summarized_data <- grouped_data %>%
    summarise(across(all_of(summarize_vars), list(
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE),
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      mode = ~calculateMode(.),
      est = ~ groups$N[cur_group_id()] * mean(., na.rm = TRUE),
      ci_mean_lower = ~calculateCI(., N = groups$N[cur_group_id()])[1],
      ci_mean_upper = ~calculateCI(., N = groups$N[cur_group_id()])[2],
      CIP = ~calculateCI(., N = groups$N[cur_group_id()])[3]
    ), .names = "{.col}_{.fn}"), .groups = "drop")
  return(summarized_data)
}
