#' calculateCI
#'
#' @param x An estimated variable for which confidence intervals are to be created.
#' @param conf_level The desired confidence level for the estimate.
#' @param N The total number households in the community.
#'
#' @returns The lower CI, the upper CI, and the
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
#' @description Helper function; not yet implemented.
#' @export
calculateMode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
getSubsStratifiedSurveyEstimate <- function(data,
                                            summarize_vars,
                                            grouping_vars=c("projID", "studyear", "communty", "strata"),
                                            N_col = "commhh",
                                            strata_col = "") {
  stop("Not implemented")
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
