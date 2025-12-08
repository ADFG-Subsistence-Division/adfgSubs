#' Combine multiple estimates and confidence intervals
#'
#' Creates a combined estimate and confidence interval from individual estimates and their confidence intervals.
#'
#' @param est A column containing the estimates to be combined
#' @param lower A column containing the lower confidence bounds to be combined
#' @param upper A column containing the upper confidence bounds to be combined
#' @param conf_level The desired confidence level for the combined interval (default is 0.95)
#' @param method The method used to combine estimates; "mean" or "sum"
#'
#' @returns A tibble with combined estimate and confidence interval
#' @export
#'
#' @examples
#' # Example usage:

# Load libraries
# library(dplyr)
# library(tibble)
# library(tidyr)
#
# # Example data frame
# df <- tibble(
#   group = c("A", "A", "B", "B", "B"),
#   est   = c(2, 3, 4, 1, 1.5),
#   lower = c(1, 2, 3, 0.5, 1),
#   upper = c(3, 4, 5, 1.5, 2)
# )
#
#
# # Use in summarize
# df_summary <- df %>%
#   group_by(group) %>%
#   summarize(out = list(combineCI(est, lower, upper, method = "sum")),
#             .groups = 'drop') %>%
#   unnest_wider(out)
#
# print(df_summary)

# # A tibble: 2 × 4
# group estimate lower upper
# <chr>    <dbl>  <dbl> <dbl>
# 1 A          5  2.298 7.70
# 2 B        6.5  3.51  9.49


combineCI <- function(est, lower, upper, conf_level = 0.95, method = c("sum", "mean")) {
  method <- match.arg(method)
  z <- qt(1 - (1 - conf_level) / 2, df = Inf)
  se <- (upper - lower) / (2 * z)

  if (method == "sum") {
    est_out <- sum(est, na.rm = TRUE)
    se_out <- sqrt(sum(se^2, na.rm = TRUE))
  } else if (method == "mean") {
    est_out <- mean(est, na.rm = TRUE)
    # Pool the variances, take sqrt, then scale for mean
    se_out <- sqrt(sum(se^2, na.rm = TRUE)) / length(est)
  }

  lower_out <- est_out - z * se_out
  upper_out <- est_out + z * se_out
  tibble::tibble(estimate = est_out, lower = lower_out, upper = upper_out)
}

