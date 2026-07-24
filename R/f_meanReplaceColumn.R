#' Mean replace values for use with mutate(across(...))
#'
#' This function imputes missing values in a numeric vector (e.g., survey data) using stratified means and then community means. If both mean imputations fail, a minimum replacement is used based on checkCol.
#'
#' @param repl_col Numeric vector. The column to be mean replaced (e.g., observed values with possible NAs).
#' @param strata Vector (factor, character, or integer). Identifies strata for stratified mean imputation.
#' @param commhh Vector. Community household identifier, or grouping used for estimation at the community level.
#' @param NHouseholds Numeric or integer vector. The number of households in each group (typically reused per row).
#' @param checkCol Numeric or integer vector. Used to determine minimum replacement (1 if >0 and not NA, else 0).
#'
#' @return A numeric vector of the same length as \code{repl_col}, containing the mean-replaced or imputed values.
#' @export
#'
#' @examples
#'
#' df <- tibble(strata = c("A","A","A","B","B","B"),
#'              commhh = c(99,99,99,53,53,53),
#'              NHouseholds = c(99,99,99,53,53,53),
#'              col1 = c(1, NA, 7, 2, 4, NA),
#'              col2 = c(18, 39, NA, 109, NA, 44),
#'              checkCol = c(1,1,1,1,1,1))
#'
#' replCols <- c("col1","col2")
#'
#' suffix <- c("MR")
#'
#' # Usage:
#'  df %>%
#'     mutate(across(replCols, ~ meanReplaceColumn(.x, strata, commhh, NHouseholds, checkCol = "checkCol"),
#'     .names = paste0("{.col}_", suffix)))


meanReplaceColumn <- function(repl_col, strata, commhh, NHouseholds, checkCol) {
  mMean <- ave(repl_col, strata, FUN = function(x) mean(x, na.rm = TRUE))
  mMean[mMean == 0] <- NA
  meanReplaced <- dplyr::coalesce(repl_col, mMean)
  strataEst <- mMean * commhh
  commEst <- ave(strataEst, commhh, FUN = function(x) sum(x, na.rm = TRUE))
  cnt <- ave(repl_col, strata, FUN = function(x) length(x))
  cMean <- commEst / NHouseholds / cnt
  cMean[cMean == 0] <- NA
  meanReplaced <- dplyr::coalesce(meanReplaced, cMean)
  minimumReplace <- ifelse(is.na(checkCol) | checkCol <= 0, 0, 1)
  dplyr::coalesce(meanReplaced, minimumReplace)
}


