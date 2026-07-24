#' Replace missing values in a specified column
#'
#' @description
#' Simple mean replacement for a stratified sample. This function REQUIRES that
#'    sourceData contain: "projID", "studyear", "communty", "resource",
#'                        "strata", replCol, "commhh", "NHouseholds".
#'    additionally, a 'check-column' is required, this might be
#'    "harvestq" (default) or "filterq" or other specified column to indicate
#'    whether or not a resource requires a non-zero amount (ie: minimum replacement).
#' This is to be used for simple circumstances where there are no details
#'    such as sex, season or gear type.
#' Note that the inclusion of harvestq in this list is intended to differentiate
#'   between 'some amount, amount unknown", and instances where the question of
#'   whether or not a harvest even occurred is unknown. If the status is
#'   "some amount, amount unknown" ie: harvestq == 1, then minimum replacement
#'   occurs, if not then the community mean is supplied regardless.
#'
#' @param sourceData A data frame
#' @param replCol A vector containing the variables from sourceData for which values will be mean replaced.
#' @param checkCol Description
#' @returns A modified column or set of columns containing mean replaced values; must be bound to original data frame (if desired).
#'
#' @export
 
meanReplaceStratified <- function(sourceData, replCol, checkCol = "harvestq")
{

  # 1.1 Required columns for mean replacement (minimum)
  sNamesList <- c("projID", "studyear", "communty",
                  "resource", "strata", replCol, "commhh", "NHouseholds",
                  checkCol)

  for(kk in sNamesList)
  {
    if(!(kk %in% names(sourceData)))
    {
      print("ERROR,data not present")
      error = c(str_interp("required column ${kk} not present in source data file"))
      return(data.frame(error))
    }
  }

  # 1.2 To simplify processing, rename variable column names to standards
  #     for easier replacement later.
  # 1.2.1 Rename target data to mean Replaced
  sourceData <- rename(sourceData, "meanReplaced"=all_of(replCol))

  # 1.2.2 Rename the checkCol.
  sourceData <- rename(sourceData, "checkCol"=all_of(checkCol))

  # 1.3 Calculate the mean for each strata.
  sourceData <- group_by(sourceData, projID, studyear, communty, resource, strata) %>%
    mutate(mMean = mean((meanReplaced), na.rm=TRUE),
           cnt = n())

  # 1.4 Replace calculated means of 0 with NA to prevent coalesce from supply a value of 0.
  sourceData <- recode_variables(sourceData, c("mMean"), 0, NA)

  # 1.5 Coalesce these columns.
  sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$mMean)

  # 1.6 Impute at the strata level.
  sourceData$strataEst = sourceData$mMean * sourceData$commhh

  # 1.7 Summarize the imputed data to the community level and calculate the
  #     community mean.
  sourceData <- mutate(sourceData, commEst = sum(strataEst, na.rm=TRUE))
  sourceData$cMean = sourceData$commEst / sourceData$NHouseholds / sourceData$cnt

  # 1.8 For community mean, do the same thing we did above to ensure no 0 means
  #     are supplied.
  sourceData <- recode_variables(sourceData, c("cMean"), 0, NA)

  sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$cMean)

  # 1.9 If all else fails, use the minimum of '1' and returns the mean replaced
  #     column; but only if the household affirmatively indicated harvest
  #     for this species.
  sourceData$minimumReplace = 1
  sourceData$minimumReplace[is.na(sourceData$checkCol) |
                               sourceData$checkCol <= 0] = 0

  return(coalesce(sourceData$meanReplaced, sourceData$minimumReplace))
}


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


#' Replace missing column values in a data frame with diagnostic output
#'
#' @description
#' Mean replacement for stratified samples with detailed diagnostic tables. This function REQUIRES that
#'    sourceData contain: "projID", "studyear", "communty", "resource",
#'                        "strata", replCol, "commhh", "NHouseholds".
#'    Additionally, a 'check-column' is required, this might be
#'    "harvestq" (default) or "filterq" or other specified column to indicate
#'    whether or not a resource requires a non-zero amount (i.e., minimum replacement).
#'
#' This function implements a hierarchical replacement strategy:
#'
#' **First**, it tries strata-level means:
#'  - Groups by projID, studyear, communty, resource, and strata
#'  - Calculates mean within these fine-grained groups
#'
#' **Then**, it tries community-level means:
#'  - Groups by projID, studyear, communty, and resource
#'  - Uses these broader groups as a secondary fallback
#'  - Special case: if community mean equals zero, falls through to minimum replacement
#'
#' **Finally**, if all else fails:
#'  - Uses 1 as the replacement value (last resort)
#'
#' **Output:**
#'  - Displays a summary table with pre/post statistics for each column
#'  - Displays a second table tracking which replacement path (original/strata/community/fallback) was used and how many times
#'
#' @param sourceData A data frame containing multiple variables.
#' @param replCols A vector containing the variables from sourceData for which values will be mean replaced.
#' @param checkCol Character; name of column used to determine if minimum replacement applies. Default is "filterq".
#' @param verbose Logical; should diagnostic tables be rendered? Default is TRUE.
#' @returns A modified data frame containing mean replaced values in the specified variables, plus diagnostic tibbles if verbose=TRUE.
#'
#' @export

dfMeanReplaceStratified <- function(sourceData, replCols, checkCol = "filterq", verbose = TRUE) {
  # ===== VALIDATE INPUT COLUMNS =====
  # Ensure all required columns are present in the input data
  required_cols <- c("projID", "studyear", "communty", "resource",
                     "strata", "commhh", "NHouseholds", checkCol)
  all_cols <- c(required_cols, replCols)

  missing_cols <- setdiff(all_cols, names(sourceData))
  if (length(missing_cols) > 0) {
    stop(paste("Required columns not present:", paste(missing_cols, collapse=", ")))
  }

  # ===== INITIALIZE DEBUG TIBBLES =====
  # Create tibbles to accumulate diagnostic metrics across all columns
  debug_tbl <- tibble(
    Column = character(),
    Metric = character(),
    Value = character()
  )

  replacement_path_tbl <- tibble(
    Column = character(),
    Path = character(),
    Count = integer()
  )

  # ===== PROCESS EACH COLUMN FOR MEAN REPLACEMENT =====
  # Loop through each column specified in replCols
  for (col in replCols) {

    # ===== CALCULATE BEFORE-REPLACEMENT METRICS =====
    # Capture overall mean and NA count before any replacements
    before_mean <- mean(sourceData[[col]], na.rm = TRUE)
    before_na_count <- sum(is.na(sourceData[[col]]))

    # ===== CALCULATE STRATA-LEVEL MEANS =====
    # Group by project, study year, community, resource, and strata
    # to compute the mean value within each strata for this resource.
    # This is the finest level of grouping.
    temp_data <- sourceData

    temp_data <- temp_data %>%
      group_by(projID, studyear, communty, resource, strata) %>%
      mutate(strata_mean = mean(!!sym(col), na.rm = TRUE)) %>%
      ungroup()

    # Convert NaN (result of mean on all NAs) to NA for consistent handling
    temp_data$strata_mean[is.nan(temp_data$strata_mean)] <- NA

    # Extract unique strata means and count of NA strata means for reporting
    unique_strata_means <- paste(unique(round(temp_data$strata_mean, 3))[!is.na(unique(round(temp_data$strata_mean, 3)))], collapse = ", ")
    strata_mean_na_count <- sum(is.na(temp_data$strata_mean))

    # ===== CALCULATE COMMUNITY-LEVEL MEANS =====
    # Group by project, study year, community, and resource
    # to compute the mean value across the entire community for this resource.
    # This is a broader grouping used as a secondary fallback.
    temp_data <- temp_data %>%
      group_by(projID, studyear, communty, resource) %>%
      mutate(community_mean = mean(!!sym(col), na.rm = TRUE)) %>%
      ungroup()

    # Convert NaN (result of mean on all NAs) to NA for consistent handling
    temp_data$community_mean[is.nan(temp_data$community_mean)] <- NA

    # Extract unique community means and count of NA community means for reporting
    unique_community_means <- paste(unique(round(temp_data$community_mean, 3))[!is.na(unique(round(temp_data$community_mean, 3)))], collapse = ", ")
    community_mean_na_count <- sum(is.na(temp_data$community_mean))

    # ===== TRACK REPLACEMENT PATHS =====
    # Initialize counters for each replacement path before applying replacements
    path_original <- sum(!is.na(sourceData[[col]]))
    path_strata <- 0
    path_community <- 0
    path_fallback <- 0

    # ===== APPLY REPLACEMENT HIERARCHY =====
    # For each row, replace NA values using the following priority:
    # 1. Keep original value if not NA
    # 2. Use strata mean if available
    # 3. Use community mean if not equal to zero
    # 4. Use 1 as a last resort
    for (i in seq_len(nrow(temp_data))) {
      if (is.na(temp_data[[col]][i])) {
        if (!is.na(temp_data$strata_mean[i])) {
          temp_data[[col]][i] <- temp_data$strata_mean[i]
          path_strata <- path_strata + 1
        } else if (!is.na(temp_data$community_mean[i]) && temp_data$community_mean[i] != 0) {
          temp_data[[col]][i] <- temp_data$community_mean[i]
          path_community <- path_community + 1
        } else {
          temp_data[[col]][i] <- 1
          path_fallback <- path_fallback + 1
        }
      }
    }

    # Round the replaced values to 2 decimal places for consistency
    sourceData[[col]] <- temp_data[[col]] %>% round(digits = 2)

    # ===== CALCULATE AFTER-REPLACEMENT METRICS =====
    # Capture summary statistics after replacements to verify the operation was successful
    after_na_count <- sum(is.na(sourceData[[col]]))
    after_zero_count <- sum(sourceData[[col]] == 0, na.rm = TRUE)
    after_mean <- mean(sourceData[[col]], na.rm = TRUE)
    after_min <- min(sourceData[[col]], na.rm = TRUE)
    after_max <- max(sourceData[[col]], na.rm = TRUE)

    # ===== ACCUMULATE DEBUG OUTPUT IN SUMMARY TABLE =====
    # Build rows for this column with all calculated metrics
    debug_tbl <- debug_tbl %>% bind_rows(tibble(
      Column = c(col, rep("", 9)),
      Metric = c(
        "Overall mean (before)",
        "NA count (before)",
        "Unique strata means",
        "NA count in strata means",
        "Unique community means",
        "NA count in community means",
        "NA count (after)",
        "Zero count (after)",
        "Mean (after)",
        "Min/Max (after)"
      ),
      Value = c(
        as.character(round(before_mean, 3)),
        as.character(before_na_count),
        ifelse(unique_strata_means == "", "NA", unique_strata_means),
        as.character(strata_mean_na_count),
        ifelse(unique_community_means == "", "NA", unique_community_means),
        as.character(community_mean_na_count),
        as.character(after_na_count),
        as.character(after_zero_count),
        as.character(round(after_mean, 3)),
        paste(round(after_min, 3), "/", round(after_max, 3))
      )
    ))

    # ===== ACCUMULATE REPLACEMENT PATH TRACKING TABLE =====
    # Track which replacement path was used and how many times for each column
    replacement_path_tbl <- replacement_path_tbl %>% bind_rows(tibble(
      Column = c(col, col, col, col),
      Path = c("Original (not NA)", "Strata mean", "Community mean", "Fallback (1)"),
      Count = c(path_original, path_strata, path_community, path_fallback)
    ))
  }

  # ===== RENDER DEBUG TABLES =====
  # If verbose mode is enabled, display the accumulated diagnostic metrics as formatted tables
  if (verbose) {
    cat("\n")
    print(kable(debug_tbl,
                caption = "dfMeanReplaceStratified: Replacement Statistics",
                booktabs = TRUE))
    cat("\n")
    print(kable(replacement_path_tbl,
                caption = "dfMeanReplaceStratified: Replacement Paths Used",
                booktabs = TRUE))
  }

  # Return the source data with all replacements applied
  return(sourceData)
}

### THIS IS AN INCOMPLETE FUNCTION ----
#
# meanReplace <- function(sourceData, projectKeyList=c("projID","studyear","communty"),
#                           hhKeyList=c("HHID","strata"),
#                           detailKeyList=c("resource","units"),
#                           replCol)
# {
#
#   # 2.1 Required columns for mean replacement (minimum)
#   sNamesList <- c(projectKeyList, hhKeyList, detailKeyList, mrColumn)
#   symsNamesList <- syms(c(projectKeyList, hhKeyList, replCol))
#
#   for(kk in sNamesList)
#   {
#     if(!(kk %in% names(sourceData)))
#     {
#       print("ERROR,data not present")
#       error = c(str_interp("required column ${kk} not present in source data file"))
#       return(data.frame(error))
#     }
#   }
#   return(sourceData$mrColumn)
# }
#
# # Mean replacement by resource and one level of detail.
# #  This function is for SDS projects only, and assumes the
# #  presence of a stratification variable.
# replaceByResourceDetail <- function(sourceData, replCol, detailColumn)
# {
#   # 3.1 Required columns for mean replacement (minimum)
#   sNamesList <- c("projID", "studyear", "communty", "resource", "strata", "commhh", "NHouseholds", replCol, detailColumn)
#
#   symsDetailCol = syms(detailColumn)
#
#   # 3.2 Verify that required variables are present.
#   for(kk in sNamesList)
#   {
#     if(!(kk %in% names(sourceData)))
#     {
#       print("ERROR,data not present")
#       error = c(str_interp("required column ${kk} not present in source data file"))
#       return(data.frame(error))
#     }
#   }
#
#
#   # 3.3 Rename the target data to mean Replaced, this will simplify the remainder
#   #     of processing.
#   sourceData <- rename(sourceData, "meanReplaced"=all_of(replCol))
#
#   # 3.4 Develop mean estimate at the highest level of detail.
#   MR1Data <- group_by(sourceData, projID, studyear, communty, NHouseholds, commhh, resource, strata, !!!symsDetailCol) %>%
#     summarize(dMean = mean((meanReplaced), na.rm=TRUE))
#
#   # 3.4.1 Impute amount based on the means of known data.
#   MR1Data$estHarv = MR1Data$dMean * MR1Data$commhh
#
#   # 3.5 Sum imputed amounts to the resource level.
#   MR1bData <- summarize(MR1Data, estHarv = sum(estHarv, na.rm = TRUE))
#
#   # 3.5.1 Calculate average HH harvest for the overall resource level.
#   MR1bData$ndMean = MR1bData$estHarv / MR1bData$commhh
#
#   # 3.6 Summarize the detail data across strata groups.
#   MR2Data <- group_by(MR1Data, projID, studyear, communty, NHouseholds, resource, !!!symsDetailCol) %>%
#     summarize(estHarv = sum(estHarv, na.rm=TRUE))
#
#   # 3.6.1 Community overall, with detail, mean from the imputed harvest amounts.
#   MR2Data$cdMean = MR2Data$estHarv / MR2Data$NHouseholds
#
#   # 3.7 Summarize imputed data to create a community-wide estimate at the resource
#   #     level.
#   MR2bData <- group_by(MR1bData, projID, studyear, communty, NHouseholds, resource) %>%
#     summarize(estHarv = sum(estHarv, na.rm=TRUE))
#
#   # 3.7.1 Compute overall resource harvest mean at the resource level.
#   MR2bData$cndMean = MR2bData$estHarv / MR2bData$NHouseholds
#
#   #
#   # 3.8 Now we've computed all of the means, clean up the temp dataframes, and start
#   #     merging in order to execute the final mean replacement.
#   #
#   MR1Data <- delete_variables(MR1Data, c("estHarv", "commhh", "NHouseholds"))
#   MR1bData <- delete_variables(MR1bData, c("estHarv", "commhh", "NHouseholds"))
#   MR2Data <- delete_variables(MR2Data, c("estHarv", "NHouseholds"))
#   MR2bData <- delete_variables(MR2bData, c("estHarv", "NHouseholds"))
#
#   #
#   # 3.9 For the means, replace 0 with NA. We don't want to mean replace with a
#   #     value of 0.
#   #
#   MR1Data <- recode_variables(MR1Data, c("dMean"), 0, NA)
#   MR1bData <- recode_variables(MR1bData, c("ndMean"), 0, NA)
#   MR2Data <- recode_variables(MR2Data, c("cdMean"), 0, NA)
#   MR2bData <- recode_variables(MR2bData, c("cndMean"), 0, NA)
#
#   # 3.10 Merge all of the means into the main source data.
#   sourceData <- left_join(sourceData, MR1Data, by=c("projID", "studyear", "communty", "resource", "strata", detailColumn)) %>%
#     left_join(MR1bData, by=c("projID", "studyear", "communty", "resource", "strata")) %>%
#     left_join(MR2Data, by=c("projID", "studyear", "communty", "resource", detailColumn)) %>%
#     left_join(MR2bData, by=c("projID", "studyear", "communty", "resource"))
#
#   # 3.11 Coalesce the column with missing data with the means in order.
#   sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$dMean)
#   sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$ndMean)
#   sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$cdMean)
#   sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$cndMean)
#
#
#   # 3.12 Minimum replacement
#   sourceData$minimumReplace = 1
#
#   # 3.13 returns the mean-replaced column.
#   return(coalesce(sourceData$meanReplaced, sourceData$minimumReplace))
#
# }

#' replaceByResourceDetail
#'
#' @param sourceData A data frame
#' @param replCol Column with missing values
#' @param detailColumn Column that contains a grouping variable
#'
#' @returns A column with missing replaced values (mean or minimum)
#' @export
#'
# Mean replacement by resource and one level of detail.
#  This function is for SDS projects only, and assumes the
#  presence of a stratification variable.
replaceByResourceDetail <- function(sourceData, replCol, detailColumn)
{
  # 3.1 Required columns for mean replacement (minimum)
  sNamesList <- c("projID", "studyear", "communty", "resource", "strata", "commhh", "NHouseholds", replCol, detailColumn)

  symsDetailCol = syms(detailColumn)

  # 3.2 Verify that required variables are present.
  for(kk in sNamesList)
  {
    if(!(kk %in% names(sourceData)))
    {
      print("ERROR,data not present")
      error = c(str_interp("required column ${kk} not present in source data file"))
      return(data.frame(error))
    }
  }


  # 3.3 Rename the target data to mean Replaced, this will simplify the remainder
  #     of processing.
  sourceData <- rename(sourceData, "meanReplaced"=replCol)

  # 3.4 Develop mean estimate at the highest level of detail.
  MR1Data <- group_by(sourceData, projID, studyear, communty, NHouseholds, commhh, resource, strata, !!!symsDetailCol) %>%
    summarize(dMean = mean((meanReplaced), na.rm=TRUE))

  # 3.4.1 Impute amount based on the means of known data.
  MR1Data$estHarv = MR1Data$dMean * MR1Data$commhh

  # 3.5 Sum imputed amounts to the resource level.
  MR1bData <- summarize(MR1Data, estHarv = sum(estHarv, na.rm = TRUE))

  # 3.5.1 Calculate average HH harvest for the overall resource level.
  MR1bData$ndMean = MR1bData$estHarv / MR1bData$commhh

  # 3.6 Summarize the detail data across strata groups.
  MR2Data <- group_by(MR1Data, projID, studyear, communty, NHouseholds, resource, !!!symsDetailCol) %>%
    summarize(estHarv = sum(estHarv, na.rm=TRUE))

  # 3.6.1 Community overall, with detail, mean from the imputed harvest amounts.
  MR2Data$cdMean = MR2Data$estHarv / MR2Data$NHouseholds

  # 3.7 Summarize imputed data to create a community-wide estimate at the resource
  #     level.
  MR2bData <- group_by(MR1bData, projID, studyear, communty, NHouseholds, resource) %>%
    summarize(estHarv = sum(estHarv, na.rm=TRUE))

  # 3.7.1 Compute overall resource harvest mean at the resource level.
  MR2bData$cndMean = MR2bData$estHarv / MR2bData$NHouseholds

  #
  # 3.8 Now we've computed all of the means, clean up the temp dataframes, and start
  #     merging in order to execute the final mean replacement.
  #
  MR1Data <- delete_variables(MR1Data, c("estHarv", "commhh", "NHouseholds"))
  MR1bData <- delete_variables(MR1bData, c("estHarv", "commhh", "NHouseholds"))
  MR2Data <- delete_variables(MR2Data, c("estHarv", "NHouseholds"))
  MR2bData <- delete_variables(MR2bData, c("estHarv", "NHouseholds"))

  #
  # 3.9 For the means, replace 0 with NA. We don't want to mean replace with a
  #     value of 0.
  #
  MR1Data <- recode_variables(MR1Data, c("dMean"), 0, NA)
  MR1bData <- recode_variables(MR1bData, c("ndMean"), 0, NA)
  MR2Data <- recode_variables(MR2Data, c("cdMean"), 0, NA)
  MR2bData <- recode_variables(MR2bData, c("cndMean"), 0, NA)

  # 3.10 Merge all of the means into the main source data.
  sourceData <- left_join(sourceData, MR1Data, by=c("projID", "studyear", "communty", "resource", "strata", detailColumn)) %>%
    left_join(MR1bData, by=c("projID", "studyear", "communty", "resource", "strata")) %>%
    left_join(MR2Data, by=c("projID", "studyear", "communty", "resource", detailColumn)) %>%
    left_join(MR2bData, by=c("projID", "studyear", "communty", "resource"))

  # 3.11 Coalesce the column with missing data with the means in order.
  sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$dMean)
  sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$ndMean)
  sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$cdMean)
  sourceData$meanReplaced = coalesce(sourceData$meanReplaced, sourceData$cndMean)


  # 3.12 Minimum replacement
  sourceData$minimumReplace = 1

  # 3.13 Return the mean-replaced column.
  return(coalesce(sourceData$meanReplaced, sourceData$minimumReplace))

}
