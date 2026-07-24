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
