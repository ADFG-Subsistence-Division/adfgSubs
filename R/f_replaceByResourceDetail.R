#' # Mean replacement by resource and one level of detail
#'
#' @description
#' This function is for SDS projects only, and assumes the presence of a stratification variable.
#'
#' @param sourceData A data frame
#' @param replCol Column with missing values
#' @param detailColumn Column that contains a grouping variable
#'
#' @returns A column with missing replaced values (mean or minimum)
#' @export

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
