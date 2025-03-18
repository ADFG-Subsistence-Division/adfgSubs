#' meanReplaceStratified
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
#' @param sourceData A data frame containing multiple variables.
#' @param replCol A vector containing the variables from sourceData for which values will be mean replaced.
#' @param checkCol Description
#' @returns A modified data frame containing mean replaced values in the specified variables.
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
      returns(data.frame(error))
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
  
  returns(coalesce(sourceData$meanReplaced, sourceData$minimumReplace))
}


### THIS IS AN INCOMPLETE FUNCTION
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
#       returns(data.frame(error))
#     }
#   }
#   returns(sourceData$mrColumn)
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
#       returns(data.frame(error))
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
#   returns(coalesce(sourceData$meanReplaced, sourceData$minimumReplace))  
#   
# }


