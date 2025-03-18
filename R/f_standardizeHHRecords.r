#' standardizeHHRecords
#'
#' @description
#' 1. Creates a 'template' record for each unique replication key value such that
#' all replication values are present for each household for a projID/studyear/communty
#' AND replication key values not appearing in a projID/studyear/communty
#'      are not added
#'
#' 2. Creates a set of all rows appearing in the dataset and containing the
#'    replication key, projectKey, and hhKey.
#'
#' 3. Loops Through each household
#' 
#' 4. Limits the list of replication keys to just those with matching projID, studyear, communty
#' 
#' 5. Adds the HHID and strata value to the template list
#' 
#' 6. Uses bind_rows into a placeholder data-frame.
#' 
#'
#' @param sourceData A data frame.
#' @param replicationKeyList The list of columns that make a row unique for a given
#' household and project in the supplied data frame. These 
#' columns may vary depending on project. The default is:
#' resource, units. This can be overridden, depending on the 
#' needs of the project.
#'
#' @returns A data frame in which all households (rows) have values for every resource column and unit column harvested.
#' 
#' @details
#' Note:
#'  Prior to standardizing resource codes, we must
#'  first have cleaned up files such that there are NO duplicate resource codes.
#'  NOTE that this code is generic. 
#'
#' Definitions:
#'  `replicationKeyList` is the list of columns that make a row unique for a given
#'  household and project in the supplied data frame. These 
#'  columns may vary depending on project. The default is:
#'  resource, units. This can be overridden, depending on the 
#'  needs of the project.
#'
#'  `hhKeyList` is the list of columns that specify a unique household for a given
#'  project in the supplied data frame. These columns are fixed across all
#'  projects coming from the SDS these are: strata, HHID
#'
#'  `projKeyList` contains columns that specify a unique project. These columns are 
#'  fixed across all projects coming from the SDS these are: 
#'  projID, studyear, communty
#'
#' @export
#'

standardizeHHRecords <- function(sourceData, replicationKeyList=c("resource", "units"))
{
  # ##############################################################################
  # 1.0 Validate function input & return errors to the calling script if required
  #     information is missing.
  # ##############################################################################
  
  # 1.1 Create list of required key information that is independent of
  #     any particular project using the SDS.
  projKeyList <- c("projID", "studyear", "communty")
  hhKeyList <- c("strata", "HHID")
  
  # 1.2 Create a full key list of columns that need to be present in the source
  #     data
  fullKeyList <- c(projKeyList, replicationKeyList, hhKeyList)
  
  # 1.3 Check the source data, which should be harvest records containing harvest amounts
  #      with units. If we don't have everything required print a message to the console
  #      and replace the return data with an error message. This way data processing
  #      can't continue until the issue has been resolved.
  for(kk in fullKeyList)
  {
    if(!(kk %in% names(sourceData)))
    { 
      print("ERROR, required column not present")
      error = c(str_interp("required column ${kk} not present in source data"))
      return(data.frame(error))
    }
  }
  
  # ##############################################################################
  # 2.0 - Produce list and datasets necessary to create a normalized list of
  #       replication keys for each household.
  # ##############################################################################  
  
  # 2.1 This converts the variable list into symbols we can use with
  #     tidyverse functions.
  RKList <- c(projKeyList, replicationKeyList)
  symsRKList = syms(RKList)
  
  # 2.2 Get a distinct list of replication keys. ie: Keys values that must
  #     be present for each household. This will reduce the file to just
  #     the project key and replication key information, eliminating the
  #     household key. The count of occurrences will be stored in keyOccr as as
  #     a placeholder.
  replData <- group_by(sourceData, !!!symsRKList) %>%
    summarize(keyOccr = n())
  replData <- delete_variables(replData, c("keyOccr"))
  
  # 2.4 Create a list of households with all relevant key information 
  #     included except the replication key.
  uHHData <- group_by(sourceData, !!!syms(c(projKeyList, hhKeyList))) %>%
    summarize(nOccur = n())
  uHHData <- delete_variables(uHHData, c("nOccur"))
  
  # 2.5 - Empty dataframe to house set of 'template' records
  pReplData <- filter(replData, projID == 0 & studyear == 0 & communty == 0)
  
  # 2.6 - Loop through each of the unique households and build up the set of
  #       desired template records.
  for(ii in 1:nrow(uHHData)){
    # 2.6.1 just the 'current' HH
    tHHData <- uHHData[ii,]
    HHID = tHHData$HHID
    strata = tHHData$strata
    # 2.6.2 Limit to relevant rows only.
    tReplData <- filter(replData, projID == tHHData$projID &
                          studyear == tHHData$studyear &
                          communty == tHHData$communty)
    # 2.6.3 Merge the household key info into the data frame
    tReplData <- data.frame(tReplData, HHID, strata)
    # 2.6.4 combine into template dataframe.
    pReplData <- dplyr::bind_rows(pReplData, tReplData)
  }
  
  # 2.7 - Now merge the given data into the template.
  sourceData <- left_join(pReplData, sourceData, by=fullKeyList)
}

# placeholder function so that if someone tries to run analysis during testing they
#  get a message.
standardizeHHRecordsPH <- function(sourceData, replicationKeyList=c("resource", "units")) {
  print("ERROR, This function is being revised - contact D.Koster")
  error = c(str_interp("Function is not available right now. Contact D. Koster for details"))
  return(data.frame(error))
}
  
standardizeHHRecordsOriginal <- function(sourceData, replicationKeyList=c("resource", "units")){
  
  # ##############################################################################
  # 1.0 Validate function input & return errors to the calling script if required
  #     information is missing.
  # ##############################################################################
  
  # 1.1 Create list of required key information that is independent of
  #     any particular project using the SDS.
  projKeyList <- c("projID", "studyear", "communty")
  hhKeyList <- c("strata", "HHID")
  
  # 1.2 Create a full key list of columns that need to be present in the source
  #     data
  fullKeyList <- c(projKeyList, replicationKeyList, hhKeyList)
  
  # 1.3 Check the source data, which should be harvest records containing harvest amounts
  #      with units. If we don't have everything required print a message to the console
  #      and replace the return data with an error message. This way data processing
  #      can't continue until the issue has been resolved.
  for(kk in fullKeyList)
  {
    if(!(kk %in% names(sourceData)))
    { 
      print("ERROR, required column not present")
      error = c(str_interp("required column ${kk} not present in source data"))
      return(data.frame(error))
    }
  }
  
  # ##############################################################################
  # 2.0 - Produce list and datasets necessary to create a normalized list of
  #       replication keys for each household.
  # ##############################################################################  
  
  # 2.1 This converts the variable list into symbols we can use with
  #     tidyverse functions.
  RKList <- c(projKeyList, replicationKeyList)
  symsRKList = syms(RKList)
  
  # 2.2 Get a distinct list of replication keys. ie: Keys values that must
  #     be present for each household. This will reduce the file to just
  #     the project key and replication key information, eliminating the
  #     household key. The count of occurrences will be stored in keyOccr as as
  #     a placeholder.
  replData <- group_by(sourceData, !!!symsRKList) %>%
             summarize(keyOccr = n())
  
  # 2.3 Remove superfluous column added above.
  replData <- delete_variables(replData, c("keyOccr"))
  
  
  #
  # FIX - Need to create a dataframe to step through that contains
  #       ONLY the projKey and HHKey. We step through these indivdiually
  #       and then add all of the records for each of those datasets.
  #       Perhaps we can use THIS function as the main call and then 
  #       move the guts of this function to another function we can call
  #       per community, then build up records as-needed.
  #
  
  # 2.4 Create a dataframe from the source data containing ONLY project
  #     and household key information.
  HKList <- c(projKeyList, hhKeyList)
  symsHKList <- syms(HKList)
  hhData <- group_by(sourceData, !!!symsHKList) %>%
    summarize(keyOccr = n())

  # 2.5 Remove extraneous column added above.  
  hhData <- delete_variables(hhData, c("keyOccr"))
  
  # 2.6 Create an empty data frame we will use to build the final keylist.
  #symsFullKeyList <- syms(fullKeyList)
  keyData <- select(sourceData, !!!syms(fullKeyList)) %>% filter(projID == 0)
  
  # ##############################################################################
  # 3.0 - Using lists and datasets produced above, create an empty data frame
  #       containing only the key information that will be merged with the
  #       sourceData.
  #       This bit of code loops through each of the replication keys required for
  #       each household, and then adds that to the list of households
  #       each iteration is appended to the list that came prior, building up
  #       the data frame containing the full key list item by item.
  # ##############################################################################    
  
  # 3.1 Loop through each row in replData using ii as an index variable.
  for(ii in 1:nrow(replData))
  {
    # 3.1.1 get the individual row of data as a dataFrame
    tResData <- replData[ii,]
    # 3.1.2 merge the household data with the replication data, using
    #       the project key columns to join on.
    hhReplData <- left_join(hhData, tResData, by=projKeyList)
    # append the households with the additional, standardized info to the 
    #   primary list.
    keyData <- dplyr::bind_rows(keyData, hhReplData)
  }
  
  # ##############################################################################
  # 4.0 - Join the source data back into the newly standardized list of data.
  # ##############################################################################
  sourceData <- left_join(keyData, sourceData, by=fullKeyList)
  
  return(sourceData)
}
