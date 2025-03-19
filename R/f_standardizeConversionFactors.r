#' standardizeConvFact
#'
#' Standardized conversion factors function; generalized to be applicable to all harvest record types, provided a given format.
#'
#' @param sourceData A data frame. Must contain the following columns: "projID", "studyear", "communty", "resource", "units"
#' @param convData A data frame containing conversion factors. Must contain the following columns: "projID", "studyear", "communty", "resource", "units", "convFact","defaultUnits"
#' @param variableList A vector of variable names for which values will be converted.
#'
#' @returns A data frame containing variables converted using specified conversion factors.
#' @export
#'
standardizeConvFact <- function(sourceData, convData, variableList){
  # ##############################################################################
  # 1.0 Validate function input & return errors to the calling script if required
  #     information is missing.
  # ##############################################################################

  # 1.1 - Define the lists of required information for each input data frame.
  sNamesList <- c("projID", "studyear", "communty", "resource", "units")
  cNamesList <- c("projID", "studyear", "communty", "resource", "units", "convFact","defaultUnits")

  # 1.2 Check the source data
  for(kk in sNamesList) {
    if(!(kk %in% names(sourceData))) {
      print("ERROR: data not present")
      error = c(str_interp("required column ${kk} not present in source data file"))
      return(data.frame(error))
    }
  }

  # 1.3 Check the conversion factor data
  for(kk in cNamesList) {
    if(!(kk %in% names(convData))) {
      print("ERROR: data not present")
      error = c(str_interp("required column ${kk} not present in conversion factor data"))
      return(data.frame(error))
    }
  }

  # ##############################################################################
  # 2.0 - merge files and complete conversion.
  # ##############################################################################

  # 2.1 Merge the data frames (FIXED: using convData instead of convFactData)
  sourceData <- left_join(sourceData, convData, by=c("projID", "studyear", "communty", "resource", "units"))

  # 2.2 Create a standard Units conversion 'sUnits'
  sourceData$sUnits = sourceData$convFact * sourceData$lbsToDefault

  # 2.3 Split into two data frames for processing
  stdData <- filter(sourceData, units == defaultUnits)
  nStdData <- filter(sourceData, units != defaultUnits)

  # 2.4 Compute standardized amounts
  if(nrow(nStdData) > 0) {
    nStdData <- mutate(nStdData, across(all_of(variableList), ~ . * sUnits))
  }

  # 2.5 Combine the data frames back together
  sourceData <- bind_rows(stdData, nStdData)

  return(sourceData)
}

