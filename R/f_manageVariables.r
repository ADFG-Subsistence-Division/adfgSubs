#' delete_variables
#' 
#' @description
#' Removes unnecessary variables from a data frame.
#' 
#' Replicates SPSS functions for:
#'   'DELETE VARIABLES'
#'    /KEEP (when saving)
#'   'RENAME VARIABLES'
#'   'RECODE x .. y (a = b).'
#' 
#' @param dataSetD A data frame containing multiple variables.
#' @param variablesToDelete A vector containing the variables to be deleted from @param dataSetD.
#' @return A modified data frame with variables named in @param variablesToDelete removed.
#' @export

delete_variables <- function(dataSetD,variablesToDelete){
  dataSetD <- dataSetD[,!(names(dataSetD) %in% variablesToDelete)]
  
  return(dataSetD)
}

#' keep_variables
#' 
#' Retains only specified variables from a data frame.
#' 
#' @param dataSetK A data frame containing multiple variables.
#' @param variablesToKeep A vector containing the variables to be retained from @param dataSetK. All other variables will be deleted.
#' @return A modified data frame with only variables named in @param variablesToKeep retained.
#' @export

keep_variables <- function(dataSetK, variablesToKeep){
  dataSetK <- dataSetK[ , (names(dataSetK) %in% variablesToKeep)]
  
  return(dataSetK)
}

#' inf_to_NA
#' 
#' Recodes variables with 'inf' values to 'NA'.
#' 
#' @param dataSetI A data frame containing multiple variables.
#' @param variableList A vector containing the variables to be recoded.
#' @return A modified data frame with 'inf' values recoded to 'NA' in variables specified by @param variableList.
#' @export

inf_to_NA <- function(dataSetI, variableList)
{

  dataSetI <- dataSetI %>% 
    mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(is.infinite(.), NA, .)))

  return(dataSetI)
}

#' nan_to_NA
#' 
#' Recodes variables with 'NaN' values to 'NA'.
#'
#' @param dataSetN A data frame containing multiple variables.
#' @param variableList A vector containing the variables to be recoded.
#' @return A modified data frame with 'NaN' values recoded to 'NA' in variables specified by @param variableList.
#' @export


nan_to_NA <- function(datasetN, variableList){
  
  datasetN <- datasetN %>% 
    mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(is.nan(.), NA, .)))
  
  return(datasetN)
}

#' recode_variables
#' 
#' Recodes variables.
#'
#' @param dataSetR A data frame containing multiple variables.
#' @param variableList A vector containing the variables to be recoded.
#' @param oldValue Existing value to be replaced.
#' @param newValue Value used to replace @param oldValue
#' @return A modified data frame with old values recoded to new values in variables specified by @param variableList.
#' @export

recode_variables <- function(dataSetR, variableList, oldValue, newValue) {
  # First, we have to establish whether or not the first value is NA
  if(is.na(oldValue)) {
    dataSetR <- dataSetR %>% 
      mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(is.na(.), newValue, .)))
  } else {
    if(is.na(newValue)) {
      dataSetR <- dataSetR %>% 
        mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(. == oldValue, NA, .)))
    } else {
        dataSetR <- dataSetR %>% 
        mutate (. , across(.cols = all_of(variableList), .fns = ~ifelse(. == oldValue, newValue, .)))
    }
  }
  
  return(dataSetR)
}

#' assign_value_variables
#' 
#' Replace whatever is in the specified columns with the assigned value, without regard for the current contents.
#'
#' @param dataSetA A data frame containing multiple variables.
#' @param variableList A vector containing the variables to be recoded.
#' @param newValue The value that will be assigned to variables specified in @param variableList.
#' @return A modified data frame with new values in variables specified by @param variableList.
#' @export


assign_value_variables <- function(dataSetA, variableList, newValue) {
  dataSetA <- dataSetA %>% mutate (. , across(.cols = all_of(variableList), ~ newValue))

    return(dataSetA)
}

#' get_code_labels
#' 
#' Description
#'
#' @param codesToFind Description
#' @param codeColumnName Description
#' @param valueColumnName Description
#' @param codeList Description 
#' @return Description
#' @export

get_code_labels <- function(codesToFind, codeColumnName, valueColumnName, codeList)
{
  sNamesList = c(codeColumnName, valueColumnName)
  for(kk in sNamesList)
  {
    if(!(kk %in% names(codeList)))
    { 
      print("ERROR,data not present")
      error = c(str_interp("required column ${kk} not present in codeList"))
      return(data.frame(error))
    }
  }
  
  codeList <- rename(codeList, "codeLabel"=valueColumnName)
  
  newCode <- left_join(data.frame(codesToFind), codeList, by=c("codesToFind"=codeColumnName))
  
  return(newCode$codeLabel)
  
}

#' normalizeColumn
#' 
#' Description
#'
#' @param vec Description
#' @return Description
#' @export

normalizeColumn <- function(vec)
{
  return ((vec - min(vec)) / (max(vec) - min(vec)))
}
  


