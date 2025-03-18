#' standardizeColumns
#' 
#' Looks through each of the columns in a data frame and if the column isn't present, gives it the value of replaceValue.
#' 
#' @param sourceData A data frame.
#' @param colList A vector of column names containing variables to be standardized with replacement values.
#' @param replaceValue A vector containing a replacement value; default = 0.
#'
#' @returns A data frame with new columns containing the replacement value in each row.
#' @export
#'
standardizeColumns <- function(sourceData, colList, replaceValue=0){

  for(kk in colList)
  {
    if(!(kk %in% names(sourceData)))
    { 
      ks = enquo(kk)
      sourceData$newCol = replaceValue
      sourceData <- rename(sourceData, !!ks := "newCol")
    }
  }
  
  return(sourceData)
}

#' standardizeColumns_
#' 
#' Efficient version of `standardizeColumns()`. Looks through each of the columns in a data frame and if the column isn't present, gives it the value of replaceValue.
#' 
#' @param sourceData A data frame.
#' @param colList A vector of column names containing variables to be standardized with replacement values.
#' @param replaceValue A vector containing a replacement value; default = 0.
#'
#' @returns A data frame with new columns containing the replacement value in each row.
#' @export
#'

standardizeColumns_ <- function(sourceData, colList, replaceValue = 0) {
  for (kk in colList) {
    if (!(kk %in% names(sourceData))) { 
      sourceData[[kk]] <- replaceValue
    }
  }
  return(sourceData)
}

