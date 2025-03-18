#' verifyColumns
#'
#' Look through each of the columns and if the column isn't present, return an error message.
#' 
#' @param sourceData A data frame.
#' @param colList A vector containing column names to check for.
#'
#' @returns Returns an error if columns are missing.
#' @export
#'

verifyColumns <- function(sourceData, colList){
  
  for(kk in colList)
  {
    if(!(kk %in% names(sourceData)))
    { 
      #print("ERROR: data not present")
      error = c(str_interp("required column ${kk} not present in source data file"))
      return(data.frame(error))
    }
  }
  
  return(sourceData)
}

