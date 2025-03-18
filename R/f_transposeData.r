#' transposeForTable
#'
#' @description 
#' 
#' Given a data frame, the name of a header column, and rounding precision
#' (which defaults to 2), this code transposes rows of data into columns.
#' The data are converted into character values to satisfy R data-requirements.
#' This code should only be used for organization of display data for
#' R-markdown or sometimes for output of data files that are presented
#' in a transposed format, such as the sample tables.
#' 
#' @param ttData A small data frame for producing a table.
#' @param headerCol A row id column. Default = NA.
#' @param roundPrecision The number of digits to which variables should be rounded. Default = 2.
#'
#' @returns A transposed data frame.
#' @export
#'

transposeForTable <- function(ttData, headerCol = NA, roundPrecision=2) {

  # Start by determining which column is going to be the header OR
  #   using a sequence from 1:n
  #   Note that if we've determined that the selected header column is 
  #   not unique, then we will also assign a number from 1:n.
  
  if(is.na(headerCol)){
    hdrName <- as.character(seq(1:nrow(ttData)))
  } else {
    
    ctHeaders <- group_by(sampData, commname) %>%
      summarize(nh = n())
    maxCTHeaders <- max(ctHeaders$nh)
    if(maxCTHeaders > 1) {
      hdrName <- as.character(seq(1:nrow(ttData)))
    } else
    {
      namesVec <- names(ttData)
      idxHdr <- which(namesVec == headerCol)
      hdrName <- ttData[,idxHdr]
      ttData <- ttData[,-idxHdr]
    }
    
  }
  
  namesVec <- names(ttData)
  
  # Round and convert to character.
  for(ii in 1:length(namesVec))
  {
    if(class(ttData[,ii])=="numeric") {
      ttData[,ii] = base::round(ttData[,ii], digits=roundPrecision)
    }
    ttData[,ii] = as.character(ttData[,ii])
  }
  
  ttDF = data.frame("col" = namesVec)
  
  #Now pivot.
  for(jj in 1:nrow(ttData)){
    rVec <- unlist(unname(ttData[jj,]))
    ttDF[,hdrName[jj]] = rVec
  }
  
  return(ttDF)
}
