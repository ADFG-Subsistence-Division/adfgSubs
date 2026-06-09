getRECName <- function() {
  
  # Name of this .Rmd
  if (!is.null(knitr::current_input())) {
    rmdName <- basename(knitr::current_input())
  } else if (requireNamespace("rstudioapi", quietly = TRUE) &&
             rstudioapi::isAvailable()) {
    rmdName <- basename(rstudioapi::getActiveDocumentContext()$path)
  } else {
    rmdName <- NA_character_
  }
  
  recName <- sub(".*_", "", sub("\\.Rmd$", "", rmdName))
  return(recName)
}