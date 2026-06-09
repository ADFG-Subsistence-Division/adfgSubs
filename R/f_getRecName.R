#' Extract the record name from the current Rmd
#'
#' @returns The name of the record (e.g., "REC04")
#' @export
#'
#' @examples
#'
getRecName <- function() {

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
