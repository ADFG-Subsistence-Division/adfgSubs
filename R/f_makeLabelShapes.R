#' Assign Plot Shapes to Labels Based on Pattern Matching
#'
#' This function maps categorical labels to plot shapes (as integers) by matching
#' each label against a series of regex patterns and returning the shape associated
#' with the first matching pattern. Useful for creating consistent shape schemes in
#' visualizations where shapes should be assigned based on label characteristics.
#'
#' @param labels A character vector of labels to be assigned shapes.
#' @param patterns A character vector of regular expressions (Perl-compatible) to
#'   match against labels. Matching is performed sequentially, and the first match
#'   determines the assigned shape.
#' @param shapes A character or integer vector of plot shape values corresponding
#'   to each pattern. Values will be coerced to integers. Must have the same length
#'   as \code{patterns}.
#'
#' @return A named integer vector where names are the input \code{labels} and
#'   values are the assigned plot shapes. Labels with no matching pattern are
#'   assigned \code{NA_integer_}.
#'
#' @details
#' Pattern matching is performed sequentially using \code{\link[base]{grepl}} with
#' Perl-compatible regular expressions. The function returns the shape associated
#' with the first pattern that matches a given label. If a label matches multiple
#' patterns, only the first match is used.
#'
#' Shape values are standard ggplot2 point shapes (0-25), where common values include:
#' 16 (filled circle), 17 (filled triangle), 18 (filled diamond), 15 (filled square).
#'
#' @examples
#' labels <- c("harvest_HH2024", "survey_data", "harvest_HH2025", "other_file")
#' patterns <- c("^harvest", "^survey")
#' shapes <- c(16, 17)
#'
#' makeLabelShapes(labels, patterns, shapes)
#'
#' @export

makeLabelShapes <- function(labels, patterns, shapes) {
  if(length(patterns) != length(shapes)) {
    stop("patterns and shapes must have the same length")
  }
  get_match <- function(label) {
    for(i in seq_along(patterns)) {
      if(grepl(patterns[i], label, perl=TRUE)) return(as.integer(shapes[i]))
    }
    return(NA_integer_)
  }
  shape_vec <- vapply(labels, get_match, integer(1))
  names(shape_vec) <- labels
  shape_vec
}
