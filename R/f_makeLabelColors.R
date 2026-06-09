#' Assign Colors to Labels Based on Pattern Matching
#'
#' This function maps categorical labels to colors by matching each label against
#' a series of regex patterns and returning the color associated with the first
#' matching pattern. Useful for creating consistent color schemes in visualizations
#' where colors should be assigned based on label characteristics.
#'
#' @param labels A character vector of labels to be assigned colors.
#' @param patterns A character vector of regular expressions (Perl-compatible) to
#'   match against labels. Matching is performed sequentially, and the first match
#'   determines the assigned color.
#' @param colors A character vector of color values (e.g., hex codes, color names)
#'   corresponding to each pattern. Must have the same length as \code{patterns}.
#'
#' @return A named character vector where names are the input \code{labels} and
#'   values are the assigned colors. Labels with no matching pattern are assigned
#'   \code{NA}.
#'
#' @details
#' Pattern matching is performed sequentially using \code{\link[base]{grepl}} with
#' Perl-compatible regular expressions. The function returns the color associated
#' with the first pattern that matches a given label. If a label matches multiple
#' patterns, only the first match is used.
#'
#' @examples
#' labels <- c("harvest_HH2024", "survey_data", "harvest_HH2025", "other_file")
#' patterns <- c("^harvest", "^survey")
#' colors <- c("#e74c3c", "#3498db")
#'
#' makeLabelColors(labels, patterns, colors)
#'
#' @export
#'
makeLabelColors <- function(labels, patterns, colors) {
  # Check lengths
  if(length(patterns) != length(colors)) {
    stop("patterns and colors must have the same length")
  }
  # For each label, find the first matching pattern
  get_match <- function(label) {
    for(i in seq_along(patterns)) {
      if(grepl(patterns[i], label, perl=TRUE)) return(colors[i])
    }
    return(NA)  # No pattern matched
  }
  color_vec <- vapply(labels, get_match, character(1))
  names(color_vec) <- labels
  color_vec
}
