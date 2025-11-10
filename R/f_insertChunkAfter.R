#' Insert a new chunk after a specified code chunk in Rmd files
#'
#' This function searches R Markdown (\code{.Rmd}) files in a specified directory and inserts new content (header, text, and/or an R chunk)
#' immediately after a code chunk with a specified label.
#'
#' @param path Character. Path to directory containing files to process. Default is current directory (\code{"."}).
#' @param pattern Character. Regex pattern to match files. Default is \code{"\\.Rmd$"}.
#' @param after_chunk_label Character. The label of the code chunk after which the new content should be inserted (e.g., \code{"setup"} for a chunk labeled \code{r setup}).
#' @param new_header Character vector or NULL. Optional. Section header(s) to insert after the chunk (e.g., \code{"## New Step"}).
#' @param new_text Character vector or NULL. Optional. Descriptive text to insert after the header.
#' @param new_chunk Character vector or NULL. Optional. Lines of a new code chunk to insert.
#'
#' @return Invisibly returns NULL. Used for its side effect of editing files in place.
#' @examples
#'
#' #Example usage:
#'
#' newHeader <- c("## Conversion factors for unspecified resources")
#' newText <- c(
#'   "Resources are often reported as 'unspecified', in cases where the survey respondent",
#'   "was unsure of the species harvested or received. To calculate the edible weight of",
#'   "unspecified resources harvested, similar resources and the amounts/weights harvested",
#'   "are used to calculate the conversion factor for unspecified resources. Analysts must",
#'   "develop a list of resources to use in this calculation for each unspecified resource."
#' )
#' newChunk <- c()
#' insertChunkAfter(
#'   path = "analysis",
#'   after_chunk_label = "standardize units",
#'   new_header = newHeader,
#'   new_text = newText,
#'   new_chunk = newChunk
#' )
#'
#' @export
insertChunkAfter <- function(
    path = ".",
    pattern = "\\.Rmd$",
    after_chunk_label,
    new_header = NULL,
    new_text = NULL,
    new_chunk = NULL
) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)

  for (file in files) {
    lines <- readLines(file)
    # Find start of target chunk by label
    chunk_starts <- grep(paste0("^```\\{r[ ,].*", after_chunk_label, ".*\\}"), lines)
    if (length(chunk_starts) == 0) {
      message(sprintf("Chunk label '%s' not found in %s. Skipping.", after_chunk_label, file))
      next
    }
    # Find end of that chunk (the first ``` after the chunk start)
    chunk_ends <- sapply(chunk_starts, function(start) {
      end <- which((seq_along(lines) > start) & grepl("^```$", lines))[1]
      if (!is.na(end)) return(end) else return(NA)
    })
    # Only proceed if both start and end are found
    if (any(is.na(chunk_ends))) {
      message(sprintf("Could not find end of chunk after '%s' in %s. Skipping.", after_chunk_label, file))
      next
    }
    # We'll only process the first occurrence for simplicity
    insert_pos <- chunk_ends[1]
    # Prepare insertion lines
    insertion <- c()
    if (!is.null(new_header)) insertion <- c(insertion, new_header)
    if (!is.null(new_text)) insertion <- c(insertion, new_text)
    if (!is.null(new_chunk)) insertion <- c(insertion, new_chunk)
    # Insert after the found chunk end
    new_lines <- append(lines, insertion, after = insert_pos)
    writeLines(new_lines, file)
    message(sprintf("Inserted new chunk after '%s' in %s.", after_chunk_label, file))
  }
  invisible(NULL)
}
