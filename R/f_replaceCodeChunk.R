#' Replace the code in a specified chunk in Rmd files
#'
#' @param path a vector of folder paths containing files where code chunk will be replaced
#' @param pattern the file type to be modified; default is ".Rmd"
#' @param exclude vector of file names to exclude
#' @param chunk_label code chunk label, e.g., "write csv files"
#' @param new_code new code to replace existing code in chunk, as a vector of lines (no chunk header/footer)
#' @param rec_pattern extracts the record type from the existing code chunk
#' @param rec_placeholder placeholder record type vector that is replaced by the extracted record type
#'
#' @returns Invisibly returns NULL. Used for its side effect of editing files in place.
#' @export
#'
#' @examples
#'
#' # Example usage
#'
#' Can specify full path names or paths relative to root directory.
#'
#' foldersAmbler <- c(
#' "../Step 1 - Extraction and Validation",
#' "../Step 2 - Standardize and Validate",
#' "../Step 3 - Demographics",
#' "../Step 4 - Income and Employment",
#' "../Step 5 - Harvest processing",
#' "../Step 6 - Assessments and summary")
#'
#' New code stored as quoted text in vector. Note: any text or characters
#'  being pasted into chunk should use single quotes.
#'
#' newCode <- c("# Set some knit options and functions for formatting data.
#' knitr::opts_chunk$set(echo = FALSE,
#'                       include = TRUE,
#'                       message = FALSE,
#'                       warning = FALSE,
#'                       results='asis')
#'
#' options(knitr.kable.NA = '')"
#' )
#'
#' replaceCodeChunks(path = foldersAmbler,
#' chunk_label = "setup",
#' new_code = newCode)
#'
#'

replaceCodeChunk <- function(
    path = ".",
    pattern = "\\.Rmd$",
    exclude = NULL,           # vector of filenames to exclude
    chunk_label = NULL,       # code chunk label, e.g., "write csv files"
    new_code = NULL,          # new code, as a vector of lines (no chunk header/footer)
    rec_pattern = "REC[0-9]{2}",
    rec_placeholder = "REC_PLACEHOLDER"
) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (!is.null(exclude)) {
    files <- files[!basename(files) %in% exclude]
  }

  for (file in files) {
    lines <- readLines(file)
    # Find chunk start by label
    chunk_start <- grep(paste0("^```\\{r[ ,].*", chunk_label, ".*\\}"), lines)
    if (length(chunk_start) == 0) {
      message(sprintf("Chunk label '%s' not found in %s. Skipping.", chunk_label, file))
      next
    }
    # Find chunk end (first ``` after chunk_start)
    chunk_end <- which((seq_along(lines) > chunk_start) & grepl("^```$", lines))[1]
    if (is.na(chunk_end)) {
      message(sprintf("Could not find end of chunk after '%s' in %s. Skipping.", chunk_label, file))
      next
    }
    # Extract RECxx value from the old chunk body
    chunk_body <- lines[(chunk_start+1):(chunk_end-1)]
    rec_matches <- regmatches(chunk_body, gregexpr(rec_pattern, chunk_body))
    rec_value <- unique(unlist(rec_matches))
    if (length(rec_value) > 0) {
      rec_value <- rec_value[1]
      # Replace placeholder in new code with the actual RECxx value
      new_code_fixed <- gsub(rec_placeholder, rec_value, new_code)
    } else {
      new_code_fixed <- new_code
    }
    # Rebuild lines: everything before chunk_start, chunk header, new_code_fixed, chunk footer, everything after chunk_end
    new_lines <- c(
      lines[1:(chunk_start-1)],
      lines[chunk_start],   # chunk header (once)
      new_code_fixed,
      lines[chunk_end]      # chunk footer (once)
    )
    if (chunk_end < length(lines)) {
      new_lines <- c(new_lines, lines[(chunk_end+1):length(lines)])
    }
    writeLines(new_lines, file)
    message(sprintf("Updated chunk '%s' in %s.", chunk_label, file))
  }
}
