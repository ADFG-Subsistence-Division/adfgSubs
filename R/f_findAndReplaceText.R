#' Find and replace text in multiple files or folders
#'
#' Searches specified files or all files in given folders, replacing occurrences of search strings with replacement strings. Intended for batch processing of text across multiple files, such as R Markdown documents.
#'
#' @param files Character vector of file paths to process. If \code{NULL}, files are determined from \code{folders}.
#' @param folders Character vector of folder paths. All files matching \code{pattern} within these folders (and subfolders, if \code{recursive = TRUE}) will be processed.
#' @param pattern Regular expression pattern for files to include (default: \code{"\\.Rmd$"}).
#' @param find Character vector of search strings.
#' @param replace Character vector of replacement strings (must be same length as \code{find}).
#' @param fixed Logical, use fixed (literal) matching if \code{TRUE} (default), or regular expressions if \code{FALSE}.
#' @param recursive Logical, if \code{TRUE} (default), search folders recursively.
#' @param verbose Logical, display messages for each file (default: \code{TRUE}).
#'
#' @return Invisibly returns a data frame detailing the status of each file processed.
#'
#' @examples
#'
#' findAndReplaceText(
#'   folders = c("docs"),
#'   find = c("oldtext1", "oldtext2"),
#'   replace = c("newtext1", "newtext2")
#' )
#'
#' @export
findAndReplaceText <- function(
    files = NULL,
    folders = NULL,
    pattern = "\\.Rmd$",
    find,
    replace,
    fixed = TRUE,
    recursive = TRUE,
    verbose = TRUE
) {
  # Check arguments
  if (length(find) != length(replace)) {
    stop("'find' and 'replace' must be the same length.")
  }
  if (!is.character(find) || !is.character(replace)) {
    stop("'find' and 'replace' must be character vectors.")
  }

  # Collect files from folders if specified
  if (!is.null(folders)) {
    files_from_folders <- unlist(
      lapply(folders, function(dir) list.files(dir, pattern = pattern, full.names = TRUE, recursive = recursive))
    )
    files <- unique(c(files, files_from_folders))
  }
  files <- files[!is.na(files) & file.exists(files)] # filter valid files
  if(length(files) == 0) {
    if(verbose) message("No files found.")
    return(invisible(NULL))
  }

  results <- data.frame(
    file = files,
    status = NA_character_,
    message = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(files)) {
    file <- files[i]
    res <- tryCatch({
      lines <- readLines(file, warn = FALSE)
      original_lines <- lines
      # Apply replacements, pairwise
      for (j in seq_along(find)) {
        lines <- gsub(find[j], replace[j], lines, fixed = fixed)
      }
      changed <- !identical(original_lines, lines)
      if (changed) {
        writeLines(lines, file)
        if (verbose) message(sprintf("Replaced text in: %s", file))
        results$status[i] <- "updated"
        results$message[i] <- "Text replaced"
      } else {
        if (verbose) message(sprintf("No matching text found in: %s", file))
        results$status[i] <- "skipped"
        results$message[i] <- "No matching text"
      }
    }, error = function(e) {
      warning(sprintf("Error processing %s: %s", file, e$message))
      results$status[i] <- "error"
      results$message[i] <- e$message
    })
  }
  invisible(results)
}
