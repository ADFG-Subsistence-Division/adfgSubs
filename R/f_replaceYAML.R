#' Replace the YAML header in Rmd files
#'
#' @param files a vector of .Rmd file paths for which the YAML header will be replaced
#' @param folders a vector of .Rmd folder paths for which the YAML header will be replaced
#' @param pattern the file type to be modified; default is ".Rmd"
#' @param author the name of the author to be updated in the YAML header
#' @param date the date to be updated in the YAML header
#' @param html_lines extra HTML code to be added below the header for rendering (suffix)
#' @param output_lines the text specifying the outupt of the YAML header; defaults to HTML
#' @param suffix the project ID and title to be included in the suffix
#'
#' @returns Invisibly returns NULL. Used for its side effect of editing files in place.
#' @export
#'
#' @examples
#'
#' # Example usage:
#'
#' replaceYAML(files = filesKivalinaStep2[c(5:15)]
#'          author = c("Jesse Coleman"),
#'          date = c("2025-08-18"),
#'          suffix = c("- (313) SWG Kivalina Comprehensive"))

replaceYAML <- function(
    files = NULL,
    folders = NULL,
    pattern = "\\.Rmd$",
    author = NULL,
    date = NULL,
    html_lines = NULL,
    output_lines = c(
      "  html_document:",
      "    theme: null",
      "    css: ../HTML/adfg-style.css",
      "    include:",
      "      before_body: ../HTML/header.html",
      "      after_body: ../HTML/footer.html"
    ),
    suffix = NULL
) {
  # Collect files from folders if needed
  if (!is.null(folders)) {
    files_from_folders <- unlist(
      lapply(folders, function(dir) list.files(dir, pattern = pattern, full.names = TRUE, recursive = TRUE))
    )
    files <- unique(c(files, files_from_folders))
  }
  files <- files[!is.na(files) & file.exists(files)] # Filter valid files

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

      yaml_starts <- which(trimws(lines) == "---")

      file_base <- tools::file_path_sans_ext(basename(file))
      # Construct html_lines if NULL
      if (is.null(html_lines)) {
        html_lines_use <- c(
          '<div class="title-container">',
          '  <img class="title-logo" src="../HTML/adfg_logo.png" alt="ADFG Logo"/>',
          sprintf('  <div class="title">%s %s</div>', file_base, suffix),
          "</div>"
        )
      } else {
        html_lines_use <- html_lines
      }

      # Check if html_lines already exist (all lines must appear in sequence)
      html_exists <- FALSE
      if (length(html_lines_use) > 0) {
        # Collapse file lines into a single string and check for the pattern
        file_text <- paste(lines, collapse = "\n")
        html_text <- paste(html_lines_use, collapse = "\n")
        html_exists <- grepl(html_text, file_text, fixed = TRUE)
      }

      # If html_lines already present, skip adding them
      if (html_exists) {
        html_lines_to_add <- character(0)
        html_message <- "HTML already present"
      } else {
        html_lines_to_add <- html_lines_use
        html_message <- "YAML and HTML replaced"
      }

      if (length(yaml_starts) >= 2) {
        new_header <- c(
          "---",
          sprintf('title: "%s %s"', file_base, suffix),
          sprintf('author: "%s"', author),
          sprintf('date: "%s"', date),
          "output:",
          output_lines,
          "---",
          html_lines_to_add
        )
        before <- if (yaml_starts[1] > 1) lines[1:(yaml_starts[1] - 1)] else character(0)
        after <- lines[(yaml_starts[2] + 1):length(lines)]
        new_lines <- c(before, new_header, after)
        writeLines(new_lines, file)
        message(sprintf("Updated: %s", file))
        results$status[i] <- "updated"
        results$message[i] <- html_message
      } else {
        warning(sprintf("YAML header not found: %s", file))
        results$status[i] <- "skipped"
        results$message[i] <- "YAML header not found"
      }
    }, error = function(e) {
      warning(sprintf("Error processing %s: %s", file, e$message))
      results$status[i] <- "error"
      results$message[i] <- e$message
    })
  }
  invisible(results)
}
