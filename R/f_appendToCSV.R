#' Append Data Frame to a CSV File
#'
#' Appends rows from a data frame to a CSV file in a safe and simple way.
#' If the file does not exist, a new file is created with column names.
#'
#' @param file Character string. Path to the CSV file to append to.
#' @param new_data Data frame. The rows to append to the CSV file.
#'
#' @details
#' - If \code{file} does not exist, it will be created and column headers will be written.
#' - If \code{file} exists, \code{new_data} rows are appended without headers.
#' - Strings are quoted as needed. No row names are written.
#' - Use this for simple, flat CSV files—does not handle complex objects or nested data frames.
#'
#' @return No return value. Called for its side effect (writes to file).
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(a = 1:2, b = letters[1:2])
#'   appendToCSV("out.csv", df)
#' }
#'
#' @export

appendToCSV <- function(file,
                        newData,
                        by = c("figName", "communty", "studyear"),
                        colTypes = "iiiccccccc") {
  if (file.exists(file)) {
    existing_data <- read_csv(file,
                              na = "",
                              col_types = colTypes)

    # Remove rows with same keys as any in new_data
    existing_filtered <- dplyr::anti_join(existing_data, newData, by = by)

    updated_data <- dplyr::bind_rows(existing_filtered, newData)

  } else {

    updated_data <- newData
  }

  write_excel_csv(
    updated_data,
    file = file,
    quote = "needed",
    na = "")
}
