#' Apply ADF&G table style to a gt table object
#'
#' This function applies a consistent, publication-ready styling theme to a GT
#' table object. The theme is designed for subsistence harvest and demographic
#' analysis tables, featuring a clean layout with minimal borders, right-aligned
#' numeric columns, and Times New Roman font.
#'
#' @param data A GT table object to be styled.
#'
#' @return A styled GT table object with applied formatting, alignment, and
#'   border specifications.
#'
#' @details
#' This function applies the following styling elements:
#' \itemize{
#'   \item Right-aligns all numeric columns
#'   \item Centers numeric column headers
#'   \item Colors zero values in numeric columns light gray
#'   \item Replaces missing values with blank strings
#'   \item Removes all internal horizontal lines for a clean appearance
#'   \item Adds 1.5px black borders above and below the table body
#'   \item Adds 1.5px black borders above and below column labels
#'   \item Uses letter-based footnote marks (a, b, c, etc.)
#'   \item Sets font to Times New Roman at 13.5px
#' }
#'
#' The function is designed to work with GT tables containing subsistence harvest
#' data and is typically applied as the final styling step before rendering or
#' saving the table.
#'
#' @examples
#' library(gt)
#' library(tidyverse)
#'
#' # Create a sample table
#' harvestData <- data.frame(
#'   species = c("Salmon", "Halibut", "Herring"),
#'   hhCount = c(42, 18, 5),
#'   meanHarvest = c(145.3, 78.5, 22.1),
#'   sd = c(89.2, 56.3, 15.8)
#' )
#'
#' harvestData %>%
#'   gt() %>%
#'   themeSubsTable()
#'
#' @importFrom gt cols_align tab_style tab_style_body sub_missing tab_options
#'   opt_table_font opt_footnote_spec
#' @importFrom dplyr where
#'
#' @export

themeSubsTable <- function(data) {

  data %>%

    cols_align(
      align = "right",
      columns = where(is.numeric)
    ) %>%

    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = where(is.numeric))
    ) %>%

    tab_style_body(style = cell_text(color = "lightgray"),
                   columns = where(is.numeric),
                   values = 0
    ) %>%

    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = ""
    ) %>%

    tab_options(
      footnotes.marks = "letters",
      footnotes.font.size = px(13.5),
      footnotes.padding = px(1),

      data_row.padding = px(0),
      # data_row.font.size = px(10),


      # Line between the last data row and footnotes/source notes
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(0),
      table.border.top.color = "transparent",           # Color of top border
      table.border.top.width = px(1.5),             # Thickness of top border

      column_labels.border.top.color = "black",   # Color above column labels/spanners
      column_labels.border.top.width = px(1.5),
      column_labels.border.bottom.color = "black",# Color below column labels/spanners
      column_labels.border.bottom.width = px(1.5),
      column_labels.padding = px(0),

      heading.border.bottom.color = "black",      # Line below the table heading/title, if present
      heading.border.bottom.width = px(1.5),
      heading.padding = px(0),
      heading.align = "left",
      heading.title.font.size = px(13.5),

      # Remove body row lines
      row.striping.include_table_body = FALSE,    # (Unrelated, but often toggled for "clean" look)
      row_group.padding = px(0),

      table_body.hlines.color = "transparent",    # Remove all body horizontal lines
      table_body.hlines.width = px(0),
      table_body.border.bottom.width = px(1.5),
      table_body.border.bottom.color = "black",
      table_body.border.top.width = px(1.5),
      table_body.border.top.color = "black",

      # Row group borders
      row_group.border.top.width = px(0),
      row_group.border.top.color = "transparent",
      row_group.border.bottom.width = px(0),
      row_group.border.bottom.color = "transparent",

      # Stub borders (removes vertical line between stub and value column)
      table_body.vlines.width = px(0),
      table_body.vlines.color = "transparent",

      # Also try these if needed:
      stub.border.width = px(0),
      stub.border.color = "transparent"
    ) %>%

    opt_table_font(
      font = "Times New Roman",
      size = px(13.5)
    ) %>%

    opt_footnote_spec(spec_ref = "^", spec_ftr = "a.")
}
