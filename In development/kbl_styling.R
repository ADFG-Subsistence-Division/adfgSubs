#' kbl_nonsalmon_style: Custom kableExtra styling for nonsalmon fish tables
#'
#' @param df Data frame. Should match the structure of the nonsalmon harvest table.
#' @param resource_col Name or index of the resource/category column (default: 1).
#' @return A kableExtra table object styled to match ADF&G nonsalmon tables.
#' @importFrom knitr kbl
#' @importFrom kableExtra kable_styling column_spec row_spec add_header_above footnote
#' @examples
#' kbl_nonsalmon_style(your_dataframe)
kbl_nonsalmon_style <- function(df, resource_col = 1) {
  library(knitr)
  library(kableExtra)
  
  # Find indices for bold/italic rows (customize as needed for your data)
  bold_rows <- grep("^All nonsalmon fish$|^Other nonsalmon fish$", df[[resource_col]])
  italic_rows <- grep("^\\s{2,}|Whitefish, including|sheefish", df[[resource_col]])
  
  # Build kbl table
  kbl(df, align = c("l", rep("r", ncol(df)-1)), booktabs = TRUE, linesep = "") %>%
    kable_styling(
      font_size = 12, 
      full_width = FALSE, 
      position = "center", 
      latex_options = c("hold_position")
    ) %>%
    column_spec(resource_col, bold = FALSE, width = "7cm") %>%
    row_spec(0, bold = TRUE, extra_css = "border-bottom: 1pt solid;") %>%
    { if (length(bold_rows) > 0) row_spec(., bold_rows, bold = TRUE) else . } %>%
    { if (length(italic_rows) > 0) row_spec(., italic_rows, italic = TRUE) else . } %>%
    add_header_above(c(" " = 1, "Month of harvest" = ncol(df)-2, " " = 1)) %>%
    footnote(
      general = "Source  ADF&G Division of Subsistence household surveys, 2024.",
      symbol = "Table n-m.–Estimated number of nonsalmon harvested by month, Allakaket and Alatna, 2023.",
      threeparttable = TRUE
    )
}


#' kbl_grouped_style: Generic kableExtra styling for grouped tables
#'
#' @param df Data frame.
#' @param group_rows Character vector of row labels to treat as section headers (bolded, optional border).
#' @param group_col_start Index of the first column to group under a spanning header.
#' @param group_col_end Index of the last column to group under a spanning header.
#' @param group_col_label String to use as the spanning column header.
#' @param resource_col Name or index of the resource/category column (default: 1).
#' @param font_size Font size (default: 12).
#' @param footnote_general Optional general footnote.
#' @param footnote_symbol Optional symbol footnote.
#' @return A kableExtra table object.
#' @importFrom knitr kbl
#' @importFrom kableExtra kable_styling column_spec row_spec add_header_above footnote
#' @examples
#' kbl_grouped_style(df, group_rows=c("Total","Subtotal"), group_col_start=2, group_col_end=13, group_col_label="Month", resource_col=1)
kbl_grouped_style <- function(
    df,
    group_rows,
    group_col_start,
    group_col_end,
    group_col_label,
    resource_col = 1,
    font_size = 12,
    footnote_general = NULL,
    footnote_symbol = NULL
) {
  library(knitr)
  library(kableExtra)
  
  # Indices of group header rows
  group_rows_idx <- which(df[[resource_col]] %in% group_rows)
  
  # Build vector for add_header_above
  n_cols <- ncol(df)
  header_vec <- rep(1, n_cols)
  header_vec[group_col_start:group_col_end] <- NA # to be replaced by group_col_label
  header_vec <- replace(header_vec, group_col_start, group_col_end - group_col_start + 1)
  names(header_vec) <- rep(" ", n_cols)
  names(header_vec)[group_col_start] <- group_col_label
  
  # kable construction
  tab <- kbl(df, align = c("l", rep("r", n_cols-1)), booktabs = TRUE, linesep = "") %>%
    kable_styling(
      font_size = font_size, 
      full_width = FALSE, 
      position = "center", 
      latex_options = c("hold_position")
    ) %>%
    column_spec(resource_col, bold = FALSE, width = "7cm") %>%
    row_spec(0, bold = TRUE, extra_css = "border-bottom: 1pt solid;")
  
  # Style grouping rows
  for (idx in group_rows_idx) {
    tab <- row_spec(tab, idx, bold = TRUE, extra_css = "border-top: 1pt solid;")
  }
  
  # Add grouped column header
  tab <- add_header_above(tab, header_vec)
  
  # Optional footnotes
  if (!is.null(footnote_general) || !is.null(footnote_symbol)) {
    tab <- footnote(
      tab,
      general = footnote_general,
      symbol = footnote_symbol,
      threeparttable = TRUE
    )
  }
  
  tab
}