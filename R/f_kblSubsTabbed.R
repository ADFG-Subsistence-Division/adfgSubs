#' Outputs R Markdown tabset syntax with a table per group
#' @param df        Data frame
#' @param groupCol  Name or position of grouping variable
#' @param tableFun  Function to render table (default: kableExtra::kbl)
#' @param ...       Passed to tableFun
#' @examples
#' kblSubsTabbed(iris, "Species", kable::kable)
kblSubsTabbed <- function(df, groupCol, tableFun = kableExtra::kbl, ...) {
  if (is.numeric(groupCol)) groupCol <- names(df)[groupCol]
  groups <- unique(df[[groupCol]])

  cat(sprintf("## Tables by %s {.tabset}\n\n", groupCol))
  for (g in groups) {
    cat(sprintf("### %s\n\n", as.character(g)))
    print(tableFun(subset(df, df[[groupCol]] == g), ...))
    cat("\n\n")
  }
}
