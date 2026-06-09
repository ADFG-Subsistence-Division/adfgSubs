#' Apply adfgSubs plot theme
#'
#' A ggplot2 theme to mimic tables and figures produced in Excel.
#'
#' @param base_size Font size; defaults to 10 point.
#' @param base_family Font family; defaults to "Times New Roman".
#'
#' @returns No return value. Called for its side effect (applies ggplot formatting)
#' @export
#'
themeSubs <- function (base_size = 10, base_family = "Times New Roman")
{
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          # axis.ticks.length = unit(0, "pt"),
          axis.text = element_text(color = "black", family = base_family, size = base_size),
          axis.title = element_text(color = "black",family = base_family, size = base_size),
          axis.text.x = element_text(hjust = 0.5, vjust = 0.4),
          axis.title.y = element_text(margin = margin(r = 10), angle = 90),

          panel.grid.major.y = element_line(color = "#d9d9d9", linewidth = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_blank(),

          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA),
          legend.title = element_blank(),
          legend.text = element_text(family = base_family),
          legend.position = "bottom", strip.background = element_rect(fill = "white", color = NA),

          strip.text = element_text(family = base_family, size = base_size),

          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(family = base_family, hjust = 0.5, size = base_size),
          plot.subtitle = element_text(family = base_family, hjust = 0.5),
          plot.caption = element_text(hjust = 0.01, family = base_family, size = base_size))
}
