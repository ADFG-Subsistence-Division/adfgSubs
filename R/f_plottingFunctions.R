#' Box and whisker plot a la adfgSubs
#'
#' Function for producing ggplot outputs according to current ADF&G Division of Subsistence standards.
#'
#' @param figData description
#' @param categoryColName description
#' @param dataColName description
#' @param yAxisTitle default = "Reported number"
#' @param yBreaks default = 20
#' @returns description
#' @details
#' Additional details...
#' These functions rely on ggplot2 and standardized inputs. This file
#       is not intended to be run as a stand-alone file.
#' @export

ggBoxWhiskerFormatted <- function(figData, categoryColName, dataColName, yAxisTitle="Reported number", yBreaks=20) {

  # Note: No title per div. standards; this will be printed in the RMD as
  #       a caption.

  figData <- figData %>% rename("category" = all_of(categoryColName),
                                "plotVals" = all_of(dataColName))

  # pre-emptively remove NA and invalid data.
  figData <- figData %>% filter(!is.na(figData$plotVals))

  plotOut <- ggplot(figData,        # Create ggplot2 boxplot
                    aes(x = category,
                        y = plotVals)) +
    geom_boxplot(fill="lightgray",) +
    scale_y_continuous(limits = c(0, max(figData$plotVals)), breaks = seq(0, max(figData$plotVals), by = yBreaks)) +
    ylab(yAxisTitle) +
    xlab("") +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.key = element_rect(fill = "white"),
          legend.text = element_text(size=10),
          legend.margin=margin(t = 0, unit='cm'),
          legend.box.spacing = unit(0, "mm")) +
    theme(text=element_text(family="serif", size = 10),
          axis.text = element_text(size=10),
          axis.title.x = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          panel.grid.major.y = element_line(color="lightgray")) +
    stat_summary(fun = mean,
                 geom = "point",
                 aes(group = 1,
                     shape="Mean"),
                 col = "#3288bd",
                 size=3) +
    stat_summary(fun = mean,
                 geom = "text",
                 aes(label=round(after_stat(y),1),
                     vjust = -0.5, hjust = -.25),
                 col = "#3288bd", family="serif") +
    scale_shape_manual(name = "Mean", values = c(4))


  return(plotOut)
}


#' Apply adfgSubs plot theme
#'
#' A ggplot2 theme to mimick tables and figures produced in Excel.
#'
#' @param base_size Font size; defaults to 12 point.
#' @param base_family Font family; defaults to "serif".
#'
#' @returns Nothing
#' @export
#'

themeSubs <- function(base_size = 12, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Axis lines and ticks
      axis.line = element_line(size = 0.5, color = "black"),
      axis.ticks = element_line(size = 0.5, color = "black"),
      axis.text = element_text(color = "black", family = base_family, size = base_size),
      axis.title = element_text(color = "black", family = base_family, size = base_size + 2),
      axis.text.x = element_text(hjust = 1, vjust = 1), # for bar plots

      # Grid lines
      panel.grid.major.y = element_line(color = "#E5E5E5", size = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      # Backgrounds
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),

      # Legend
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      legend.title = element_text(family = base_family, face = "plain"),
      legend.text = element_text(family = base_family),
      legend.position = "bottom",

      # Strip for facets
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(family = base_family, size = base_size + 2),

      # Plot title
      plot.title = element_text(family = base_family, hjust = 0.5, size = base_size + 4),
      plot.subtitle = element_text(family = base_family, hjust = 0.5)
    )
}

#' Get adfgSubs standard plotting colors
#'
#' Helper function for color palettes. This function provides a way for
#'  analysts to do a better job of customizing colors per-project,
#'  while starting with the default scheme.
#'
#' @param palName Palette name; default is "Spectral"
#' @param nColors Number of colors needed
#' @param override Logical; should colorblind-safe values be used? Default is FALSE
#'
#' @returns A vector of color values.
#' @export
#'

getColors <- function(palName = "Spectral", nColors = 1, override = FALSE)
{
  pal=c('#000000')
  if(palName == "Rb")
  {
    pal = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7', '#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a')
  }
  if(palName == "RylG")
  {
    pal = c('#a50026', '#006837','#d73027','#1a9850','#f46d43',
            '#66bd63','#fdae61','#a6d96a','#fee08b','#d9ef8b',
            '#000000')
  }
  if(palName == "Spectral")
  {
    pal = c('#3288bd','#f46d43','#66c2a5','#d53e4f','#abdda4',
            '#fdae61', '#e6f598', '#FFD36E', '#5e4fa2', '#9e0142', '#000000')
    if(override)
    {
      pal = c(pal, '#e0e0e0','#bababa','#878787','#4d4d4d', '#fddbc7', '#f4a582')
    }
  }

  if(nColors > 11 & override == FALSE)
    return(c('#000000'))
  else
    return(pal[1:nColors])
}

#' adfgSubs themed pie charts
#'
#' A function to create custom pie charts with base R. This is an alternative to
#' ggplot's coord_polar() transformation, which doesn't always lend itself to
#' correct label placement.
#'
#' @param df A data frame containing variables to be plotted.
#' @param value_col A column of values to be represented by pie slices.
#' @param percent_col A column of percentage values to be represented
#' @param commname_col A column of community name values.
#' @param category_col A column of resource category values to be mapped to colors.
#' @param label_col A column of labels combining resource category/percentage.
#' @param colors A vector of colors to be used for plotting.
#' @param main The main title of the plot.
#' @param font_family Font family; default is "serif".
#' @param font_size Font size; default is 12 point.
#' @param leader_line_col Color of the leader lines between labels and pie slices.
#' @param leader_line_len Length of the leader lines between labels and pie slices.
#'
#' @returns A pie chart.
#' @export
#'

pieSubs <- function(
    df,
    value_col = "value",
    percent_col = "percent",
    commname_col = "commname",
    category_col = "category",
    label_col = "label",
    colors = NULL,
    main = NULL,
    font_family = "Times New Roman",
    font_size = 1,
    leader_line_col = "gray50",
    leader_line_len = 1.2
) {
  # Extract values and labels from data frame
  values <- df[[value_col]]
  percents <- df[[percent_col]]
  commnames <- df[[commname_col]]
  categories <- df[[category_col]]
  labels <- df[[label_col]]

  # Set default colors if not supplied
  if (is.null(colors)) {
    colors <- rainbow(length(values))
  }

  # Register font for Windows users
  if (.Platform$OS.type == "windows") {
    windowsFonts(TimesNewRoman = windowsFont(font_family))
    par(family = "TimesNewRoman", cex = font_size)
  } else {
    par(family = font_family, cex = font_size)
  }

  # Draw pie without labels
  pie(values, labels = NA, col = colors, main = main, radius = 1, border = NA)

  # Calculate label positions for leader lines
  angle <- cumsum(c(0, values / sum(values))) * 2 * pi
  label_pos <- (angle[-length(angle)] + angle[-1]) / 2
  x0 <- cos(label_pos) * 0.8
  y0 <- sin(label_pos) * 0.8
  x1 <- cos(label_pos) * leader_line_len
  y1 <- sin(label_pos) * leader_line_len

  # Draw leader lines
  segments(x0, y0, x1, y1, col = leader_line_col, lwd = 1.5)

  # Place labels at the end of leader lines
  text(x1, y1, labels, cex = font_size)

  # Reset graphics parameters
  par(family = "", cex = 1)
}
