#' ggBoxWhiskerFormatted
#' 
#' Function for producing ggplot outputs according to current ADF&G Division of Subsistence Subsistence standards.
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