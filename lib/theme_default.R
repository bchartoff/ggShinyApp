library('ggplot2')
source('lib/grid/unit.R')

    theme_default <- function(){
      theme(
      line = element_line(size=.5, color = "#000000", linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "#ffffff", color = "#000000", size = .5, linetype = 1),
      text = element_text(family = "", face = "plain", color = "#000000", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
      title = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      axis.text = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      strip.text = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      axis.line = element_line(size=NULL, color = NULL, linetype = NULL, lineend = NULL),
      axis.text.x = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      axis.text.y = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      axis.ticks = element_line(size=NULL, color = NULL, linetype = NULL, lineend = NULL),
      axis.title.x = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      axis.title.y = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      axis.ticks.length = unit(4, "points"),
      axis.ticks.margin = unit(0.3, "points"),
      legend.background = element_rect(fill = NULL, color = NULL, size = NULL, linetype = NULL),
      legend.margin = unit(8, "points"),
      legend.key = element_rect(fill = NULL, color = NULL, size = NULL, linetype = NULL),
      legend.key.size = unit(10, "points"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      legend.text.align = NULL,
      legend.title = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      legend.title.align = NULL,
      legend.position = "right",
      legend.direction = NULL,
      legend.justification = NULL,
      legend.box = NULL,
      panel.background = element_rect(fill = NULL, color = NULL, size = NULL, linetype = NULL),
      panel.border = element_rect(fill = NA, color = NULL, size = NULL, linetype = NULL),
      panel.grid.major = element_line(size=NULL, color = NULL, linetype = NULL, lineend = NULL),
      panel.grid.minor = element_line(size=NULL, color = NULL, linetype = NULL, lineend = NULL),
      panel.margin = unit(2, "points"),
      panel.margin.x = NULL,
      panel.margin.y = NULL,
      strip.background = element_rect(fill = NULL, color = NULL, size = NULL, linetype = NULL),
      strip.text.x = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      strip.text.y = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      plot.background = element_rect(fill = NULL, color = NULL, size = NULL, linetype = NULL),
      plot.title = element_text(family = NULL, face = NULL, color = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      plot.margin = unit(c(10, 10, 10, 10), "points"),
      #top, right, bottom, left
      complete = TRUE
    )
}




