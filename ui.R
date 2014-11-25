library(shiny)
library(ggplot2)
library(extrafont)


source('lib/theme_default.R')
source("lib/customSlider/R/customSliderInput.R")



jscolorInput <- function(id, default) {
  tagList(
    singleton(tags$head(tags$script(src = "ShinySky/inst/www/ss-jscolor.js"))),
    singleton(tags$head(tags$script(src = "ShinySky/inst/www/jscolor/jscolor.js"))),
    singleton(tags$head(tags$link(rel = "stylsheet", type = "text/css", href = "css/main.css"))),
    tags$input(id = id, value = default, class = "color shiny-bound-input", type = "text", onchange = sprintf("$('#%s').trigger('afterChange')",id)),
    tags$script(sprintf("$('#%s').trigger('afterChange')",id))
  )
}

gg_colour_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

promote_list_item <- function(list,item,includeFirstOption){
  #delete item
  list[which(list==item)] <- NULL
  #append at start of list
  if(includeFirstOption){
    list <- c(item,list)
  }
  else{
    list <- c("",item,list)
  }
  # print(list)
  list
}

lineOpts <- function(title,element,width,isParent=FALSE,axis_ticks=FALSE){
  if(axis_ticks){
    length <- numericInput("axis_ticks_length",'Tick Length (pt): ',value= theme_current$axis.ticks.length, step = 0.1)
    margin <- numericInput("axis_ticks_margin",'Tick Margin (pt): ',value= theme_current$axis.ticks.margin, step = 0.1)
  }
  else{
    length <- NULL
    margin <- NULL
  }
  name <- gsub("_",".",element)
  col <- column(width,
          h4(title),
          selectizeInput(paste(element,"linetype",sep="_"),
            label = "Type: ", 
            choices = promote_list_item(lineTypes, theme_current[name][[1]]["linetype"][[1]],isParent),
            options = list(
              # allowEmptyOption = I(sprintf('function(){true;}')),
              dropdownParent = 'body',
              render = I(sprintf(
              "{
                  option: function(item, escape) {
                    return '<div><img ' +
                        'src=\"images/lineTypes/' + item.value + '_line.png\" /></div>';
                  }
              }"
                        ))
                      )
          ),
          selectizeInput(paste(element,"lineend",sep="_"),
          label = "End: ",
          choices = promote_list_item(lineEnds, theme_current[name][[1]]["linened"][[1]],isParent),
          options = list(
            dropdownParent = 'body',
            render = I(sprintf(
            "{
                option: function(item, escape) {
                  return '<div><img ' +
                      'src=\"images/lineEnds/' + item.value + '_end.png\" />' + ' ' + item.value + '</div>';
                }
            }"
                      ))
                    )
          ),
          numericInput(paste(element,"size",sep="_"),'Stroke (pt): ',value= theme_current[name][[1]]["size"][[1]], step = 0.1),
          tags$label("Color: "),
          jscolorInput(paste(element,"colour",sep="_"), theme_current[name][[1]]["colour"][[1]]),
          length,
          margin,
          checkboxInput(paste(element,"hide",sep="_"),"Remove Element",FALSE)
          )
  col
}

rectOpts <- function(title,element,width,isParent=FALSE,panel_border=FALSE, strip_background=FALSE, legend_background=FALSE, legend_key=FALSE){
  if(panel_border){
    panel_fill <- div(id="panel_border_fill")
  }
  else{
    panel_fill <- jscolorInput(paste(element,"fill",sep="_"), theme_current[element][[1]]["fill"][[1]])
  }

  if(strip_background){
    panel_margin <- numericInput("panel_margin",'Facet Panel Margin (pt): ',value= theme_current$panel.margin, step = 0.1)
  }
  else{
    panel_margin <- NULL
  }

  if(legend_background){
    legend_margin <- numericInput("legend_margin",'Legend Margin (pt): ',value= theme_current$legend.margin, step = 0.1)
  }
  else{
    legend_margin <- NULL
  }

  if(legend_key){
    key_size <- numericInput("legend_key_dimensions",'Key Size (pt): ',value= theme_current$legend.key.size, step = 0.1)
    key_height <- numericInput("legend_key_height",'Key Height (pt): ',value= theme_current$legend.key.height, step = 0.1)
    key_width <- numericInput("legend_key_width",'Key Width (pt): ',value= theme_current$legend.key.width, step = 0.1)
  }
  else{
    key_size <- NULL
    key_height <- NULL
    key_width <- NULL
  }
  col <- column(width,
          h4(title),
          selectizeInput(paste(element,"linetype",sep="_"),
            label = "Type: ", 
            choices = promote_list_item(lineTypes, theme_current[element][[1]]["linetype"][[1]],isParent),
            options = list(
              dropdownParent = 'body',
              render = I(sprintf(
              "{
                  option: function(item, escape) {
                    return '<div><img ' +
                        'src=\"images/lineTypes/' + item.value + '_line.png\" /></div>';
                  }
              }"
            )))),
          numericInput(paste(element,"size",sep="_"),'Stroke (pt): ',value= theme_current[element][[1]]["size"][[1]], step = 0.1),
          tags$label("Fill: "),
          panel_fill,
          tags$label("Stroke: "),
          jscolorInput(paste(element,"colour",sep="_"), theme_current[element][[1]]["colour"][[1]]),
          panel_margin,
          legend_margin,
          key_size,
          key_height,
          key_width,
          checkboxInput(paste(element,"hide",sep="_"),"Remove Element",FALSE)
        )
  col
}

fontOpts <- function(title,element,width,isParent=FALSE){
   if(isParent){
    hidden <- 1
    value <- 0
   }
   else{
    hidden <- 0
    value <- NULL
   }
   col <- column(width,
          h4(title),
           selectizeInput(paste(element,"family",sep="_"),
            label="font",
            choices= promote_list_item(c("default","",fonts()), theme_current[element][[1]]["font"][[1]],isParent),
            options = list(
              dropdownParent = 'body'
            )),
           selectizeInput(paste(element,"face",sep="_"),
            label="face",
            choices=promote_list_item(faces, theme_current[element][[1]]["face"][[1]],isParent),
            options = list(
              dropdownParent = 'body'
            )),
          numericInput(paste(element,"size",sep="_"),'Size: ',value= theme_current[element][[1]]["size"][[1]], step = 1),
          tags$label("Color: "),
          jscolorInput(paste(element,"colour",sep="_"), theme_current[element][[1]]["colour"][[1]]),
          customSliderInput(paste(element,"hjust",sep="_"),'Horizontal Align: ', min = 0, max = 1, value= value, step = 0.01),
          numericInput(paste(element,"hjust_hidden",sep="_"),'',value=hidden),
          customSliderInput(paste(element,"vjust",sep="_"),'Vertical Align: ', min = 0, max = 1, value= value, step = 0.01),
          numericInput(paste(element,"vjust_hidden",sep="_"),'',value=hidden),
          customSliderInput(paste(element,"angle",sep="_"),'Angle: ', min = 0, max = 360, value= value, step = 0.1),
          numericInput(paste(element,"angle_hidden",sep="_"),'',value=hidden),
          numericInput(paste(element,"lineheight",sep="_"),'Line Height: ',value= theme_current[element][[1]]["lineheight"][[1]], step = 0.1),
          checkboxInput(paste(element,"hide",sep="_"),"Remove Element",FALSE)
        )
  col
}

theme_current <- theme_default()
nineColours <- gg_colour_hue(9)

lineTypes <- list(0,1,2,3,4,5,6)
# ,"longdash","dotted","dashed","dotdash","twodash","F1","4C88C488","1F","12345678")
lineEnds <- list("butt","round","square")
themes <- list("default","grey","bw","calc","economist","excel","few","fivethirtyeight","gdocs","solarized","stata","tufte","wsj")
faces <- list("plain", "italic", "bold", "bold.italic")
 
shinyUI(fluidPage(theme = "css/main.css",
 
  headerPanel("ggplot2 Theme Builder"),
  fluidRow(
    plotOutput(outputId = 'plot', width = "100%", height = "100%"),
    h4("Download the Theming Scripts"),
    downloadButton('downloadData', 'R Script for theme (run every R session)'),
    downloadButton('installationScript', 'Installation script (run once)')
  ),
  fluidRow(
    a(href="https://github.com/bchartoff/ggShinyApp","View this project's code on github")
  ),
  hr(),
  navlistPanel(widths = c(3,9),
    tabPanel("Theme Builder Settings",
        column(12,
          helpText("See the output R script, \"ggplot_styling.R\", for example code to build these charts"),
          selectizeInput("sampleChart",
            options = list(
              dropdownParent = 'body'
            ),
            label = "Sample chart Type", 
            choices = list( "Bar chart: 1 colour" = 1,
                          "Bar chart: 3 colours" = 2,
                          "Bar chart: 5 colours" = 3,
                          "Scatter plot: 3 colours" = 4,
                          "Scatter plot: 9 colours" = 5,
                          "Line chart: 3 colours" = 6,
                          "Scatter plot: Facet Grid" = 7,
                          "Histogram: 22 colours" = 8), selected = 3
          ),
          numericInput('plotWidth', 'Plot Dimensions (w x h)', value = 600, step = 1),
          div(class='dimensionx_x','x'),
          numericInput('plotHeight', '', value = 400, step = 1)
      )
    ),
    tabPanel("Global colour palette",
      fluidRow(
        helpText("NOTE: This script is intentionally built to limit themes to 9 colors of fewer. Graphs with greater than 9 colors can become difficult to parse, or differentiate groupings. However, if you want to build a graph with more colors (for the purposes of sketching a large data set, for example), you can use the extendedPalette function:"),
        helpText("p <- ggplot(data)"),
        helpText("p + scale_fill_manual(values = extendedPalette(NUMBER_OF_CATEGORIES))")
      ),
      fluidRow(
        column(4,
          jscolorInput("palColour1", nineColours[1]),
          jscolorInput("palColour2", nineColours[4]),
          jscolorInput("palColour3", nineColours[7])
        ),
        column(4,
          jscolorInput("palColour4", nineColours[2]),
          jscolorInput("palColour5", nineColours[5]),
          jscolorInput("palColour6", nineColours[8])
        ),
        column(4,
          jscolorInput("palColour7", nineColours[3]),
          jscolorInput("palColour8", nineColours[6]),
          jscolorInput("palColour9", nineColours[9])
        )
      )
    ),
    tabPanel("Parent Elements",
      lineOpts("All Lines","line",3,isParent=TRUE),
      rectOpts("All Rectangles","rect",3,isParent=TRUE),
      fontOpts("All Text","text",3,isParent=TRUE),
      fontOpts("All Titles","title",3)

    ),
    tabPanel("Axis Lines",
      lineOpts("Axes","axis_line",3),
      lineOpts("X Axis","axis_line_x",3),
      lineOpts("Y Axis","axis_line_y",3)
    ),
    tabPanel("Axis Labels",
      fontOpts("Both Axis Labels","axis_title",3),
      fontOpts("X Axis Label","axis_title_x",3),
      fontOpts("Y Axis Label","axis_title_y",3)
    ),
    tabPanel("Axis Ticks",
      lineOpts("Tick marks","axis_ticks",3,axis_ticks=TRUE),
      lineOpts("X Axis Tick Marks","axis_ticks_x",3),
      lineOpts("Y Axis Tick Marks","axis_ticks_y",3)
    ),
    tabPanel("Axis Tick Labels",
      fontOpts("All Ticks Labels","axis_text",3),
      fontOpts("X Axis Tick Labels","axis_text_x",3),
      fontOpts("Y Axis Tick Labels","axis_text_y",3)
    ),
    tabPanel("Legend",
      fluidRow(
      rectOpts("Legend Background","legend_background",3,legend_background=TRUE),
      rectOpts("Legend Keys","legend_key",3,legend_key=TRUE),
      column(6,
        h4("Legend Layout"),
        selectizeInput("legend_direction",
            options = list(
              dropdownParent = 'body'
            ),
            label = "Legend Layout:", 
            choices = list( "",
                            "vertical",
                            "horizontal"
                          )
          ),
        selectizeInput("legend_box",
            label = "Multiple Legend Layout:", 
            choices = list( "",
                            "horizontal",
                            "vertical"
                          ),
            options = list(
              dropdownParent = 'body'
            )
          ),
        selectizeInput("legend_position",
            options = list(
              dropdownParent = 'body'
            ),
            label = "Legend Position:", 
            choices = list( "right",
                            "left",
                            "bottom",
                            "top"
                          )
        ),
        # div(id="legend_position_description")
        helpText("Or, you can enter a vector specifying Legend Position (0 to 1). Manual vector input takes precedence over the dropdown above. Delete both manual inputs to default back to the dropdown."),
        numericInput("legend_position_x",NULL,value= NULL, step = 0.01),
        div(class= 'comma',','),
        numericInput("legend_position_y",NULL,value= NULL, step = 0.01),
        helpText("For manual positions, optionally specify origin point as vector (0 to 1)"),
        numericInput("legend_justification_x",NULL,value= NULL, step = 0.01),
        div(class = 'comma', ','),
        numericInput("legend_justification_y",NULL,value= NULL, step = 0.01)
      )
)
    ),
    tabPanel("Legend Text",
      fontOpts("Legend Title","legend_title",3),
      fontOpts("Legend Text","legend_text",3),
      column(3,
        h4("Text Alignment"),
        customSliderInput("legend_title_align",'Title Align: ', min = 0, max = 1, value= NULL, step = 0.01),
        numericInput('legend_title_align_hidden','',value=0),
        customSliderInput("legend_text_align",'Text Align: ', min = 0, max = 1, value= NULL, step = 0.01),
        numericInput('legend_text_align_hidden','',value=0)
        )
      ),
    tabPanel("Plotting Area",
      rectOpts("Plot Background","panel_background",3),
      rectOpts("Plot Border","panel_border",3,panel_border=TRUE),
      lineOpts("All Grid Lines","panel_grid",3)
    ),
    tabPanel("Major Grid Lines",
      lineOpts("Major Grid Lines","panel_grid_major",3),
      lineOpts("X Major Grid Lines","panel_grid_major_x",3),
      lineOpts("Y Major Grid Lines","panel_grid_major_y",3)
    ),
    tabPanel("Minor Grid Lines",
      lineOpts("Minor Grid Lines","panel_grid_minor",3),
      lineOpts("X Minor Grid Lines","panel_grid_minor_x",3),
      lineOpts("Y Minor Grid Lines","panel_grid_minor_y",3)
    ),
    tabPanel("Background",
      rectOpts("Background Area","plot_background",3),
      fontOpts("Plot Title","plot_title",3),
      column(3,
        h4("Plot Margin"),
        numericInput("plot_margin_top","Top (pt): ",value = as.numeric(theme_default()$plot.margin[1])),
        numericInput("plot_margin_right","Right (pt): ",value = as.numeric(theme_default()$plot.margin[2])),
        numericInput("plot_margin_bottom","Bottom (pt): ",value = as.numeric(theme_default()$plot.margin[3])),
        numericInput("plot_margin_left","Left (pt): ",value = as.numeric(theme_default()$plot.margin[4]))
      )
    ),
    tabPanel("Facet Labels",
      rectOpts("Label Background","strip_background",3,strip_background=TRUE),
      fontOpts("Label Text", "strip_text",3),
      fontOpts("X Label Text", "strip_text_x",3),
      fontOpts("Y Label Text", "strip_text_y",3)
    )
  ),
  hr(),
  tags$head(tags$script(src="js/remove_element.js")),
  tags$script(src="js/null_slider.js")
  # tags$head(tags$script(src = "js/jquery.slider.js")),
  # tags$head(tags$link(rel = "stylsheet", type = "text/css", href = "css/jquery.slider.min.css"))
  # )


))
