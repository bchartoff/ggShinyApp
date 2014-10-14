library(shiny)
library(ggplot2)
library(extrafont)

source('lib/theme_default.R')


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

lineOpts <- function(title,element,width,isParent){
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
            )))),
          numericInput(paste(element,"size",sep="_"),'Stroke: ',value= theme_current[name][[1]]["size"][[1]]),
          tags$label("Color: "),
          jscolorInput(paste(element,"colour",sep="_"), theme_current[name][[1]]["colour"][[1]]),
          selectizeInput(paste(element,"lineend",sep="_"),
            label = "End: ",
            choices = promote_list_item(lineEnds, theme_current[name][[1]]["linened"][[1]],isParent),
            options = list(
              dropdownParent = 'body',
              render = I(sprintf(
              "{
                  option: function(item, escape) {
                    return '<div><img ' +
                        'src=\"images/lineEnds/' + item.value + '.png\" /></div>';
                  }
              }"
          ))))
        )
  col
}

rectOpts <- function(title,element,width,isParent){
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
                        'src=\"images/lineTypes/' + item.value + '.png\" /></div>';
                  }
              }"
            )))),
          numericInput(paste(element,"size",sep="_"),'Stroke: ',value= theme_current[element][[1]]["size"][[1]]),
          tags$label("Fill: "),
          jscolorInput(paste(element,"fill",sep="_"), theme_current[element][[1]]["fill"][[1]]),
          tags$label("Stroke: "),
          jscolorInput(paste(element,"colour",sep="_"), theme_current[element][[1]]["colour"][[1]])
        )
  col
}

fontOpts <- function(title,element,width){
   col <- column(width,
          h4(title),
           selectizeInput("font",
            label="font",
            choices=fonts()
            ),
          numericInput("rectSize",'Stroke: ',value= theme_current$element$size),
          tags$label("Fill: "),
          jscolorInput("rectFill", theme_current$element$fill),
          tags$label("Stroke: "),
          jscolorInput("rectColour", theme_current$element$colour)
        )
  col
}

theme_current <- theme_default()
nineColours <- gg_colour_hue(9)

lineTypes <- list(0,1,2,3,4,5,6)
# ,"longdash","dotted","dashed","dotdash","twodash","F1","4C88C488","1F","12345678")
lineEnds <- list("butt","round","square")
themes <- list("default","grey","bw","calc","economist","excel","few","fivethirtyeight","gdocs","solarized","stata","tufte","wsj")
 
shinyUI(fluidPage(theme = "css/main.css",
 
  headerPanel("ggplot2 Theme Builder"),

  navlistPanel(
    tabPanel("Theme Builder Settings",
      fluidRow(
        column(12,
          selectizeInput("currentTheme",
            label = "Theme: ", 
            choices = themes,
            options = list(
              dropdownParent = 'body'
            )),
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
                          "Line chart: 3 colours" = 6), selected = 2)

        )
      )
    ),
    tabPanel("Global colour palette",
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
      fluidRow(
        lineOpts("All Lines","line",3,TRUE),
        rectOpts("All Rectangles","rect",3,TRUE),
        
        column(3,
          h4("All Text"),
          selectizeInput("tmp",
            options = list(
              dropdownParent = 'body'
            ),
            label="tmp",
            choices=fonts()
            ),
          numericInput("lineSizae",'stroke',value=NULL),
          numericInput("lineSizae",'stroke',value=NULL)
        ),
        column(3,
          h4("All Titles"),
          numericInput("lineSizae",'stroke',value=NULL),
          numericInput("lineSizae",'stroke',value=NULL),
          numericInput("lineSizae",'stroke',value=NULL)
        )
      )
    ),
    tabPanel("Axes",
      fluidRow(
        lineOpts("Axes","axis_line",3,FALSE),
        lineOpts("X Axis","axis_line_x",3,FALSE),
        lineOpts("Y Axis","axis_line_y",3,FALSE)
      )
    ),
    tabPanel("Ticks"),
    tabPanel("Legend"),
    tabPanel("Plotting Area"),
    tabPanel("Background"),
    tabPanel("Facet Labels"),
    tabPanel("The rest",
      jscolorInput("plotBgColour", "#000000"),
      numericInput('plotWidth', 'Plot Width', value = 600),
      div('x'),
      numericInput('plotHeight', 'Plot Height', value = 600)
    )
  ),
  hr(),
  plotOutput(outputId = 'plot', width = "100%", height = "100%")

))
