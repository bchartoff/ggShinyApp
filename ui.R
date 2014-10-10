library(shiny)
library(ggplot2)
library(extrafont)

# source('lib/theme_default.R')


jscolorInput <- function(id, default) {
  tagList(
    singleton(tags$head(tags$script(src = "ShinySky/inst/www/ss-jscolor.js"))),
    singleton(tags$head(tags$script(src = "ShinySky/inst/www/jscolor/jscolor.js"))),
    singleton(tags$head(tags$link(rel = "stylsheet", type = "text/css", href = "css/main.css"))),
    tags$input(id = id, value = default, class = "color shiny-bound-input", type = "text", onchange = sprintf("$('#%s').trigger('afterChange')",id)),
    tags$script(sprintf("$('#%s').trigger('afterChange')",id))
  )
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

promote_list_item <- function(list,item){
  #delete item
  list[which(list==item)] <- NULL
  #append at start of list
  list <- c(item,list)
  list
}

lineOpts <- function(title,element,width){
  col <- column(width,
          h4(title),
          selectizeInput(paste(element,"Linetype",sep=""),
            label = "Type: ", 
            choices = promote_list_item(lineTypes, theme_current[element][[1]]["linetype"][[1]]),
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
          numericInput(paste(element,"Size",sep=""),'Stroke: ',value= theme_current[element][[1]]["size"][[1]]),
          tags$label("Color: "),
          jscolorInput(paste(element,"Color",sep=""), theme_current[element][[1]]["colour"][[1]]),
          selectizeInput(paste(element,"Lineend",sep=""),
            label = "End: ",
            choices = promote_list_item(lineEnds, theme_current[element][[1]]["linened"][[1]]),
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

rectOpts <- function(title,element,width){
  col <- column(width,
          h4(title),
          selectizeInput(paste(element,"Linetype",sep=""),
            label = "Type: ", 
            choices = promote_list_item(lineTypes, theme_current[element][[1]]["linetype"][[1]]),
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
          numericInput(paste(element,"Size",sep=""),'Stroke: ',value= theme_current[element][[1]]["size"][[1]]),
          tags$label("Fill: "),
          jscolorInput(paste(element,"Fill",sep=""), theme_current[element][[1]]["fill"][[1]]),
          tags$label("Stroke: "),
          jscolorInput(paste(element,"Color",sep=""), theme_current[element][[1]]["colour"][[1]])
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
          jscolorInput("rectColor", theme_current$element$colour)
        )
  col
}

theme_current <- theme_default()
nineColors <- gg_color_hue(9)

dataset <- diamonds
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
          selectInput("sampleChart", label = "Sample chart Type", 
            choices = list( "Bar chart: 1 color" = 1,
                          "Bar chart: 3 colors" = 2,
                          "Bar chart: 5 colors" = 3,
                          "Scatter plot: 3 colors" = 4,
                          "Scatter plot: 9 colors" = 5,
                          "Line chart: 3 colors" = 6), selected = 2)

        )
      )
    ),
    tabPanel("Global color palette",
      fluidRow(
        column(4,
          jscolorInput("palColor1", nineColors[1]),
          jscolorInput("palColor2", nineColors[4]),
          jscolorInput("palColor3", nineColors[7])
        ),
        column(4,
          jscolorInput("palColor4", nineColors[2]),
          jscolorInput("palColor5", nineColors[5]),
          jscolorInput("palColor6", nineColors[8])
        ),
        column(4,
          jscolorInput("palColor7", nineColors[3]),
          jscolorInput("palColor8", nineColors[6]),
          jscolorInput("palColor9", nineColors[9])
        )
      )
    ),
    tabPanel("Parent Elements",
      fluidRow(
        lineOpts("All Lines","line",3),
        rectOpts("All Rectangles","rect",3),
        
        column(3,
          h4("All Text"),
          selectizeInput("tmp",
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
    tabPanel("Axes"),
    tabPanel("Legend"),
    tabPanel("Plotting Area"),
    tabPanel("Background"),
    tabPanel("Facet Labels"),
    tabPanel("The rest",
      jscolorInput("plotBgColor", "#000000"),
      numericInput('plotWidth', 'Plot Width', value = 600),
      div('x'),
      numericInput('plotHeight', 'Plot Height', value = 600)
    )
  ),
  hr(),
  plotOutput(outputId = 'plot', width = "100%", height = "100%")

))
