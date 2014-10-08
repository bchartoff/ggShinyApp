library(shiny)
library(ggplot2)

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

nineColors <- gg_color_hue(9)



dataset <- diamonds
lineTypes <- c("solid","blank","longdash","dotted","dashed","dotdash","twodash","F1","4C88C488","1F","12345678")
lineEnds <- c("butt","round","square")
 
shinyUI(fluidPage(theme = "css/main.css",
 
  headerPanel("ggplot2 Theme Builder"),

  navlistPanel(
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
        column(3,
          h4("All Lines"),
          selectizeInput("lineType",
            label = "type: ", 
            choices = lineTypes,
            options = list(render = I(sprintf(
              "{
                  option: function(item, escape) {
                    return '<div><img ' +
                        'src=\"images/lineTypes/' + item.value + '.png\" /></div>';
                  }
              }"
            )))),
          numericInput("lineSize",'stroke: ',value= theme_grey()$line$size),
          div("color: "),
          jscolorInput("lineColor", "#000000"),
          selectizeInput("lineEnd",
            label = "end: ",
            choices = lineEnds,
            options = list(render = I(sprintf(
              "{
                  option: function(item, escape) {
                    return '<div><img ' +
                        'src=\"images/lineEnds/' + item.value + '.png\" /></div>';
                  }
              }"
          ))))
        ),
        column(3,
          h4("All Rectangles"),
          numericInput("lineSizae",'stroke',value=NULL),
          numericInput("lineSizae",'stroke',value=NULL),
          numericInput("lineSizae",'stroke',value=NULL)
        ),
        column(3,
          h4("All Text"),
          numericInput("lineSizae",'stroke',value=NULL),
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

      selectInput("sampleChart", label = "Sample chart Type", 
          choices = list( "Bar chart: 1 color" = 1,
                          "Bar chart: 3 colors" = 2,
                          "Bar chart: 5 colors" = 3,
                          "Scatter plot: 3 colors" = 4,
                          "Scatter plot: 9 colors" = 5,
                          "Line chart: 3 colors" = 6), selected = 2),
      numericInput('plotWidth', 'Plot Width', value = 600),
      div('x'),
      numericInput('plotHeight', 'Plot Height', value = 600)
    )
  ),
  hr(),
  plotOutput(outputId = 'plot', width = "100%", height = "100%")

))
