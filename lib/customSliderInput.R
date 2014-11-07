### Functions to create custom slider, taken and modified from the R shiny package
### available under GPLv3 license.

source("lib/customSlider/R/customSlider.R")
library(shiny)

### %AND% function internal to shiny package, found in utils.R
### https://github.com/rstudio/shiny/blob/master/R/utils.R
`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

### controlLabel function internal to shiny package, found in bootstrip.R
### https://github.com/rstudio/shiny/blob/master/R/bootstrap.R
controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

### sliderInput function internal to shiny package, found in bootstrip.R
### https://github.com/rstudio/shiny/blob/master/R/bootstrap.R
customSliderInput <- function(inputId, label, min, max, value, step = NULL,
                        round=FALSE, format='#,##0.#####', locale='us',
                        ticks=TRUE, animate=FALSE, width=NULL) {

  if (identical(animate, TRUE))
    animate <- animationOptions()

  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton))
      animate$playButton <- tags$i(class='icon-play')
    if (is.null(animate$pauseButton))
      animate$pauseButton <- tags$i(class='icon-pause')
  }


  # build slider
  sliderTag <- customSlider(inputId, min=min, max=max, value=value, step=step,
    round=round, locale=locale, format=format, ticks=ticks, animate=animate,
    width=width)

  if (is.null(label)) {
    sliderTag
  } else {
    tags$div(
      controlLabel(inputId, label),
      sliderTag
    )
  }
}