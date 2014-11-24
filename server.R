library('shiny')
library('ggplot2')
library('reshape2')
library('RColorBrewer')
library('plyr')
library('stringr')

source('lib/grid/unit.R')
source('lib/theme_default.R')

shinyServer(function(input, output) {

  plotWidth <- function() {
    input$plotWidth
  }
  plotHeight <- function() {
    input$plotHeight
  }
  #################### Functions to Define custom colours #####################
  divlist<-c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")
  quallist<-c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
  seqlist<-c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd",
  "PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")

  divnum <- rep(11, length(divlist))
  qualnum <- c( 8, 8, 12, 9, 8, 9, 8, 12)
  seqnum <- rep(9, length(seqlist))

  namelist<-c(divlist,quallist,seqlist)
  maxcolours <- c(divnum,qualnum,seqnum)
  catlist<-rep(c("div","qual","seq"),c(length(divlist),length(quallist),length(seqlist)))

  custom.pal.info<-data.frame(maxcolours=maxcolours,category=catlist,row.names=namelist)


  custom.pal<-function(n,name){
     if(!(name %in% namelist)){
     stop(paste(name,"is not a valid palette name for custom.pal\n"))
     }
     if(n<3){
     warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
     return(custom.pal(3,name))
     }
     if(n>maxcolours[which(name==namelist)]){
     warning(paste("n too large, allowed maximum for palette",name,"is",maxcolours[which(name==namelist)]),
  "\nReturning the palette you asked for with that many colours\n")
     return(custom.pal(maxcolours[which(name==namelist)],name))
     }

    c1 <- col2rgb(input$palColour1)
    c2 <- col2rgb(input$palColour2)
    c3 <- col2rgb(input$palColour3)
    c4 <- col2rgb(input$palColour4)
    c5 <- col2rgb(input$palColour5)
    c6 <- col2rgb(input$palColour6)
    c7 <- col2rgb(input$palColour7)
    c8 <- col2rgb(input$palColour8)
    c9 <- col2rgb(input$palColour9)



     switch(name,

      Set1 =  switch(n,

  rgb(c(c1[1]),
      c(c1[2]),
      c(c1[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1]),
      c(c1[2],c2[2]),
      c(c1[3],c2[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1],c3[1]),
      c(c1[2],c2[2],c3[2]),
      c(c1[3],c2[3],c3[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1],c3[1],c4[1]),
      c(c1[2],c2[2],c3[2],c4[2]),
      c(c1[3],c2[3],c3[3],c4[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1]),
      c(c1[2],c2[2],c3[2],c4[2],c5[2]),
      c(c1[3],c2[3],c3[3],c4[3],c5[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1]),
      c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2]),
      c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1]),
      c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2]),
      c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1],c8[1]),
      c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2]),
      c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3],c8[3]),maxColorValue=255),
  rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1],c8[1],c9[1]),
      c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2],c9[2]),
      c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3],c8[3],c9[3]),maxColorValue=255),
  ),
      Set2 =  switch(n,
  rgb(c(154),
      c(62),
      c(37),maxColorValue=255),
  rgb(c(154,21),
      c(62,107),
      c(37,144),maxColorValue=255),
  rgb(c(154,21,112),
      c(62,107,130),
      c(37,144,89),maxColorValue=255)
  )
  )
  }

  display.custom.pal<-function(n,name){
     if(!(name %in% namelist)){
     stop(paste(name,"is not a valid palette name for custom.pal\n"))
     }
     if(n<3){
     warning("minimal value for n is 3, displaying requested palette with 3 different levels\n")
     return(display.custom.pal(3,name))
     }
     if(n>maxcolours[which(name==namelist)]){
     warning(paste("n too large, allowed maximum for palette",name,"is",maxcolours[which(name==namelist)]),
  "\nDisplaying the palette you asked for with that many colours\n")
     return(display.custom.pal(maxcolours[which(name==namelist)],name))
     }


     if(length(which(name==quallist))>0) palattr<-"(qualitative)"
     if(length(which(name==divlist))>0) palattr<-"(divergent)"
     if(length(which(name==seqlist))>0) palattr<-"(sequential)"
      image(1:n,1,as.matrix(1:n),col=custom.pal(n,name),
 xlab=paste(name,palattr),ylab="",xaxt="n",yaxt="n",bty="n")

  }

  display.custom.all <-
      function (n=NULL, type="all", select=NULL, exact.n=TRUE)
  {
      gaplist <- ""

      totallist <- c(divlist, gaplist, quallist,gaplist, seqlist)
      gapnum <- max(c(divnum,qualnum,seqnum))
      totnum <- c(divnum, gapnum, qualnum, gapnum, seqnum)
      if (!(type %in% c("div","qual","seq","all"))) {
  stop(paste(type, "is not a valid name for a colour list\n"))
      }
      colourlist <- switch(type, div=divlist,
          qual=quallist, seq=seqlist,
          all=totallist)
      maxnum <- switch(type, div=divnum,
       qual=qualnum,
       seq=seqnum,
       all=totnum)
      if(!is.null(select)){colourlist <- colourlist[select]
   maxnum <- maxnum[select]
   if(any(is.na(colourlist)))
       stop(paste("Illegal value(s) of select: ",
          paste(select[is.na(colourlist)],
        collapse=" ")))
       }
      palattr <-  switch(type,  qual="qualitative",  div
         ="divergent", seq="sequential",
         all="qualitative+divergent+sequential")
      if(is.null(n))n <- maxnum
      if(length(n)==1)n <- rep(n, length(colourlist))
      if(exact.n){
  keep <- n<=maxnum
  colourlist <- colourlist[keep]
  n <- n[keep]
  maxnum <- maxnum[keep]
      }
      if (any(n < 3) | exact.n & any(n>maxnum)|
  length(n)!=length(colourlist)){
  warning("Illegal vector of colour numbers")
  print(paste(n, collapse=" "))
      }
      n[n<3] <- 3
      n[n>maxnum] <- maxnum[n>maxnum]
      nr <- length(colourlist)
      nc <- max(n)
      ylim <- c(0,nr)
      oldpar <- par(mgp=c(2,0.25,0))
      on.exit(par(oldpar))
      plot(1,1,xlim=c(0,nc),ylim=ylim,type="n", axes=FALSE, bty="n",
   xlab="",ylab="")
      for(i in 1:nr)
      {nj <- n[i]
       if(colourlist[i]=="")next
       shadi <- custom.pal(nj, colourlist[i])
       rect(xleft=0:(nj-1), ybottom=i-1, xright=1:nj, ytop=i-0.2, col=shadi,
    border="light grey")
   }
      text(rep(-0.1,nr),(1:nr)-0.6, labels=colourlist, xpd=TRUE, adj=1)
  }


  pal_name <- function(palette, type) {
    if (is.character(palette)) {
      if (!palette %in% RColorBrewer:::namelist) {
        warning("Unknown palette ", palette)
        palette <- "Set1"
      }
      return(palette)
    }

    switch(type,
      div = divlist,
      qual = quallist,
      seq = seqlist,
      stop("Unknown palette type. Should be 'div', 'qual' or 'seq'",
        call. = FALSE)
    )[palette]
  }

  custom_pal <- function(type = "seq", palette = 1) {
    pal <- pal_name(palette, type)

    function(n) {
      if (n < 3)
        suppressWarnings(custom.pal(n, pal))[seq_len(n)]
      else
        custom.pal(n, pal)[seq_len(n)]
    }
  }

  scale_colour_custom <- function(..., type = "seq", palette = 1) {
    discrete_scale("colour", "custom", custom_pal(type, palette), ...)
  }

  #' @export
  #' @rdname scale_custom
  scale_fill_custom <- function(..., type = "seq", palette = 1) {
    discrete_scale("fill", "custom", custom_pal(type, palette), ...)
  }


linetype <- function(linetype){
  if(linetype == ""){
    val <- NULL
  }
  else{
    val <- as.integer(linetype)
  }
  val
}

size <- function(size){
  if(is.na(size)){
    val <- NULL
  }
  else{
    val <- size
  }
  val
}

key <- function(key){
  if(is.na(key)){
    val <- NULL
  }
  else{
    val <- unit(key,"points")
  }
  val
}

colour <- function(colour){
  if(colour == ""){
    val <- NULL
  }
  else{
    val <- colour
  }
  val
}

dropdownText <- function(text){
  if(text == ""){
    val <- NULL
  }
  else{
    val <- text
  }
  val
}

family <- function(family){
  if(family == "default"){
    val <- ""
  }
  else if(family == ""){
    val <- NULL
  }
  else{
    val <- family
  }
  val
}

slider <- function(slider, hidden){
  if(hidden == 0){
    val <- NULL
  }
  else{
    val <- slider
  }
  val
}

position <- function(text,px,py){
  if(is.na(px) || is.na(py)){
    val <- text
  }
  else{
    val <- c(px,py)
  }
  val
}

justification <- function(jx,jy){
  if(is.na(jx) || is.na(jy)){
    val <- NULL
  }
  else{
    val <- c(jx,jy)
  }
}

#tab 1
line.hide <- reactive({input$line_hide})
line.size <- reactive({size(input$line_size)})
line.colour <- reactive({colour(input$line_colour)})
line.linetype <- reactive({linetype(input$line_linetype)})
line.lineend <- reactive({dropdownText(input$line_lineend)})

rect.hide <- reactive({input$rect_hide})
rect.fill <- reactive({colour(input$rect_fill)})
rect.colour <- reactive({colour(input$rect_colour)})
rect.size <- reactive({size(input$rect_size)})
rect.linetype <- reactive({linetype(input$rect_linetype)})

text.hide <- reactive({input$text_hide})
text.family <- reactive({family(input$text_family)})
text.face <- reactive({dropdownText(input$text_face)})
text.size <- reactive({size(input$text_size)})
text.colour <- reactive({colour(input$text_colour)})
text.hjust <- reactive({slider(input$text_hjust,input$text_hjust_hidden)})
text.vjust <- reactive({slider(input$text_vjust,input$text_vjust_hidden)})
text.angle <- reactive({slider(input$text_angle,input$text_angle_hidden)})
text.lineheight <- reactive({size(input$text_lineheight)})

title.hide <- reactive({input$title_hide})
title.family <- reactive({family(input$title_family)})
title.face <- reactive({dropdownText(input$title_face)})
title.size <- reactive({size(input$title_size)})
title.colour <- reactive({colour(input$title_colour)})
title.hjust <- reactive({slider(input$title_hjust,input$title_hjust_hidden)})
title.vjust <- reactive({slider(input$title_vjust,input$title_vjust_hidden)})
title.angle <- reactive({slider(input$title_angle,input$title_angle_hidden)})
title.lineheight <- reactive({size(input$title_lineheight)})

#tab 2
axis.line.hide <- reactive({input$axis_line_hide})
axis.line.size <- reactive({size(input$axis_line_size)})
axis.line.colour <- reactive({colour(input$axis_line_colour)})
axis.line.linetype <- reactive({linetype(input$axis_line_linetype)})
axis.line.lineend <- reactive({dropdownText(input$axis_line_lineend)})

axis.line.x.hide <- reactive({input$axis_line_x_hide})
axis.line.x.size <- reactive({size(input$axis_line_x_size)})
axis.line.x.colour <- reactive({colour(input$axis_line_x_colour)})
axis.line.x.linetype <- reactive({linetype(input$axis_line_x_linetype)})
axis.line.x.lineend <- reactive({dropdownText(input$axis_line_x_lineend)})

axis.line.y.hide <- reactive({input$axis_line_y_hide})
axis.line.y.size <- reactive({size(input$axis_line_y_size)})
axis.line.y.colour <- reactive({colour(input$axis_line_y_colour)})
axis.line.y.linetype <- reactive({linetype(input$axis_line_y_linetype)})
axis.line.y.lineend <- reactive({dropdownText(input$axis_line_y_lineend)})

#tab 3
axis.title.hide <- reactive({input$axis_title_hide})
axis.title.family <- reactive({family(input$axis_title_family)})
axis.title.face <- reactive({dropdownText(input$axis_title_face)})
axis.title.size <- reactive({size(input$axis_title_size)})
axis.title.colour <- reactive({colour(input$axis_title_colour)})
axis.title.hjust <- reactive({slider(input$axis_title_hjust,input$axis_title_hjust_hidden)})
axis.title.vjust <- reactive({slider(input$axis_title_vjust,input$axis_title_vjust_hidden)})
axis.title.angle <- reactive({slider(input$axis_title_angle,input$axis_title_angle_hidden)})
axis.title.lineheight <- reactive({size(input$axis_title_lineheight)})

axis.title.x.hide <- reactive({input$axis_title_x_hide})
axis.title.x.family <- reactive({family(input$axis_title_x_family)})
axis.title.x.face <- reactive({dropdownText(input$axis_title_x_face)})
axis.title.x.size <- reactive({size(input$axis_title_x_size)})
axis.title.x.colour <- reactive({colour(input$axis_title_x_colour)})
axis.title.x.hjust <- reactive({slider(input$axis_title_x_hjust,input$axis_title_x_hjust_hidden)})
axis.title.x.vjust <- reactive({slider(input$axis_title_x_vjust,input$axis_title_x_vjust_hidden)})
axis.title.x.angle <- reactive({slider(input$axis_title_x_angle,input$axis_title_x_angle_hidden)})
axis.title.x.lineheight <- reactive({size(input$axis_title_x_lineheight)})

axis.title.y.hide <- reactive({input$axis_title_y_hide})
axis.title.y.family <- reactive({family(input$axis_title_y_family)})
axis.title.y.face <- reactive({dropdownText(input$axis_title_y_face)})
axis.title.y.size <- reactive({size(input$axis_title_y_size)})
axis.title.y.colour <- reactive({colour(input$axis_title_y_colour)})
axis.title.y.hjust <- reactive({slider(input$axis_title_y_hjust,input$axis_title_y_hjust_hidden)})
axis.title.y.vjust <- reactive({slider(input$axis_title_y_vjust,input$axis_title_y_vjust_hidden)})
axis.title.y.angle <- reactive({slider(input$axis_title_y_angle,input$axis_title_y_angle_hidden)})
axis.title.y.lineheight <- reactive({size(input$axis_title_y_lineheight)})

#tab 4
axis.ticks.hide <- reactive({input$axis_ticks_hide})
axis.ticks.size <- reactive({size(input$axis_ticks_size)})
axis.ticks.colour <- reactive({colour(input$axis_ticks_colour)})
axis.ticks.linetype <- reactive({linetype(input$axis_ticks_linetype)})
axis.ticks.lineend <- reactive({dropdownText(input$axis_ticks_lineend)})

axis.ticks.x.hide <- reactive({input$axis_ticks_x_hide})
axis.ticks.x.size <- reactive({size(input$axis_ticks_x_size)})
axis.ticks.x.colour <- reactive({colour(input$axis_ticks_x_colour)})
axis.ticks.x.linetype <- reactive({linetype(input$axis_ticks_x_linetype)})
axis.ticks.x.lineend <- reactive({dropdownText(input$axis_ticks_x_lineend)})

axis.ticks.y.hide <- reactive({input$axis_ticks_y_hide})
axis.ticks.y.size <- reactive({size(input$axis_ticks_y_size)})
axis.ticks.y.colour <- reactive({colour(input$axis_ticks_y_colour)})
axis.ticks.y.linetype <- reactive({linetype(input$axis_ticks_y_linetype)})
axis.ticks.y.lineend <- reactive({dropdownText(input$axis_ticks_y_lineend)})

axis.ticks.length <- reactive({size(input$axis_ticks_length)})
axis.ticks.margin <- reactive({size(input$axis_ticks_margin)})

#tab 5
axis.text.hide <- reactive({input$axis_text_hide})
axis.text.family <- reactive({family(input$axis_text_family)})
axis.text.face <- reactive({dropdownText(input$axis_text_face)})
axis.text.size <- reactive({size(input$axis_text_size)})
axis.text.colour <- reactive({colour(input$axis_text_colour)})
axis.text.hjust <- reactive({slider(input$axis_text_hjust,input$axis_text_hjust_hidden)})
axis.text.vjust <- reactive({slider(input$axis_text_vjust,input$axis_text_vjust_hidden)})
axis.text.angle <- reactive({slider(input$axis_text_angle,input$axis_text_angle_hidden)})
axis.text.lineheight <- reactive({size(input$axis_text_lineheight)})

axis.text.x.hide <- reactive({input$axis_text_x_hide})
axis.text.x.family <- reactive({family(input$axis_text_x_family)})
axis.text.x.face <- reactive({dropdownText(input$axis_text_x_face)})
axis.text.x.size <- reactive({size(input$axis_text_x_size)})
axis.text.x.colour <- reactive({colour(input$axis_text_x_colour)})
axis.text.x.hjust <- reactive({slider(input$axis_text_x_hjust,input$axis_text_x_hjust_hidden)})
axis.text.x.vjust <- reactive({slider(input$axis_text_x_vjust,input$axis_text_x_vjust_hidden)})
axis.text.x.angle <- reactive({slider(input$axis_text_x_angle,input$axis_text_x_angle_hidden)})
axis.text.x.lineheight <- reactive({size(input$axis_text_x_lineheight)})

axis.text.y.hide <- reactive({input$axis_text_y_hide})
axis.text.y.family <- reactive({family(input$axis_text_y_family)})
axis.text.y.face <- reactive({dropdownText(input$axis_text_y_face)})
axis.text.y.size <- reactive({size(input$axis_text_y_size)})
axis.text.y.colour <- reactive({colour(input$axis_text_y_colour)})
axis.text.y.hjust <- reactive({slider(input$axis_text_y_hjust,input$axis_text_y_hjust_hidden)})
axis.text.y.vjust <- reactive({slider(input$axis_text_y_vjust,input$axis_text_y_vjust_hidden)})
axis.text.y.angle <- reactive({slider(input$axis_text_y_angle,input$axis_text_y_angle_hidden)})
axis.text.y.lineheight <- reactive({size(input$axis_text_y_lineheight)})

#tab 6
legend.background.hide <- reactive({input$legend_background_hide})
legend.background.fill <- reactive({colour(input$legend_background_fill)})
legend.background.colour <- reactive({colour(input$legend_background_colour)})
legend.background.size <- reactive({size(input$legend_background_size)})
legend.background.linetype <- reactive({linetype(input$legend_background_linetype)})

legend.margin <- reactive({size(input$legend_margin)})

legend.key.hide <- reactive({input$legend_key_hide})
legend.key.fill <- reactive({colour(input$legend_key_fill)})
legend.key.colour <- reactive({colour(input$legend_key_colour)})
legend.key.size <- reactive({size(input$legend_key_size)})
legend.key.linetype <- reactive({linetype(input$legend_key_linetype)})

legend.key.dimensions <- reactive({size(input$legend_key_dimensions)})
legend.key.height <- reactive({key(input$legend_key_height)})
legend.key.width <- reactive({key(input$legend_key_width)})

legend.direction <- reactive({dropdownText(input$legend_direction)})
legend.box <- reactive({dropdownText(input$legend_box)})
legend.position <- reactive({position(dropdownText(input$legend_position),input$legend_position_x,input$legend_position_y)})
legend.justification <- reactive({justification(input$legend_justification_x,input$legend_justification_y)})

###################

#tab 7
legend.title.hide <- reactive({input$legend_title_hide})
legend.title.family <- reactive({family(input$legend_title_family)})
legend.title.face <- reactive({dropdownText(input$legend_title_face)})
legend.title.size <- reactive({size(input$legend_title_size)})
legend.title.colour <- reactive({colour(input$legend_title_colour)})
legend.title.hjust <- reactive({slider(input$legend_title_hjust,input$legend_title_hjust_hidden)})
legend.title.vjust <- reactive({slider(input$legend_title_vjust,input$legend_title_vjust_hidden)})
legend.title.angle <- reactive({slider(input$legend_title_angle,input$legend_title_angle_hidden)})
legend.title.lineheight <- reactive({size(input$legend_title_lineheight)})

legend.text.hide <- reactive({input$legend_text_hide})
legend.text.family <- reactive({family(input$legend_text_family)})
legend.text.face <- reactive({dropdownText(input$legend_text_face)})
legend.text.size <- reactive({size(input$legend_text_size)})
legend.text.colour <- reactive({colour(input$legend_text_colour)})
legend.text.hjust <- reactive({slider(input$legend_text_hjust,input$legend_text_hjust_hidden)})
legend.text.vjust <- reactive({slider(input$legend_text_vjust,input$legend_text_vjust_hidden)})
legend.text.angle <- reactive({slider(input$legend_text_angle,input$legend_text_angle_hidden)})
legend.text.lineheight <- reactive({size(input$legend_text_lineheight)})

legend.title.align <- reactive({slider(input$legend_title_align,input$legend_title_align_hidden)})
legend.text.align <- reactive({slider(input$legend_text_align,input$legend_text_align_hidden)})

#tab 8
panel.background.hide <- reactive({input$panel_background_hide})
panel.background.fill <- reactive({colour(input$panel_background_fill)})
panel.background.colour <- reactive({colour(input$panel_background_colour)})
panel.background.size <- reactive({size(input$panel_background_size)})
panel.background.linetype <- reactive({linetype(input$panel_background_linetype)})

panel.border.hide <- reactive({input$panel_border_hide})
panel.border.fill <- function(){NA}
panel.border.colour <- reactive({colour(input$panel_border_colour)})
panel.border.size <- reactive({size(input$panel_border_size)})
panel.border.linetype <- reactive({linetype(input$panel_border_linetype)})

panel.grid.hide <- reactive({input$panel_grid_hide})
panel.grid.size <- reactive({size(input$panel_grid_size)})
panel.grid.colour <- reactive({colour(input$panel_grid_colour)})
panel.grid.linetype <- reactive({linetype(input$panel_grid_linetype)})
panel.grid.lineend <- reactive({dropdownText(input$panel_grid_lineend)})


#tab 9
panel.grid.major.hide <- reactive({input$panel_grid_major_hide})
panel.grid.major.size <- reactive({size(input$panel_grid_major_size)})
panel.grid.major.colour <- reactive({colour(input$panel_grid_major_colour)})
panel.grid.major.linetype <- reactive({linetype(input$panel_grid_major_linetype)})
panel.grid.major.lineend <- reactive({dropdownText(input$panel_grid_major_lineend)})

panel.grid.major.x.hide <- reactive({input$panel_grid_major_x_hide})
panel.grid.major.x.size <- reactive({size(input$panel_grid_major_x_size)})
panel.grid.major.x.colour <- reactive({colour(input$panel_grid_major_x_colour)})
panel.grid.major.x.linetype <- reactive({linetype(input$panel_grid_major_x_linetype)})
panel.grid.major.x.lineend <- reactive({dropdownText(input$panel_grid_major_x_lineend)})

panel.grid.major.y.hide <- reactive({input$panel_grid_major_y_hide})
panel.grid.major.y.size <- reactive({size(input$panel_grid_major_y_size)})
panel.grid.major.y.colour <- reactive({colour(input$panel_grid_major_y_colour)})
panel.grid.major.y.linetype <- reactive({linetype(input$panel_grid_major_y_linetype)})
panel.grid.major.y.lineend <- reactive({dropdownText(input$panel_grid_major_y_lineend)})

#tab 10
panel.grid.minor.hide <- reactive({input$panel_grid_minor_hide})
panel.grid.minor.size <- reactive({size(input$panel_grid_minor_size)})
panel.grid.minor.colour <- reactive({colour(input$panel_grid_minor_colour)})
panel.grid.minor.linetype <- reactive({linetype(input$panel_grid_minor_linetype)})
panel.grid.minor.lineend <- reactive({dropdownText(input$panel_grid_minor_lineend)})

panel.grid.minor.x.hide <- reactive({input$panel_grid_minor_x_hide})
panel.grid.minor.x.size <- reactive({size(input$panel_grid_minor_x_size)})
panel.grid.minor.x.colour <- reactive({colour(input$panel_grid_minor_x_colour)})
panel.grid.minor.x.linetype <- reactive({linetype(input$panel_grid_minor_x_linetype)})
panel.grid.minor.x.lineend <- reactive({dropdownText(input$panel_grid_minor_x_lineend)})

panel.grid.minor.y.hide <- reactive({input$panel_grid_minor_y_hide})
panel.grid.minor.y.size <- reactive({size(input$panel_grid_minor_y_size)})
panel.grid.minor.y.colour <- reactive({colour(input$panel_grid_minor_y_colour)})
panel.grid.minor.y.linetype <- reactive({linetype(input$panel_grid_minor_y_linetype)})
panel.grid.minor.y.lineend <- reactive({dropdownText(input$panel_grid_minor_y_lineend)})

#tab 11
plot.background.hide <- reactive({input$plot_background_hide})
plot.background.fill <- reactive({colour(input$plot_background_fill)})
plot.background.colour <- reactive({colour(input$plot_background_colour)})
plot.background.size <- reactive({size(input$plot_background_size)})
plot.background.linetype <- reactive({linetype(input$plot_background_linetype)})

plot.title.hide <- reactive({input$plot_title_hide})
plot.title.family <- reactive({family(input$plot_title_family)})
plot.title.face <- reactive({dropdownText(input$plot_title_face)})
plot.title.size <- reactive({size(input$plot_title_size)})
plot.title.colour <- reactive({colour(input$plot_title_colour)})
plot.title.hjust <- reactive({slider(input$plot_title_hjust,input$plot_title_hjust_hidden)})
plot.title.vjust <- reactive({slider(input$plot_title_vjust,input$plot_title_vjust_hidden)})
plot.title.angle <- reactive({slider(input$plot_title_angle,input$plot_title_angle_hidden)})
plot.title.lineheight <- reactive({size(input$plot_title_lineheight)})

plot.margin.top <- reactive({size(input$plot_margin_top)})
plot.margin.right <- reactive({size(input$plot_margin_right)})
plot.margin.bottom <- reactive({size(input$plot_margin_bottom)})
plot.margin.left <- reactive({size(input$plot_margin_left)})

#tab 12
strip.background.hide <- reactive({input$strip_background_hide})
strip.background.fill <- reactive({colour(input$strip_background_fill)})
strip.background.colour <- reactive({colour(input$strip_background_colour)})
strip.background.size <- reactive({size(input$strip_background_size)})
strip.background.linetype <- reactive({linetype(input$strip_background_linetype)})

panel.margin <- reactive({input$panel_margin})

strip.text.hide <- reactive({input$strip_text_hide})
strip.text.family <- reactive({family(input$strip_text_family)})
strip.text.face <- reactive({dropdownText(input$strip_text_face)})
strip.text.size <- reactive({size(input$strip_text_size)})
strip.text.colour <- reactive({colour(input$strip_text_colour)})
strip.text.hjust <- reactive({slider(input$strip_text_hjust,input$strip_text_hjust_hidden)})
strip.text.vjust <- reactive({slider(input$strip_text_vjust,input$strip_text_vjust_hidden)})
strip.text.angle <- reactive({slider(input$strip_text_angle,input$strip_text_angle_hidden)})
strip.text.lineheight <- reactive({size(input$strip_text_lineheight)})

strip.text.x.hide <- reactive({input$strip_text_x_hide})
strip.text.x.family <- reactive({family(input$strip_text_x_family)})
strip.text.x.face <- reactive({dropdownText(input$strip_text_x_face)})
strip.text.x.size <- reactive({size(input$strip_text_x_size)})
strip.text.x.colour <- reactive({colour(input$strip_text_x_colour)})
strip.text.x.hjust <- reactive({slider(input$strip_text_x_hjust,input$strip_text_x_hjust_hidden)})
strip.text.x.vjust <- reactive({slider(input$strip_text_x_vjust,input$strip_text_x_vjust_hidden)})
strip.text.x.angle <- reactive({slider(input$strip_text_x_angle,input$strip_text_x_angle_hidden)})
strip.text.x.lineheight <- reactive({size(input$strip_text_x_lineheight)})

strip.text.y.hide <- reactive({input$strip_text_y_hide})
strip.text.y.family <- reactive({family(input$strip_text_y_family)})
strip.text.y.face <- reactive({dropdownText(input$strip_text_y_face)})
strip.text.y.size <- reactive({size(input$strip_text_y_size)})
strip.text.y.colour <- reactive({colour(input$strip_text_y_colour)})
strip.text.y.hjust <- reactive({slider(input$strip_text_y_hjust,input$strip_text_y_hjust_hidden)})
strip.text.y.vjust <- reactive({slider(input$strip_text_y_vjust,input$strip_text_y_vjust_hidden)})
strip.text.y.angle <- reactive({slider(input$strip_text_y_angle,input$strip_text_y_angle_hidden)})
strip.text.y.lineheight <- reactive({size(input$strip_text_y_lineheight)})

pal.color.1 <- reactive(input$palColour1)
pal.color.2 <- reactive(input$palColour2)
pal.color.3 <- reactive(input$palColour3)
pal.color.4 <- reactive(input$palColour4)
pal.color.5 <- reactive(input$palColour5)
pal.color.6 <- reactive(input$palColour6)
pal.color.7 <- reactive(input$palColour7)
pal.color.8 <- reactive(input$palColour8)
pal.color.9 <- reactive(input$palColour9)




  output$plot <- renderPlot({

  scale_colour_discrete <- function(...) scale_colour_custom(..., palette="Set1")
  scale_fill_discrete <- function(...) scale_fill_custom(... , palette="Set1")
  t = custom_pal(9,palette="Set1")
  extendedPalette = colorRampPalette(custom_pal(palette="Set1")(9))


    theme_set(theme_default())

    theme_update(
      #tab 1
      line = element_line(size=line.size(),colour=line.colour(),linetype=line.linetype(),lineend=line.lineend()),
      rect = element_rect(fill = rect.fill(), colour = rect.colour(), size = rect.size(), linetype = rect.linetype()),
      text = element_text(family=text.family(),face=text.face(),size=text.size(),colour=text.colour(),hjust=text.hjust(),vjust=text.vjust(),angle=text.angle(),lineheight=text.lineheight()),
      title = element_text(family=title.family(),face=title.face(),size=title.size(),colour=title.colour(),hjust=title.hjust(),vjust=title.vjust(),angle=title.angle(),lineheight=title.lineheight()),

      #tab 2
      axis.line = element_line(size=axis.line.size(),linetype=axis.line.linetype(),colour=axis.line.colour(), lineend=axis.line.lineend()),
      axis.line.x = element_line(size=axis.line.x.size(), colour=axis.line.x.colour(), linetype=axis.line.x.linetype(), lineend=axis.line.x.lineend()),
      axis.line.y = element_line(size=axis.line.y.size(),colour=axis.line.y.colour(),linetype=axis.line.y.linetype(),lineend=axis.line.y.lineend()),

      #tab 3
      axis.title = element_text(family=axis.title.family(),face=axis.title.face(),size=axis.title.size(),colour=axis.title.colour(),hjust=axis.title.hjust(),vjust=axis.title.vjust(),angle=axis.title.angle(),lineheight=axis.title.lineheight()),
      axis.title.x = element_text(family=axis.title.x.family(),face=axis.title.x.face(),size=axis.title.x.size(),colour=axis.title.x.colour(),hjust=axis.title.x.hjust(),vjust=axis.title.x.vjust(),angle=axis.title.x.angle(),lineheight=axis.title.x.lineheight()),
      axis.title.y = element_text(family=axis.title.y.family(),face=axis.title.y.face(),size=axis.title.y.size(),colour=axis.title.y.colour(),hjust=axis.title.y.hjust(),vjust=axis.title.y.vjust(),angle=axis.title.y.angle(),lineheight=axis.title.y.lineheight()),

      # #tab 4
      axis.ticks = element_line(size=axis.ticks.size(),linetype=axis.ticks.linetype(),colour=axis.ticks.colour(), lineend=axis.ticks.lineend()),
      axis.ticks.x = element_line(size=axis.ticks.x.size(), colour=axis.ticks.x.colour(), linetype=axis.ticks.x.linetype(), lineend=axis.ticks.x.lineend()),
      axis.ticks.y = element_line(size=axis.ticks.y.size(),colour=axis.ticks.y.colour(),linetype=axis.ticks.y.linetype(),lineend=axis.ticks.y.lineend()),
      axis.ticks.length = unit(axis.ticks.length(),"points"),
      axis.ticks.margin = unit(axis.ticks.margin(),"points"),

      # #tab 5
      axis.text = element_text(family=axis.text.family(),face=axis.text.face(),size=axis.text.size(),colour=axis.text.colour(),hjust=axis.text.hjust(),vjust=axis.text.vjust(),angle=axis.text.angle(),lineheight=axis.text.lineheight()),
      axis.text.x = element_text(family=axis.text.x.family(),face=axis.text.x.face(),size=axis.text.x.size(),colour=axis.text.x.colour(),hjust=axis.text.x.hjust(),vjust=axis.text.x.vjust(),angle=axis.text.x.angle(),lineheight=axis.text.x.lineheight()),
      axis.text.y = element_text(family=axis.text.y.family(),face=axis.text.y.face(),size=axis.text.y.size(),colour=axis.text.y.colour(),hjust=axis.text.y.hjust(),vjust=axis.text.y.vjust(),angle=axis.text.y.angle(),lineheight=axis.text.y.lineheight()),

      # #tab 6
      legend.background = element_rect(fill = legend.background.fill(), colour = legend.background.colour(), size = legend.background.size(), linetype = legend.background.linetype()),
      legend.key = element_rect(fill = legend.key.fill(), colour = legend.key.colour(), size = legend.key.size(), linetype = legend.key.linetype()),
      legend.key.size = unit(legend.key.dimensions(),"points"),
      legend.key.height = legend.key.height(),
      legend.key.width = legend.key.width(),
      legend.margin = unit(legend.margin(),"points"),
      legend.direction = legend.direction(),
      legend.box = legend.box(),
      legend.position = legend.position(),
      legend.justification = legend.justification(),

      # #tab 7
      legend.title = element_text(family=legend.title.family(),face=legend.title.face(),size=legend.title.size(),colour=legend.title.colour(),hjust=legend.title.hjust(),vjust=legend.title.vjust(),angle=legend.title.angle(),lineheight=legend.title.lineheight()),
      legend.text = element_text(family=legend.text.family(),face=legend.text.face(),size=legend.text.size(),colour=legend.text.colour(),hjust=legend.text.hjust(),vjust=legend.text.vjust(),angle=legend.text.angle(),lineheight=legend.text.lineheight()),
      legend.title.align = legend.title.align(),
      legend.text.align = legend.text.align(),

      # #tab 8
      panel.background = element_rect(fill = panel.background.fill(), colour = panel.background.colour(), size = panel.background.size(), linetype = panel.background.linetype()),
      panel.border = element_rect(fill = panel.border.fill(), colour = panel.border.colour(), size = panel.border.size(), linetype = panel.border.linetype()),
      panel.grid = element_line(size=panel.grid.size(),linetype=panel.grid.linetype(),colour=panel.grid.colour(), lineend=panel.grid.lineend()),

      # #tab 9
      panel.grid.major = element_line(size=panel.grid.major.size(),linetype=panel.grid.major.linetype(),colour=panel.grid.major.colour(), lineend=panel.grid.major.lineend()),
      panel.grid.major.x = element_line(size=panel.grid.major.x.size(),linetype=panel.grid.major.x.linetype(),colour=panel.grid.major.x.colour(), lineend=panel.grid.major.x.lineend()),
      panel.grid.major.y = element_line(size=panel.grid.major.y.size(),linetype=panel.grid.major.y.linetype(),colour=panel.grid.major.y.colour(), lineend=panel.grid.major.y.lineend()),

      # #tab 10
      panel.grid.minor = element_line(size=panel.grid.minor.size(),linetype=panel.grid.minor.linetype(),colour=panel.grid.minor.colour(), lineend=panel.grid.minor.lineend()),
      panel.grid.minor.x = element_line(size=panel.grid.minor.x.size(),linetype=panel.grid.minor.x.linetype(),colour=panel.grid.minor.x.colour(), lineend=panel.grid.minor.x.lineend()),
      panel.grid.minor.y = element_line(size=panel.grid.minor.y.size(),linetype=panel.grid.minor.y.linetype(),colour=panel.grid.minor.y.colour(), lineend=panel.grid.minor.y.lineend()),

      # #tab 11
      plot.background = element_rect(fill = plot.background.fill(), colour = plot.background.colour(), size = plot.background.size(), linetype = plot.background.linetype()),
      plot.title = element_text(family=plot.title.family(),face=plot.title.face(),size=plot.title.size(),colour=plot.title.colour(),hjust=plot.title.hjust(),vjust=plot.title.vjust(),angle=plot.title.angle(),lineheight=plot.title.lineheight()),
      plot.margin = unit(c(plot.margin.top(),plot.margin.right(),plot.margin.bottom(),plot.margin.left()), "points"),

      #tab 12
      strip.background = element_rect(fill = strip.background.fill(), colour = strip.background.colour(), size = strip.background.size(), linetype = strip.background.linetype()),
      panel.margin = unit(panel.margin(),"points"),
      strip.text = element_text(family=strip.text.family(),face=strip.text.face(),size=strip.text.size(),colour=strip.text.colour(),hjust=strip.text.hjust(),vjust=strip.text.vjust(),angle=strip.text.angle(),lineheight=strip.text.lineheight()),
      strip.text.x = element_text(family=strip.text.x.family(),face=strip.text.x.face(),size=strip.text.x.size(),colour=strip.text.x.colour(),hjust=strip.text.x.hjust(),vjust=strip.text.x.vjust(),angle=strip.text.x.angle(),lineheight=strip.text.x.lineheight()),
      strip.text.y = element_text(family=strip.text.y.family(),face=strip.text.y.face(),size=strip.text.y.size(),colour=strip.text.y.colour(),hjust=strip.text.y.hjust(),vjust=strip.text.y.vjust(),angle=strip.text.y.angle(),lineheight=strip.text.y.lineheight())
    )

    if(line.hide()){theme_update(line = element_blank())}
    if(rect.hide()){theme_update(rect = element_blank())}
    if(title.hide()){theme_update(title = element_blank())}
    if(text.hide()){theme_update(text = element_blank())}

    if(axis.line.hide()){theme_update(axis.line = element_blank())}
    if(axis.line.x.hide()){theme_update(axis.line.x = element_blank())}
    if(axis.line.y.hide()){theme_update(axis.line.y = element_blank())}

    if(axis.title.hide()){theme_update(axis.title = element_blank())}
    if(axis.title.x.hide()){theme_update(axis.title.x = element_blank())}
    if(axis.title.y.hide()){theme_update(axis.title.y = element_blank())}

    if(axis.ticks.hide()){theme_update(axis.ticks = element_blank())}
    if(axis.ticks.x.hide()){theme_update(axis.ticks.x = element_blank())}
    if(axis.ticks.y.hide()){theme_update(axis.ticks.y = element_blank())}

    if(axis.text.hide()){theme_update(axis.text = element_blank())}
    if(axis.text.x.hide()){theme_update(axis.text.x = element_blank())}
    if(axis.text.y.hide()){theme_update(axis.text.y = element_blank())}

    if(legend.background.hide()){theme_update(legend.background = element_blank())}
    if(legend.key.hide()){theme_update(legend.key = element_blank())}

    if(legend.title.hide()){theme_update(legend.title = element_blank())}
    if(legend.text.hide()){theme_update(legend.text = element_blank())}

    if(panel.grid.hide()){theme_update(panel.grid = element_blank())}
    if(panel.background.hide()){theme_update(panel.background = element_blank())}
    if(panel.border.hide()){theme_update(panel.border = element_blank())}

    if(panel.grid.major.hide()){theme_update(panel.grid.major = element_blank())}
    if(panel.grid.major.x.hide()){theme_update(panel.grid.major.x = element_blank())}
    if(panel.grid.major.y.hide()){theme_update(panel.grid.major.y = element_blank())}

    if(panel.grid.minor.hide()){theme_update(panel.grid.minor = element_blank())}
    if(panel.grid.minor.x.hide()){theme_update(panel.grid.minor.x = element_blank())}
    if(panel.grid.minor.y.hide()){theme_update(panel.grid.minor.y = element_blank())}

    if(plot.background.hide()){theme_update(plot.background = element_blank())}
    if(plot.title.hide()){theme_update(plot.title = element_blank())}

    if(strip.background.hide()){theme_update(strip.background = element_blank())}
    if(strip.text.hide()){theme_update(strip.text = element_blank())}
    if(strip.text.x.hide()){theme_update(strip.text.x = element_blank())}
    if(strip.text.y.hide()){theme_update(strip.text.y = element_blank())}

#Note, because of Shiny reactions, I add `scale_fill_discrete()` or `scale_colour_discrete()` to each plot manually
#When the final (downloadable) script is run, there is no need to add color scales to each graph object, they
#are set as defaults.
    if (input$sampleChart == 1){
        print(ggplot(mtcars, aes(factor(cyl))) + geom_bar() + scale_fill_discrete() + coord_cartesian(ylim = c(0, 25))+ggtitle("Title"))
    }
    else if (input$sampleChart == 2){
        print(qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl)) + coord_cartesian(ylim = c(0, 15)) + ggtitle("Title"))
    }
    else if (input$sampleChart == 3){
        print(ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar() + scale_fill_discrete() + ggtitle("Title") + coord_cartesian(ylim = c(0, 15000)))
    }
    else if (input$sampleChart == 4){
        print(ggplot(mtcars, aes(wt, mpg))+geom_point(aes(colour = factor(cyl))) + scale_colour_discrete() + ggtitle("Title"))
    }
    else if (input$sampleChart == 5){
        dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
        d <- qplot(carat, price, data=dsamp, colour=clarity, size = 3)
        print(d+ggtitle("Title"))
    }
    else if (input$sampleChart == 6){
        mtcars.long <- melt(mtcars, id = "mpg", measure = c("disp", "hp", "wt"))
        print(ggplot(mtcars.long, aes(mpg, value, colour = variable)) + scale_colour_discrete() + geom_line()+ggtitle("Title"))
    }
    else if (input$sampleChart == 7){
      p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + scale_colour_discrete() + ggtitle("Title")
      print(p + facet_grid(vs ~ am, margins=TRUE))
    }
    else if (input$sampleChart == 8){
      print(ggplot(mtcars) + scale_fill_manual(values = extendedPalette(22)) + geom_histogram(aes(factor(hp), fill=factor(hp))))
    }
 
    
  }, height = plotHeight, width = plotWidth)


parse_theme <- function(name,element){
  in_str <- paste(deparse(element),collapse="")
  #str_match returns df w/ 1st column entire string, then match groups in subsequent columns
  #so match groups are 2-indexed (vs 1-indexed or 0-indexed)
  type <- str_match(in_str,"(\")(element_[a-z]+)(\")")[,3]
  if(in_str == "NULL"){
    out_str <- "NULL"
  }
  else if(grepl("element_blank",in_str)){
    out_str <- "element_blank()"
  }
  else if(grepl(".Names",in_str)){
    obj <- str_match(in_str,"(^structure\\(list\\()(.*)(, \\.Names)")[,3]
    out_str <- paste(type,"(",obj,sep="")
  }
  else if(grepl("structure",in_str)){
    match <- str_match(in_str,"(^structure\\()(.*)(, unit = )(\".*\")(,)")
    size <- match[,3]
    unit <- match[,5]
    out_str <- paste("unit(",size,", ",unit,")",sep="")
  }
  else{
    out_str <- in_str
  }
  val <- paste(name,"=",out_str)
  val
}



output$downloadData <- downloadHandler(
  filename = function() {
    "ggplot_styling.R"
  },
  content = function(file) {
    template <- readLines("output_template.R")
    output <- gsub("REPLACE_COLOR_ONE",toString(pal.color.1()),template)
    output <- gsub("REPLACE_COLOR_TWO",toString(pal.color.2()),output)
    output <- gsub("REPLACE_COLOR_THREE",toString(pal.color.3()),output)
    output <- gsub("REPLACE_COLOR_FOUR",toString(pal.color.4()),output)
    output <- gsub("REPLACE_COLOR_FIVE",toString(pal.color.5()),output)
    output <- gsub("REPLACE_COLOR_SIX",toString(pal.color.6()),output)
    output <- gsub("REPLACE_COLOR_SEVEN",toString(pal.color.7()),output)
    output <- gsub("REPLACE_COLOR_EIGHT",toString(pal.color.8()),output)
    output <- gsub("REPLACE_COLOR_NINE",toString(pal.color.9()),output)

    output <- gsub("REPLACE_PLOT_WIDTH",toString(plotWidth()/72.0),output)
    output <- gsub("REPLACE_PLOT_HEIGHT",toString(plotHeight()/72.0),output)

    theme_string <- mlply(cbind(names(theme_get()), theme_get()),parse_theme)
    output <- gsub("REPLACE_THEME",gsub("\\s+"," ",toString(theme_string)),output)

    writeLines(output,file)
  }
)
  
})



