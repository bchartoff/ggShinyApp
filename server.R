library('shiny')
library('ggplot2')
library('reshape2')

source('lib/grid/unit.R')
source('lib/theme_default.R')
 
shinyServer(function(input, output) {

  plotWidth <- function() {
    input$plotWidth
  }
  plotHeight <- function() {
    input$plotHeight
  }
"#000000"
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

colour <- function(colour){
  if(colour == ""){
    val <- NULL
  }
  else{
    val <- colour
  }
  val
}

lineend <- function(lineend){
  if(lineend == ""){
    val <- NULL
  }
  else{
    val <- lineend
  }
  val
}

line.hide <- reactive({input$line_hide})
line.size <- reactive({size(input$line_size)})
line.colour <- reactive({colour(input$line_colour)})
line.linetype <- reactive({linetype(input$line_linetype)})
line.lineend <- reactive({lineend(input$line_lineend)})

axis.line.hide <- reactive({input$axis_line_hide})
axis.line.size <- reactive({size(input$axis_line_size)})
axis.line.colour <- reactive({colour(input$axis_line_colour)})
axis.line.linetype <- reactive({linetype(input$axis_line_linetype)})
axis.line.lineend <- reactive({lineend(input$axis_line_lineend)})

axis.line.x.hide <- reactive({input$axis_line_x_hide})
axis.line.x.size <- reactive({size(input$axis_line_x_size)})
axis.line.x.colour <- reactive({colour(input$axis_line_x_colour)})
axis.line.x.linetype <- reactive({linetype(input$axis_line_x_linetype)})
axis.line.x.lineend <- reactive({lineend(input$axis_line_x_lineend)})

axis.line.y.hide <- reactive({input$axis_line_y_hide})
axis.line.y.size <- reactive({size(input$axis_line_y_size)})
axis.line.y.colour <- reactive({colour(input$axis_line_y_colour)})
axis.line.y.linetype <- reactive({linetype(input$axis_line_y_linetype)})
axis.line.y.lineend <- reactive({lineend(input$axis_line_y_lineend)})

axis.ticks.hide <- reactive({input$axis_ticks_hide})
axis.ticks.size <- reactive({size(input$axis_ticks_size)})
axis.ticks.colour <- reactive({colour(input$axis_ticks_colour)})
axis.ticks.linetype <- reactive({linetype(input$axis_ticks_linetype)})
axis.ticks.lineend <- reactive({lineend(input$axis_ticks_lineend)})

axis.ticks.x.hide <- reactive({input$axis_ticks_x_hide})
axis.ticks.x.size <- reactive({size(input$axis_ticks_x_size)})
axis.ticks.x.colour <- reactive({colour(input$axis_ticks_x_colour)})
axis.ticks.x.linetype <- reactive({linetype(input$axis_ticks_x_linetype)})
axis.ticks.x.lineend <- reactive({lineend(input$axis_ticks_x_lineend)})

axis.ticks.y.hide <- reactive({input$axis_ticks_y_hide})
axis.ticks.y.size <- reactive({size(input$axis_ticks_y_size)})
axis.ticks.y.colour <- reactive({colour(input$axis_ticks_y_colour)})
axis.ticks.y.linetype <- reactive({linetype(input$axis_ticks_y_linetype)})
axis.ticks.y.lineend <- reactive({lineend(input$axis_ticks_y_lineend)})

axis.ticks.length <- reactive({size(input$axis_ticks_length)})
axis.ticks.margin <- reactive({size(input$axis_ticks_margin)})

panel.background.fill <- reactive({colour(input$panel_background_fill)})
panel.background.colour <- reactive({colour(input$panel_background_colour)})
panel.background.size <- reactive({size(input$panel_background_size)})
panel.background.linetype <- reactive({linetype(input$panel_background_linetype)})

panel.border.fill <- function(){NA}
panel.border.colour <- reactive({colour(input$panel_border_colour)})
panel.border.size <- reactive({size(input$panel_border_size)})
panel.border.linetype <- reactive({linetype(input$panel_border_linetype)})

panel.grid.hide <- reactive({input$panel_grid_hide})
panel.grid.size <- reactive({size(input$panel_grid_size)})
panel.grid.colour <- reactive({colour(input$panel_grid_colour)})
panel.grid.linetype <- reactive({linetype(input$panel_grid_linetype)})
panel.grid.lineend <- reactive({lineend(input$panel_grid_lineend)})

panel.grid.major.hide <- reactive({input$panel_grid_major_hide})
panel.grid.major.size <- reactive({size(input$panel_grid_major_size)})
panel.grid.major.colour <- reactive({colour(input$panel_grid_major_colour)})
panel.grid.major.linetype <- reactive({linetype(input$panel_grid_major_linetype)})
panel.grid.major.lineend <- reactive({lineend(input$panel_grid_major_lineend)})

panel.grid.major.x.hide <- reactive({input$panel_grid_major_x_hide})
panel.grid.major.x.size <- reactive({size(input$panel_grid_major_x_size)})
panel.grid.major.x.colour <- reactive({colour(input$panel_grid_major_x_colour)})
panel.grid.major.x.linetype <- reactive({linetype(input$panel_grid_major_x_linetype)})
panel.grid.major.x.lineend <- reactive({lineend(input$panel_grid_major_x_lineend)})

panel.grid.major.y.hide <- reactive({input$panel_grid_major_y_hide})
panel.grid.major.y.size <- reactive({size(input$panel_grid_major_y_size)})
panel.grid.major.y.colour <- reactive({colour(input$panel_grid_major_y_colour)})
panel.grid.major.y.linetype <- reactive({linetype(input$panel_grid_major_y_linetype)})
panel.grid.major.y.lineend <- reactive({lineend(input$panel_grid_major_y_lineend)})

panel.grid.minor.hide <- reactive({input$panel_grid_minor_hide})
panel.grid.minor.size <- reactive({size(input$panel_grid_minor_size)})
panel.grid.minor.colour <- reactive({colour(input$panel_grid_minor_colour)})
panel.grid.minor.linetype <- reactive({linetype(input$panel_grid_minor_linetype)})
panel.grid.minor.lineend <- reactive({lineend(input$panel_grid_minor_lineend)})

panel.grid.minor.x.hide <- reactive({input$panel_grid_minor_x_hide})
panel.grid.minor.x.size <- reactive({size(input$panel_grid_minor_x_size)})
panel.grid.minor.x.colour <- reactive({colour(input$panel_grid_minor_x_colour)})
panel.grid.minor.x.linetype <- reactive({linetype(input$panel_grid_minor_x_linetype)})
panel.grid.minor.x.lineend <- reactive({lineend(input$panel_grid_minor_x_lineend)})

panel.grid.minor.y.hide <- reactive({input$panel_grid_minor_y_hide})
panel.grid.minor.y.size <- reactive({size(input$panel_grid_minor_y_size)})
panel.grid.minor.y.colour <- reactive({colour(input$panel_grid_minor_y_colour)})
panel.grid.minor.y.linetype <- reactive({linetype(input$panel_grid_minor_y_linetype)})
panel.grid.minor.y.lineend <- reactive({lineend(input$panel_grid_minor_y_lineend)})

rect.fill <- reactive({colour(input$rect_fill)})
rect.colour <- reactive({colour(input$rect_colour)})
rect.size <- reactive({size(input$rect_size)})
rect.linetype <- reactive({linetype(input$rect_linetype)})


  output$plot <- renderPlot({
    theme_set(theme_default())

    theme_update(
      line = element_line(size=line.size(),colour=line.colour(),linetype=line.linetype(),lineend=line.lineend()),
      # rect = element_rect(fill = rect.fill(), colour = input$rect_colour, size = input$rect_size, linetype = as.integer(input$rect_linetype)),

     
      axis.line = element_line(size=axis.line.size(),linetype=axis.line.linetype(),colour=axis.line.colour(), lineend=axis.line.lineend()),
      axis.line.x = element_line(size=axis.line.x.size(), colour=axis.line.x.colour(), linetype=axis.line.x.linetype(), lineend=axis.line.x.lineend()),
      axis.line.y = element_line(size=axis.line.y.size(),colour=axis.line.y.colour(),linetype=axis.line.y.linetype(),lineend=axis.line.y.lineend()),

      axis.ticks = element_line(size=axis.ticks.size(),linetype=axis.ticks.linetype(),colour=axis.ticks.colour(), lineend=axis.ticks.lineend()),
      axis.ticks.x = element_line(size=axis.ticks.x.size(), colour=axis.ticks.x.colour(), linetype=axis.ticks.x.linetype(), lineend=axis.ticks.x.lineend()),
      axis.ticks.y = element_line(size=axis.ticks.y.size(),colour=axis.ticks.y.colour(),linetype=axis.ticks.y.linetype(),lineend=axis.ticks.y.lineend()),
      axis.ticks.length = unit(axis.ticks.length(),"points"),
      axis.ticks.margin = unit(axis.ticks.margin(),"points"),

      panel.background = element_rect(fill = panel.background.fill(), colour = panel.background.colour(), size = panel.background.size(), linetype = panel.background.linetype()),
      panel.border = element_rect(fill = panel.border.fill(), colour = panel.border.colour(), size = panel.border.size(), linetype = panel.border.linetype()),
      panel.grid = element_line(size=panel.grid.size(),linetype=panel.grid.linetype(),colour=panel.grid.colour(), lineend=panel.grid.lineend()),
      panel.grid.major = element_line(size=panel.grid.major.size(),linetype=panel.grid.major.linetype(),colour=panel.grid.major.colour(), lineend=panel.grid.major.lineend()),
      panel.grid.major.x = element_line(size=panel.grid.major.x.size(),linetype=panel.grid.major.x.linetype(),colour=panel.grid.major.x.colour(), lineend=panel.grid.major.x.lineend()),
      panel.grid.major.y = element_line(size=panel.grid.major.y.size(),linetype=panel.grid.major.y.linetype(),colour=panel.grid.major.y.colour(), lineend=panel.grid.major.y.lineend()),
      panel.grid.minor = element_line(size=panel.grid.minor.size(),linetype=panel.grid.minor.linetype(),colour=panel.grid.minor.colour(), lineend=panel.grid.minor.lineend()),
      panel.grid.minor.x = element_line(size=panel.grid.minor.x.size(),linetype=panel.grid.minor.x.linetype(),colour=panel.grid.minor.x.colour(), lineend=panel.grid.minor.x.lineend()),
      panel.grid.minor.y = element_line(size=panel.grid.minor.y.size(),linetype=panel.grid.minor.y.linetype(),colour=panel.grid.minor.y.colour(), lineend=panel.grid.minor.y.lineend())

    )

    if(line.hide()){theme_update(line = element_blank())}
    if(axis.line.hide()){theme_update(axis.line = element_blank())}
    if(axis.line.x.hide()){theme_update(axis.line.x = element_blank())}
    if(axis.line.y.hide()){theme_update(axis.line.y = element_blank())}
    if(axis.ticks.hide()){theme_update(axis.ticks = element_blank())}
    if(axis.ticks.x.hide()){theme_update(axis.ticks.x = element_blank())}
    if(axis.ticks.y.hide()){theme_update(axis.ticks.y = element_blank())}
    if(panel.grid.hide()){theme_update(panel.grid = element_blank())}
    if(panel.grid.major.hide()){theme_update(panel.grid.major = element_blank())}
    if(panel.grid.major.x.hide()){theme_update(panel.grid.major.x = element_blank())}
    if(panel.grid.major.y.hide()){theme_update(panel.grid.major.y = element_blank())}
    if(panel.grid.minor.hide()){theme_update(panel.grid.minor = element_blank())}
    if(panel.grid.minor.x.hide()){theme_update(panel.grid.minor.x = element_blank())}
    if(panel.grid.minor.y.hide()){theme_update(panel.grid.minor.y = element_blank())}


  scale_colour_discrete <- function(...) scale_colour_custom(..., palette="Set1")
  scale_fill_discrete <- function(...) scale_fill_custom(... , palette="Set1")


    if (input$sampleChart == 1){
        print(ggplot(mtcars, aes(factor(cyl))) + geom_bar() + coord_cartesian(ylim = c(0, 25))+ggtitle("Title"))
    }
    else if (input$sampleChart == 2){
        print(qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl)) + coord_cartesian(ylim = c(0, 15)) + ggtitle("Title"))
    }
    else if (input$sampleChart == 3){
        print(ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar() +ggtitle("Title") + coord_cartesian(ylim = c(0, 15000)))
    }
    else if (input$sampleChart == 4){
        print(ggplot(mtcars, aes(wt, mpg))+geom_point(aes(colour = factor(cyl)))+ggtitle("Title"))
    }
    else if (input$sampleChart == 5){
        dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
        d <- qplot(carat, price, data=dsamp, colour=clarity, size = 3)
        print(d+ggtitle("Title"))
    }
    else if (input$sampleChart == 6){
        mtcars.long <- melt(mtcars, id = "mpg", measure = c("disp", "hp", "wt"))
        print(ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line()+ggtitle("Title"))
    }
    else if (input$sampleChart == 7){
      p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ggtitle("Title")
      print(p + facet_grid(vs ~ am, margins=TRUE))
    }
 
    
  }, height = plotHeight, width = plotWidth)
  
})