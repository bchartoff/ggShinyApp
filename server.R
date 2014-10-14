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


tmp<-  reactive({
     theme_update(
#####################################################################################################################################################################################################################################################################
      line = element_line(size=input$line_size, colour = input$line_colour, linetype = as.integer(input$line_linetype), lineend = input$line_lineend),
      rect = element_rect(fill = input$rect_fill, colour = input$rect_colour, size = input$rect_size, linetype = as.integer(input$rect_linetype))
      )
      # text = element_text(family = "", face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
      # title = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # axis.text = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # strip.text = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      if(nchar(input$axis_line_linetype)>0 && nchar(input$axis_line_lineend)>0)
      {
        print("bar")
        theme_update(
          axis.line = element_line(size=input$axis_line_size, colour = input$axis_line_colour, linetype = as.integer(input$axis_line_linetype), lineend = input$axis_line_lineend)
          )
      }
      # axis.line.x = element_line(size=input$axis_line_x_size, colour = input$axis_line_x_colour, linetype = as.integer(input$axis_line_x_linetype), lineend = input$axis_line_x_lineend),
      # axis.line.y = element_line(size=input$axis_line_y_size, colour = input$axis_line_y_colour, linetype = as.integer(input$axis_line_y_linetype), lineend = input$axis_line_y_lineend)

      # axis.text.x = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # axis.text.y = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # axis.ticks = element_line(size=NULL, colour = NULL, linetype = NULL, lineend = NULL),
      # axis.title.x = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # axis.title.y = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # axis.ticks.length = unit(0.15, "cm"),
      # axis.ticks.margin = unit(0.1, "cm"),
      # legend.background = element_rect(fill = NULL, colour = NULL, size = NULL, linetype = NULL),
      # legend.margin = unit(0.2, "cm"),
      # legend.key = element_rect(fill = NULL, colour = NULL, size = NULL, linetype = NULL),
      # legend.key.size = unit(1.2, "lines"),
      # legend.key.height = NULL,
      # legend.key.width = NULL,
      # legend.text = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # legend.text.align = NULL,
      # legend.title = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # legend.title.align = NULL,
      # legend.position = "right",
      # legend.direction = NULL,
      # legend.justification = "center",
      # legend.box = NULL,
      # panel.background = element_rect(fill = NULL, colour = NULL, size = NULL, linetype = NULL),
      # panel.border = element_rect(fill = NA, colour = NULL, size = NULL, linetype = NULL),
      # panel.grid.major = element_line(size=NULL, colour = NULL, linetype = NULL, lineend = NULL),
      # panel.grid.minor = element_line(size=NULL, colour = NULL, linetype = NULL, lineend = NULL),
      # panel.margin = unit(0.25, "lines"),
      # panel.margin.x = NULL,
      # panel.margin.y = NULL,
      # strip.background = element_rect(fill = NULL, colour = NULL, size = NULL, linetype = NULL),
      # strip.text.x = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # strip.text.y = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # plot.background = element_rect(fill = NULL, colour = NULL, size = NULL, linetype = NULL),
      # plot.title = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
      # complete = TRUE
    
    })

  inTheme <- reactive({
        paste("theme",input$currentTheme,sep="_")
        # theme_set(inThemeFunc())
    })

  # tmp <- reactive({
  #     input$axis.lineColour
  #   })
  # theme_set(theme_bw())
  output$plot <- renderPlot({

    # print(input$axis_line_x_linetype)
          # tmp <-  get(paste("theme",inTheme,sep="_"),mode="function")
          # theme_set(tmp())
    theme_set(get(inTheme(),mode="function")())
    # tmp()
    theme_update(
#####################################################################################################################################################################################################################################################################
      line = element_line(size=input$line_size, colour = input$line_colour, linetype = as.integer(input$line_linetype), lineend = input$line_lineend),
      rect = element_rect(fill = input$rect_fill, colour = input$rect_colour, size = input$rect_size, linetype = as.integer(input$rect_linetype))
      )
      # text = element_text(family = "", face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
      # title = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # axis.text = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      # strip.text = element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL),
      if(nchar(input$axis_line_linetype)>0 && nchar(input$axis_line_lineend)>0)
      {
        print("bar")
        theme_update(
          axis.line = element_line(size=input$axis_line_size, colour = input$axis_line_colour, linetype = as.integer(input$axis_line_linetype), lineend = input$axis_line_lineend)
          )
      }
    # theme_set(get(inTheme,mode="function")())
    # theme_set(Atheme())

    # if (tmp == "grey"){
    #   theme_set(theme_grey())
    # }
    # else if (tmp == "default"){
    #   theme_set(theme_default())
    # }


    # theme_set(theme_default)
   

    # inTheme <- "default"



###############################################################################################################################################################################################################
    
    #background, margins, title
    # plot.background = element_rect(fill=paste("#",input$plotBgColour,sep=""), colour= "black"),
    # plot.margin = unit(c(2.8835,.582083,2.0748,.9378), "cm"),
    # plot.title = element_text(vjust = 7.9746, hjust = 0, size = 20, family = "ITC Franklin Gothic Std Demi Condensed"),

    # #plot area and grid
    # panel.border = element_blank(),
    # panel.background = element_rect(fill="transparent", size = 0),
    # panel.grid.minor = element_blank(),
    # panel.grid.major = element_line(size=.4703669, colour = "#FFFFFF"),

    # #axes, ticks, and axis labels
    # axis.line = element_line(size=.4703669, colour = "#FFFFFF"),
    # axis.ticks = element_blank(),
    # axis.ticks.length = unit(.20888, "cm"),
    # axis.text.x = element_text(size = 12, family = "ITC Franklin Gothic Std Demi"),
    # axis.text.y = element_text(size = 12, family = "ITC Franklin Gothic Std Demi"),
    # axis.title.x = element_text(vjust = -2.1804, size = 12, family = "Franklin Gothic Book"),
    # axis.title.y = element_text(vjust = -1.2633, angle = -90, size = 12, family = "Franklin Gothic Book"),

    # #legend
    # legend.background = element_rect(fill = "#E5E2E0", size = .4703669, colour = "#C0C0BB"),
    # legend.text = element_text(size = 10, family = "Franklin Gothic Book"),
    # legend.title = element_blank(),
    # legend.key = element_rect(fill="#E5E2E0", colour= "#E5E2E0", size = 0),
    # legend.key.height = unit(1, "cm"),
    # legend.key.width = unit(.645,"cm"),
    # legend.margin = unit(1.5741,"cm")
  # theme_set(theme_current)

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
      print("foo")
       print(theme_get()$axis.line)
 
    
  }, height = plotHeight, width = plotWidth)
  
})