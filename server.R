library('shiny')
library('ggplot2')
library('reshape2')

source('lib/grid/unit.R')
 
shinyServer(function(input, output) {

  plotWidth <- function() {
    input$plotWidth
  }
  plotHeight <- function() {
    input$plotHeight
  }

  #################### Functions to Define custom colors #####################
  divlist<-c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")
  quallist<-c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
  seqlist<-c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd",
  "PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")

  divnum <- rep(11, length(divlist))
  qualnum <- c( 8, 8, 12, 9, 8, 9, 8, 12)
  seqnum <- rep(9, length(seqlist))

  namelist<-c(divlist,quallist,seqlist)
  maxcolors <- c(divnum,qualnum,seqnum)
  catlist<-rep(c("div","qual","seq"),c(length(divlist),length(quallist),length(seqlist)))

  custom.pal.info<-data.frame(maxcolors=maxcolors,category=catlist,row.names=namelist)


  custom.pal<-function(n,name){
     if(!(name %in% namelist)){
     stop(paste(name,"is not a valid palette name for custom.pal\n"))
     }
     if(n<3){
     warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
     return(custom.pal(3,name))
     }
     if(n>maxcolors[which(name==namelist)]){
     warning(paste("n too large, allowed maximum for palette",name,"is",maxcolors[which(name==namelist)]),
  "\nReturning the palette you asked for with that many colors\n")
     return(custom.pal(maxcolors[which(name==namelist)],name))
     }

    c1 <- col2rgb(input$palColor1)
    c2 <- col2rgb(input$palColor2)
    c3 <- col2rgb(input$palColor3)
    c4 <- col2rgb(input$palColor4)
    c5 <- col2rgb(input$palColor5)
    c6 <- col2rgb(input$palColor6)
    c7 <- col2rgb(input$palColor7)
    c8 <- col2rgb(input$palColor8)
    c9 <- col2rgb(input$palColor9)



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
     if(n>maxcolors[which(name==namelist)]){
     warning(paste("n too large, allowed maximum for palette",name,"is",maxcolors[which(name==namelist)]),
  "\nDisplaying the palette you asked for with that many colors\n")
     return(display.custom.pal(maxcolors[which(name==namelist)],name))
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
  stop(paste(type, "is not a valid name for a color list\n"))
      }
      colorlist <- switch(type, div=divlist,
          qual=quallist, seq=seqlist,
          all=totallist)
      maxnum <- switch(type, div=divnum,
       qual=qualnum,
       seq=seqnum,
       all=totnum)
      if(!is.null(select)){colorlist <- colorlist[select]
   maxnum <- maxnum[select]
   if(any(is.na(colorlist)))
       stop(paste("Illegal value(s) of select: ",
          paste(select[is.na(colorlist)],
        collapse=" ")))
       }
      palattr <-  switch(type,  qual="qualitative",  div
         ="divergent", seq="sequential",
         all="qualitative+divergent+sequential")
      if(is.null(n))n <- maxnum
      if(length(n)==1)n <- rep(n, length(colorlist))
      if(exact.n){
  keep <- n<=maxnum
  colorlist <- colorlist[keep]
  n <- n[keep]
  maxnum <- maxnum[keep]
      }
      if (any(n < 3) | exact.n & any(n>maxnum)|
  length(n)!=length(colorlist)){
  warning("Illegal vector of color numbers")
  print(paste(n, collapse=" "))
      }
      n[n<3] <- 3
      n[n>maxnum] <- maxnum[n>maxnum]
      nr <- length(colorlist)
      nc <- max(n)
      ylim <- c(0,nr)
      oldpar <- par(mgp=c(2,0.25,0))
      on.exit(par(oldpar))
      plot(1,1,xlim=c(0,nc),ylim=ylim,type="n", axes=FALSE, bty="n",
   xlab="",ylab="")
      for(i in 1:nr)
      {nj <- n[i]
       if(colorlist[i]=="")next
       shadi <- custom.pal(nj, colorlist[i])
       rect(xleft=0:(nj-1), ybottom=i-1, xright=1:nj, ytop=i-0.2, col=shadi,
    border="light grey")
   }
      text(rep(-0.1,nr),(1:nr)-0.6, labels=colorlist, xpd=TRUE, adj=1)
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


 
  dataset <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
 
  output$plot <- renderPlot({

    # theme_new <- theme_set(theme_grey())
    theme_new <- theme(


#####################################################################################################################################################################################################################################################################

      rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
      text = element_text(family = "", face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
      axis.text = element_text(size = rel(0.8), colour = "grey50"),
      strip.text = element_text(size = rel(0.8)), axis.line = element_blank(),
      axis.text.x = element_text(vjust = 1),
      axis.text.y = element_text(hjust = 1),
      axis.ticks = element_line(colour = NULL),
      axis.title.x = element_text(),
      axis.title.y = element_text(angle = 90),
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks.margin = unit(0.1,     "cm"),
      legend.background = element_rect(colour = NA),
      legend.margin = unit(0.2, "cm"),
      legend.key = element_rect(fill = "grey95", colour = "white"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL, legend.key.width = NULL,
      legend.text = element_text(size = rel(0.8)),
      legend.text.align = NULL,
      legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
      legend.title.align = NULL,
      legend.position = "right",
      legend.direction = NULL,
      legend.justification = "center",
      legend.box = NULL,
      panel.background = element_rect(fill = "grey90", colour = NA),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_line(colour = "grey95", size = 0.25),
      panel.margin = unit(0.25, "lines"),
      panel.margin.x = NULL,
      panel.margin.y = NULL,
      strip.background = element_rect(fill = "grey80", colour = NA),
      strip.text.x = element_text(),
      strip.text.y = element_text(angle = -90),
      plot.background = element_rect(colour = "white"),
      plot.title = element_text(size = rel(1.2)),
      plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
      line = element_line(size=input$lineSize, color = input$lineColor, linetype = input$lineType, lineend = input$lineEnd),
      complete = TRUE
    )






###############################################################################################################################################################################################################
    
    #background, margins, title
    # plot.background = element_rect(fill=paste("#",input$plotBgColor,sep=""), color= "black"),
    # plot.margin = unit(c(2.8835,.582083,2.0748,.9378), "cm"),
    # plot.title = element_text(vjust = 7.9746, hjust = 0, size = 20, family = "ITC Franklin Gothic Std Demi Condensed"),

    # #plot area and grid
    # panel.border = element_blank(),
    # panel.background = element_rect(fill="transparent", size = 0),
    # panel.grid.minor = element_blank(),
    # panel.grid.major = element_line(size=.4703669, color = "#FFFFFF"),

    # #axes, ticks, and axis labels
    # axis.line = element_line(size=.4703669, color = "#FFFFFF"),
    # axis.ticks = element_blank(),
    # axis.ticks.length = unit(.20888, "cm"),
    # axis.text.x = element_text(size = 12, family = "ITC Franklin Gothic Std Demi"),
    # axis.text.y = element_text(size = 12, family = "ITC Franklin Gothic Std Demi"),
    # axis.title.x = element_text(vjust = -2.1804, size = 12, family = "Franklin Gothic Book"),
    # axis.title.y = element_text(vjust = -1.2633, angle = -90, size = 12, family = "Franklin Gothic Book"),

    # #legend
    # legend.background = element_rect(fill = "#E5E2E0", size = .4703669, color = "#C0C0BB"),
    # legend.text = element_text(size = 10, family = "Franklin Gothic Book"),
    # legend.title = element_blank(),
    # legend.key = element_rect(fill="#E5E2E0", colour= "#E5E2E0", size = 0),
    # legend.key.height = unit(1, "cm"),
    # legend.key.width = unit(.645,"cm"),
    # legend.margin = unit(1.5741,"cm")
  theme_set(theme_new)

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
    
    
  }, height = plotHeight, width = plotWidth)
  
})