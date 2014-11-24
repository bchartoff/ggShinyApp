library('ggplot2')
library('grid')
library('RColorBrewer')

########### Example plots #################

####Bar
##1 color
#print(ggplot(mtcars, aes(factor(cyl))) + geom_bar() + coord_cartesian(ylim = c(0, 100))+ggtitle("Title"))

##3 colors
#print(qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))+ggtitle("Title"))

##5 colors
#print(ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar() +ggtitle("Title") + coord_cartesian(ylim = c(0, 15000)))


####Scatter
##3 colors
#print(ggplot(mtcars, aes(wt, mpg))+geom_point(aes(colour = factor(cyl)))+ggtitle("Title"))

##9 colors
 # dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
 # d <- qplot(carat, price, data=dsamp, colour=clarity, size = 3)
 # print(d+ggtitle("Title"))

###Line
##3 colors
# mtcars.long <- melt(mtcars, id = "mpg", measure = c("disp", "hp", "wt"))
# print(ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line()+ggtitle("Title"))

###Facet Grid
# p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ggtitle("Title")
# print(p + facet_grid(vs ~ am, margins=TRUE))

###Histogram
##22 colors
# print(ggplot(mtcars) + scale_fill_manual(values = extendedPalette(22)) + geom_histogram(aes(factor(hp), fill=factor(hp))))


####################################


#resize window to 650 px width
quartz.options(width = REPLACE_PLOT_WIDTH, height = REPLACE_PLOT_HEIGHT, dpi = 72)

# For windows, uncomment below line (and comment out above line)
#windows.options(width = REPLACE_PLOT_WIDTH, height = REPLACE_PLOT_HEIGHT)

#################### redefine default ggplot theme ###################
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
	REPLACE_THEME
    )

#############################

#Redefine default discrete colors, up to 9 colors.
scale_colour_discrete <- function(...) scale_colour_custom(..., palette="Set1")
scale_fill_discrete <- function(...) scale_fill_custom(... , palette="Set1")

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

c1 <- col2rgb("REPLACE_COLOR_ONE")
c2 <- col2rgb("REPLACE_COLOR_TWO")
c3 <- col2rgb("REPLACE_COLOR_THREE")
c4 <- col2rgb("REPLACE_COLOR_FOUR")
c5 <- col2rgb("REPLACE_COLOR_FIVE")
c6 <- col2rgb("REPLACE_COLOR_SIX")
c7 <- col2rgb("REPLACE_COLOR_SEVEN")
c8 <- col2rgb("REPLACE_COLOR_EIGHT")
c9 <- col2rgb("REPLACE_COLOR_NINE")

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
