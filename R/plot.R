#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##       R script                                  ##
##                                                             ##
##                                             2018-03-29      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Plot input
#' @name plot_input
#' @author Carlos Alexandre Romani
#'
#' @description Create a multiplot from input classified images
#' 
#' @usage plot_input(X, date, pattern, color)
#' 
#' @param X is a brick
#' @param date is a vector with a time serie of dates
#' @param pattern is a metadata of land use classes
#' @param color is the color scheme to represent each classe
#' 
#' @export
#' @import rasterVis
#' 

plot_input <- function(X,date,pattern,color,pathMPinput,map_title,Width,Height){
  # read RasterStackTS from file X
  
  myKey <- list(rectangles=list(col = color),text=list(lab=pattern),
                space='bottom',
                font=.6,
                width=.8,
                columns=4)
  jpeg(pathMPinput,width=Width, height=Height)
  print(rasterVis::levelplot(X,
                             main = list(map_title,side=1,line=0.5),
                             col.regions=color, 
                             maxpixels = 1e9, 
                             colorkey=FALSE,
                             names.attr = substr(date, 1, 4),
                             key = myKey, 
                             scales=list(draw=FALSE),
                             xscale.components = xscale.raster,
                             yscale.components = yscale.raster,
                             pretty=TRUE))
  dev.off()
}




#' @title Plot output
#' @name plot_output
#' @author Carlos Alexandre Romani
#'
#' @description Create a multiplot of output
#' 
#' @usage plot_output(X, date)
#' 
#' @param X is a brick
#' @param date is a vector with a time serie of dates
#' 
#' @export
#' @import rasterVis
#' 

plot_output <- function(X,date,pathMPoutput,map_title,Width,Height){
  color1 <- c("white","white","black","black")
  myKey <- list(rectangles=list(col = color1),text=list(lab=c("False","","True","")),
                 space='bottom',
                 font=.6,
                 width=.8,
                 columns=2)
  jpeg(pathMPoutput,width=Width, height=Height)
  print(rasterVis::levelplot(X,
                             main = list(map_title,side=1,line=0.5),
                             col.regions = color1, 
                             maxpixels = 1e9, 
                             colorkey=FALSE,
                             names.attr = substr(date, 1, 4),
                             key = myKey, 
                             scales=list(draw=FALSE),
                             xscale.components = xscale.raster,
                             yscale.components = yscale.raster,
                             pretty=TRUE))
  dev.off()
}