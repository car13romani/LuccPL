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




#' @title Plot Input Data
#' @name luccpl_plot_input
#' @aliases luccpl_plot_input
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description This function creates an image in .jpeg format with all layers of the raster brick, 
#' as well as caption of the land use classes
#'
#' @usage luccpl_plot_input(X,date,pattern,color,pathMPinput,map_title,Width,Height)
#'
#' @param X S4. A spatiotemporal raster brick
#' @param date Integer. A vector that represents the timeline
#' @param pattern Character. A vector with the land use patterns of the input data
#' @param color Character. A vector with the colors to represent land use
#' @param pathMPinput Character. Path to save .jpeg of the raster brick
#' @param map_title Character. Title of multiplot
#' @param Width Integer. Dimension of multiplot
#' @param Height Integer. Dimension of multiplot
#'
#' @keywords datasets
#' @return Multiplot of input raster brick
#' 
#' @importFrom rasterVis levelplot
#' @export
#'

luccpl_plot_input <- function(X,date,pattern,color,pathMPinput,map_title,Width,Height){
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




#' @title Plot Output Data
#' @name luccpl_plot_output
#' @aliases luccpl_plot_output
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description This function creates an image in .jpeg format with all layers of the output raster brick
#'
#' @usage luccpl_plot_output(X,date,pathMPinput,map_title,Width,Height)
#'
#' @param X S4. A spatiotemporal raster brick, result of query
#' @param date Integer. A vector that represents the timeline
#' @param pathMPinput Character. Path to save .jpeg of the raster brick
#' @param map_title Character. Title of multiplot
#' @param Width Integer. Dimension of multiplot
#' @param Height Integer. Dimension of multiplot
#'
#' @keywords datasets
#' @return Multiplot of output raster brick
#' 
#' @importFrom rasterVis levelplot
#' @export
#'
luccpl_plot_output <- function(X,date,pathMPoutput,map_title,Width,Height){
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