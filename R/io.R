#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with relationships of ste query language         ##
##                                                             ##
##                                             2018-03-29      ##
##                                                             ##
##    Work based on lucc from Adeline Marinho                  ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Brick creation
#' @name create_brick
#' @author Carlos Alexandre Romani
#'
#' @description Return a brick of spatiotemporal data from classified images
#' 
#' @usage create_brick(path)
#' 
#' @param path where is the classified images in .tif format
#' 
#' @return Brick of spatiotemporal data
#' @export
#' @import raster
#' 

create_brick <- function(path){
  # list of raster files
  lst <- list.files(path=path,pattern='.tif$',full.names=TRUE)
  # stack creation
  rstack <- raster::stack(lst)
  # brick creation
  print('Making brick')
  return(raster::brick(rstack,  progress = "text", datatype='INT4S'))
  gc()
}



#################################################################

#' @title Import Brick
#' @name import_brick
#' @author Carlos Alexandre Romani
#'
#' @description Import a brick of spatiotemporal data
#' 
#' @usage import_brick(filename)
#' 
#' @param filename of a exported brick
#' 
#' @return Brick of spatiotemporal data
#' @export
#'
#' 
#'

import_brick <- function(filename){
  return(raster::brick(filename))
}


#################################################################

#' @title Export Brick
#' @name export_brick
#' @author Carlos Alexandre Romani
#'
#' @description Write a brick in a file
#' 
#' @usage export_brick(raster, filename)
#' 
#' @param path where is the classified images in .tif format
#' 
#' @export
#'
export_brick <- function(raster, filename){
  print('Export raster')
  raster::writeRaster(raster, filename = filename, datatype='INT4S', overwrite=TRUE, progress = "text")
  gc()
}