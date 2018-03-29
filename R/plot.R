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

plot_input <- function(X, date, pattern, color){
  # read RasterStackTS from file X
  
  myKey <- list(rectangles=list(col = color),text=list(lab=pattern),
                space='bottom',
                font=.3,
                width=.3,
                columns=4)
  
  rasterVis::levelplot(X, col.regions=color, colorkey=FALSE,names.attr = substr(date, 1, 4),key = myKey, scales=list(draw=FALSE), pretty=FALSE)
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

plot_output <- function(X, date){
  
  myKey2 <- list(rectangles=list(col = c("#000000", "#FFFFFF")),text=list(lab=c('True', 'False')),
                 space='bottom',
                 font=.3,
                 width=.3,
                 columns=2)
  
  rasterVis::levelplot(X, col.regions = c("#FFFFFF","#000000"), colorkey=FALSE, key = myKey2, names.attr = substr(date, 1, 4), scales=list(draw=FALSE))
}