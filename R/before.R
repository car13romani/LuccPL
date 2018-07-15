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


#' @title before
#' @name before
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage 
#' 
#' @param pattern_list1 pattern or listalist of land use
#' @param date1 
#' @param ... dates = list of time_step dates (definir o tipo de entrada); metadata = list of land use types according of raster digital number
#' 
#' @return
#' @export
#' @import
#' @import
#' 


#####  TRATAR O PROBLEMA DAS DATAS (LUBRIDATE)


before <- function(pattern_list1, date1,...) {
  and_or <- NULL
  # pattern or list of pattern to que query
  if(!is.character(pattern_list1)) stop("Pattern is not a string!")
  
  if(length(pattern_list1) == 1) and_or <- 0
  if(length(pattern_list1) > 1) {
    
    # or for all connections except the last
    and_or[1:(length(pattern_list1))] <- 1  
    and_or[length(pattern_list1)] <- 0
  }
  
  date1[1:length(pattern_list1)] <- date1

  # optional arguments (default must be declared in the project path)
  argnames <- names(list(...))
  if(!("dates" %in% argnames)){
    dates <- as.character(read.table(paste(path, "/dates.txt", sep=''))$V1)
  }
  if(!("metadata" %in% argnames)){
    metadata <- as.character(read.table(paste(path, "/metadata.txt", sep=''))$V1)
  }
  
  # establishes the relationship between the land use type and the digital number in the classifications
  pattern_number1 <- mdata(pattern_list1, metadata)
  
  #establishes the relationship between the date and the time step of each classification
  time_step_number <- tdata(date1, dates)
  
  
  query_array <- NULL
  for(i in 1:length(pattern_list1)){
    # for each pattern of land use a column is created in the query_array
    query_array <- c(query_array, 1, 0, time_step_number[i], pattern_number1[i], 0, and_or[i])
  }

  return(query_array)
}


