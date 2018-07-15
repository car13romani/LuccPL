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


#' @title holds
#' @name holds
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage 
#' 
#' @param pattern_list1 pattern or listalist of land use
#' @param date1 start date
#' @param date2 end date
#' @param ... dates = list of time_step dates (definir o tipo de entrada); metadata = list of land use types according of raster digital number
#' 
#' @return
#' @export holds
#' @import
#' @import
#' 

#####  TRATAR O PROBLEMA DAS DATAS (LUBRIDATE)


holds <- function(pattern_list1, date1, date2, ...) {
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
  date2[1:length(pattern_list1)] <- date2
  
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
  time_step1 <- tdata(date1, dates)
  time_step2 <- tdata(date2, dates)
  
  
  query_array <- NULL
  for(i in 1:length(pattern_list1)){
    # for each pattern of land use a column is created in the query_array
    query_array <- c(query_array, 4, time_step1[i], time_step2[i], pattern_number1[i], 0, and_or[i])
  }
  
  return(query_array)
}


