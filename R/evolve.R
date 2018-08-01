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


#' @title evolve
#' @name evolve
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage 
#' 
#' @param pattern_list1 list of land use types
#' @param pattern_list2 list of land use types
#' @param date1 start date
#' @param date2 end date
#' @param dates list of dates for each time-step
#' @param metadata list of land use types according of raster digital number
#' 
#' @return
#' @export evolve
#' @import
#' @import
#' 

 

# CASO ESPECIAL NxM
evolve <- function(pattern_list1, pattern_list2, date1, date2, dates, metadata) {  # date2 optional (ARRUMAR ISSO)
  and_or <- NULL
  
  if(length(pattern_list1) == 1 && length(pattern_list2) == 1) and_or <- 0
  else {
   # or for all connections except the last
    and_or[1:((length(pattern_list1))*(length(pattern_list2))-1)] <- 1  
    and_or[((length(pattern_list1))*(length(pattern_list2)))] <- 0
  }
  
  
  date1[1:length(pattern_list1)] <- date1
  date2[1:length(pattern_list2)] <- date2
  
  # establishes the relationship between land use type and digital number in the classifications
  pattern_number1 <- mdata(pattern_list1, metadata)
  pattern_number2 <- mdata(pattern_list2, metadata)
  
  # establishes the relationship between date and time-step of each classification
  time_step1 <- tdata(date1, dates)
  time_step2 <- tdata(date2, dates)
  
  
  query_array <- NULL
  for(i in 1:length(pattern_list1)){
    for(j in 1:length(pattern_list2)){
      # for each pattern of land use a column is created in the query_array
      query_array <- c(query_array, 7, time_step1[i], time_step2[j], pattern_number1[i], pattern_number2[j], and_or[i+j])
    }
  }
  
  return(query_array)
}


