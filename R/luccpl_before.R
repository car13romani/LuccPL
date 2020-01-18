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


#' @title before
#' @name before
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
#' @export
#' @import
#' @import
#' 





before <- function(pattern_list1 = NULL, pattern_list2 = NULL, date1 = NULL, date2 = NULL, dates = NULL, metadata = NULL) {
  and_or <- NULL
  ensurer::ensure_that(pattern_list1, !is.null(pattern_list1),
                       err_desc = "pattern_list1, must be defined!")
  ensurer::ensure_that(date1, !is.null(date1),
                       err_desc = "date1, must be defined!")
  ensurer::ensure_that(dates, !is.null(dates),
                       err_desc = "dates, must be defined!")
  ensurer::ensure_that(metadata, !is.null(metadata),
                       err_desc = "metadata, must be defined!")
  
  
  
  if(length(pattern_list1) == 1) and_or <- 0
  if(length(pattern_list1) > 1) {
    
    # or for all connections except the last
    and_or[1:(length(pattern_list1))] <- 1  
    and_or[length(pattern_list1)] <- 0
  }
  
  date1[1:length(pattern_list1)] <- date1
  
  # establishes the relationship between land use type and digital number in the classifications
  pattern_number1 <- mdata(pattern_list1, metadata)
  
  # establishes the relationship between date and time-step of each classification
  time_step_number <- tdata(date1, dates)
  
  
  query_array <- NULL
  for(i in 1:length(pattern_list1)){
    # for each pattern of land use a column is created in the query_array
    query_array <- c(query_array, 1, 0, time_step_number[i], pattern_number1[i], 0, and_or[i])
  }
  
  return(query_array)
}


