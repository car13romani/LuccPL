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


#' @title query
#' @name query
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage 
#' 
#' @param FUN_list list of n functions to complex query with logical connectors
#' @param result_type bool_cube(default) as data input, bool_surface, count_surface(for same operators)
#' 
#' @return
#' @export
#' @import
#' @import
#' 
#' 
#' 



query <- function(input_dc, FUN_list, result_type = "bool_cube") {
  
  # 1. To the "bool_cube"
  if(pracma::strcmp(result_type,"bool_cube")){  
    # extract the logical connectors in the FUN_list
    if(length(which("and" == FUN_list)) != 0L) {
      FUN_list[which("and" == FUN_list)-1] <- 0
      FUN_list <- FUN_list[-which("and" == FUN_list)]
    } 
    if(length(which("or" == FUN_list)) != 0L) {
      FUN_list[which("or" == FUN_list)-1] <- 1
      FUN_list <- FUN_list[-which("or" == FUN_list)]
    } 
    FUN_list <- as.integer(FUN_list)
  
    #return(FUN_list)
    
    # format query_array
    
    dim(FUN_list) <- c(6,(length(FUN_list)/6))
    query_array <- t(FUN_list)
    #print(query_array)
    # fuction to process data cube and return query result
    
    return(query_array)
    
     ########### return(LuccPL::event(input_dc, query_array))

  }
  
  # 2. To the "bool_surface"
  if(pracma::strcmp(result_type,"bool_surface")){
    
  }
  
  
  
  
}


