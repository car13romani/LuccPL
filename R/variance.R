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


#' @title variance
#' @name variance
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


variance <- function(rbrick, for_time_step = FALSE){
  # case rbrick is a path of raster brick .tif
  if(typeof(rbrick) == "character"){
    rbrick <- raster::brick(rbrick, progress = "text")
  }
  # import rbrick
  if(typeof(rbrick) == "S4"){
    
    if(for_time_step == FALSE){
      #create a single layer raster
      out <- parallel::mclapply(as.list(1:rbrick@nrows),function(i) {
        # extract a line from raster brick (cols x time)
        
        i <- 1
        
        bcin <- raster::getValuesBlock(rbrick, row=(i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))
        
        bcin[is.na(bcin)] <- 0
        bcout <- NULL
        bcout <- apply(bcin, 1, function(x){
          counter <- 0
          for (i in 1:length(x)-1) {
            if(x[i]!=x[i+1]){
              counter <- counter + 1
            }
            
          }
          
          return(counter)
          
        })
        dim(bcout) <- dim(bcin)[1]
        return(bcout)
        
      },  mc.cores = parallel::detectCores()-1)
      
      dim(out1) <- c(dim(rbrick)[2],dim(rbrick)[1])
      out1 <- t(out1)
      
      # generate result raster layer
      return(raster::setValues(rbrick[[1]], values = out1))
      
    }
    
    else {
      # create vector and graphic
      out <- parallel::mclapply(as.list(1:rbrick@nrows),function(i) {
        # extract a line from raster brick (cols x time)
        bcin <- raster::getValuesBlock(rbrick, row=(i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))
        
        bcin[is.na(bcin)] <- 0
        bcout <- NULL
        bcout <- apply(bcin, 2, sum)
        dim(bcout) <- dim(bcin)[2]
        return(bcout)
        
      },  mc.cores = parallel::detectCores()-1)
    }
    
    
    out1 <- unlist(out)

  }
  
}