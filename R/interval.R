#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##                   R script                                  ##
##                                                             ##
##                                             2018-03-29      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################


#' @title interval
#' @name interval
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage 
#' 
#' @param rbrick
#' @return
#' @export interval
#' @import
#' @import
#' 


interval <- function(rbrick){
  # case rbrick is a path of raster brick .tif
  if(typeof(rbrick) == "character"){
    rbrick <- raster::brick(rbrick, progress = "text")
  }
  # import rbrick
  if(typeof(rbrick) == "S4"){
i <- 1
      out <- parallel::mclapply(as.list(1:rbrick@nrows),function(i) {
        # extract a line from raster brick (cols x time)
        bcin <- raster::getValuesBlock(rbrickOut, row=(i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))
        
        bcin[is.na(bcin)] <- 0
        bcout <- NULL
        
        apply(bcin, 1, function(ts){
          true_ <- which(ts==1)
          if(length(true_)==0){
            counter[ts] <- -1
          }
          else { 
            counter[ts] <- (max(true_) - min(true_)) 
          }
        })
        
        
        dim(bcout) <- dim(bcin)[1]
        return(bcout)
        
      },  mc.cores = parallel::detectCores()-1)
    

    out1 <- unlist(out)
    
    if(for_time_step == FALSE){
      # redimension out to generate result restreBrick
      dim(out1) <- c(dim(rbrick)[2],dim(rbrick)[1])
      out1 <- t(out1)
      
      # generate result raster layer
      return(raster::setValues(rbrick[[1]], values = out1))
    }
    
    else {
      return(out1)
    }
    
  }
  
}
