#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##       R script                                              ##
##                                                             ##
##                                             2020-01-10      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################



#' @title True
#' @name luccpl_true
#' @aliases luccpl_true
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description 
#'
#' @usage luccpl_true(rbrick)
#'
#' @param rbrick S4. A spatiotemporal raster brick, output from luccpl_event.
#'
#' @keywords datasets
#' @return A single layer raster with true in locations with true for locations with at least one true on the timeline
#' 

#' @importFrom raster brick getValuesBlock nlayers setValues
#' @importFrom parallel makeCluster detectCores parLapply stopCluster
#' @export
#'

luccpl_true <- function(rbrick) {
    # case rbrick is a path of raster brick .tif
    if (typeof(rbrick) == "character") {
        rbrick <- raster::brick(rbrick, progress = "text")
    }
    # import rbrick
    if (typeof(rbrick) == "S4") {
        
        cl <- parallel::makeCluster(parallel::detectCores())
        out <- parallel::parLapply(cl, as.list(1:rbrick@nrows), function(i) {
            # extract a line from raster brick (cols x time)
            
            bcin <- raster::getValuesBlock(rbrick, row = (i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))
            
            bcin[is.na(bcin)] <- 0
            bcout <- NULL
            bcout <- apply(bcin, 1, function(x) {
                luccpl_counter <- 0
                
                if (sum(x == 1) >= 1) {
                  luccpl_counter <- 1
                }
                
                return(luccpl_counter)
                
            })
            dim(bcout) <- dim(bcin)[1]
            return(bcout)
            
        })
        parallel::stopCluster(cl)
        
        out1 <- unlist(out)
        
        dim(out1) <- c(dim(rbrick)[2], dim(rbrick)[1])
        out1 <- t(out1)
        
        # generate result raster layer
        return(raster::setValues(rbrick[[1]], values = out1))
        
    }
    
    
}
