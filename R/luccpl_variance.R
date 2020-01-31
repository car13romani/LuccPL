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



#' @title Variance
#' @name luccpl_variance
#' @aliases luccpl_variance
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description With a similar purpose to the count function, this, when applied to a three-dimensional matrix (x, y, t), 
#' generates a two-dimensional matrix (x, y) with the number of variations that occurred over time for each cell.
#'
#' @usage luccpl_variance(rbrick)
#'
#' @param rbrick S4. A spatiotemporal raster brick
#'
#' @keywords datasets
#' @return a single raster, the value of variance to each cell 
#' 
#' @importFrom raster brick getValuesBlock nlayers setValues hist
#' @importFrom parallel makeCluster detectCores parLapply stopCluster
#' @export
#'

luccpl_variance <- function(rbrick) {
    # case rbrick is a path of raster brick .tif
    if (typeof(rbrick) == "character") {
        rbrick <- raster::brick(rbrick, progress = "text")
    }
    # import rbrick
    if (typeof(rbrick) == "S4") {
        # create a single layer raster
        cl <- parallel::makeCluster(parallel::detectCores())
        out <- parallel::parLapply(cl, as.list(1:rbrick@nrows), function(i) {
            # extract a line from raster brick (cols x time)
            
            bcin <- raster::getValuesBlock(rbrick, row = (i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))
            
            bcin[is.na(bcin)] <- 0
            bcout <- NULL
            bcout <- apply(bcin, 1, function(x) {
                luccpl_counter <- 1
                for (i in 2:length(x)) {
                  if (x[i - 1] != x[i]) {
                    luccpl_counter <- luccpl_counter + 1
                  }
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
