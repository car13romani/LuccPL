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



# count number of classes in a classified raster


variance <- function(rbrick){
  # case rbrick is a path of raster brick .tif
  if(typeof(rbrick) == "character"){
    rbrick <- raster::brick(rbrick, progress = "text")
  }
  # import rbrick
  if(typeof(rbrick) == "S4"){
    
      #create a single layer raster
      cl <- parallel::makeCluster(parallel::detectCores())
      out <- parallel::parLapply(cl, as.list(1:rbrick@nrows),function(i) {
        # extract a line from raster brick (cols x time)
        
        bcin <- raster::getValuesBlock(rbrick, row=(i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))

        bcin[is.na(bcin)] <- 0
        bcout <- NULL
        bcout <- apply(bcin, 1, function(x){
          counter <- 1
          for (i in 2:length(x)) {
            if(x[i-1]!=x[i]){
              counter <- counter + 1
            }
          }
          return(counter)
        })
        dim(bcout) <- dim(bcin)[1]
        return(bcout)
        
      })
      

      parallel::stopCluster(cl)
      
      out1 <- unlist(out)
      
      dim(out1) <- c(dim(rbrick)[2],dim(rbrick)[1])
      out1 <- t(out1)
      
      # generate result raster layer
      return(raster::setValues(rbrick[[1]], values = out1))
      
    }

    

  
}