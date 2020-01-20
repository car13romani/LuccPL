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



  count <- function(rbrick, for_time_step = FALSE, metadata=NULL, dates=NULL){
    # case rbrick is a path of raster brick .tif
    if(typeof(rbrick) == "character"){
      rbrick <- raster::brick(rbrick, progress = "text")
    }
    # import rbrick
    if(typeof(rbrick) == "S4"){
      #create a single layer raster
      if(for_time_step == FALSE){
        cl <- parallel::makeCluster(parallel::detectCores())
        out <- parallel::parLapply(cl, as.list(1:rbrick@nrows),function(i) {
          # extract a line from raster brick (cols x time)
          bcin <- raster::getValuesBlock(rbrick, row=(i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))

          bcin[is.na(bcin)] <- 0
          bcout <- NULL
          bcout <- apply(bcin, 1, sum)
          dim(bcout) <- dim(bcin)[1]
          return(bcout)
          
        })
        
      
        parallel::stopCluster(cl)
      
        
        out1 <- unlist(out)
        
        # redimension out to generate result restreBrick
        dim(out1) <- c(dim(rbrick)[2],dim(rbrick)[1])
        out1 <- t(out1)
          
        # generate result raster layer
        return(raster::setValues(rbrick[[1]], values = out1))
      }
    
      
      # create vector and graphic
      else{
        
        a <- hist(rbrick[[1]], maxpixels=1000000000000, plot=FALSE)
        
        # input raster
        if(max(a$breaks)>1.1){
          df_ <- data.frame("land_use" = metadata)
          for(i in 1:nlayers(rbrick)){
            a[[i]] <- raster::hist(rbrick[[i]], maxpixels=1000000000000, plot=FALSE)
            ci <- a[[i]]$counts
            rd <- round(a[[i]]$mids)
            co <- NULL

            for(j in 1:length(rd)){
              
              if(j == rd[j] || rd[j] != rd[j-1]){
                co[rd[j]] <- ci[j]
              }
            }
            df_[[i+1]] <- co
          }
          colnames(df_) <- c("land_use", dates)
          return(df_)
        }

        # output raster
        else{
          df_ <- data.frame("land_use"=c("False","True"))
          for(i in 1:nlayers(rbrick)){
            a[[i]] <- raster::hist(rbrick[[i]], maxpixels=1000000000000, plot=FALSE)
            ci <- a[[i]]$counts
            co <- NULL
            
            co <- c(ci[(which(a[[i]]$breaks==0))],ci[(which(a[[i]]$breaks==max(a[[i]]$breaks)))-1])
            df_[[i+1]] <- co
          }
          colnames(df_) <- c("land_use", dates)
          return(df_)
        }
          

      }

      
    }
    
  }