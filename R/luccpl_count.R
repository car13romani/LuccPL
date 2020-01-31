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



#' @title Count
#' @name luccpl_count
#' @aliases luccpl_count
#' @author Carlos Alexandre Romani
#' @docType data
#'
#' @description Queries performed with the event function return a three-dimensional matrix (x, y, t) the count
#'  function performs a vector summation in time or space of these results, allowing for more tangible analyzes. 
#'  The result can be a two-dimensional matrix (x, y) with the true count for each location or a table with the 
#'  true count for each year. If applied in the classifications rasterbrick, it returns a table with the number 
#'  of cells of each class and in each time instant.
#'
#' @usage luccpl_count(rbrick, for_time_step = FALSE, metadata = NULL, dates = NULL)
#'
#' @param rbrick S4. A spatiotemporal raster brick
#' @param for_time_step Boolean. FALSE returns a single raster with sum of timeserie values (use for query result only). 
#'                               TRUE returns a table with histogram for each land use. 
#' @param metadata Character. A vector with the land use patterns of the input data
#' @param dates Integer. A vector that represents the timeline
#'
#' @keywords datasets
#' @return 
#'  - for_time_step = FALSE, returns a single raster, the value of each cell being 
#'    the result of the sum of true values over time (only applicable to the query result raster_brick)
#'  - for_time_step = TRUE, returns a table with the number of cells of each class and in each instant of time.
#'    If applied to the rasterbrick result of the query, it returns a table with the true and false for each instant of time.
#' @importFrom raster brick getValuesBlock nlayers setValues hist
#' @importFrom parallel makeCluster detectCores parLapply stopCluster
#' @export
#'

luccpl_count <-
  function(rbrick,
           for_time_step = FALSE,
           metadata = NULL,
           dates = NULL) {
    # case rbrick is a path of raster brick .tif
    if (typeof(rbrick) == "character") {
      rbrick <- raster::brick(rbrick, progress = "text")
    }
    # import rbrick
    if (typeof(rbrick) == "S4") {
      #create a single layer raster
      if (for_time_step == FALSE) {
        cl <- parallel::makeCluster(parallel::detectCores())
        out <-
          parallel::parLapply(cl, as.list(1:rbrick@nrows), function(i) {
            # extract a line from raster brick (cols x time)
            bcin <-
              raster::getValuesBlock(
                rbrick,
                row = (i),
                nrows = 1,
                col = 1,
                ncols = rbrick@ncols,
                lyrs = 1:(raster::nlayers(rbrick))
              )
            
            bcin[is.na(bcin)] <- 0
            bcout <- NULL
            bcout <- apply(bcin, 1, sum)
            dim(bcout) <- dim(bcin)[1]
            return(bcout)
            
          })
        
        
        parallel::stopCluster(cl)
        
        
        out1 <- unlist(out)
        
        # redimension out to generate result restreBrick
        dim(out1) <- c(dim(rbrick)[2], dim(rbrick)[1])
        out1 <- t(out1)
        
        # generate result raster layer
        return(raster::setValues(rbrick[[1]], values = out1))
      }
      
      
      # create vector and graphic
      else{
        a <- hist(rbrick[[1]], maxpixels = 1000000000000, plot = FALSE)
        
        # input raster
        if (max(a$breaks) > 1.1) {
          df_ <- data.frame("land_use" = metadata)
          for (i in 1:nlayers(rbrick)) {
            a[[i]] <-
              raster::hist(rbrick[[i]], maxpixels = 1000000000000, plot = FALSE)
            ci <- a[[i]]$luccpl_counts
            rd <- round(a[[i]]$mids)
            co <- NULL
            
            for (j in 1:length(rd)) {
              if (j == rd[j] || rd[j] != rd[j - 1]) {
                co[rd[j]] <- ci[j]
              }
            }
            df_[[i + 1]] <- co
          }
          colnames(df_) <- c("land_use", dates)
          return(df_)
        }
        
        # output raster
        else{
          df_ <- data.frame("land_use" = c("False", "True"))
          for (i in 1:nlayers(rbrick)) {
            a[[i]] <-
              raster::hist(rbrick[[i]], maxpixels = 1000000000000, plot = FALSE)
            ci <- a[[i]]$luccpl_counts
            co <- NULL
            
            co <-
              c(ci[(which(a[[i]]$breaks == 0))], ci[(which(a[[i]]$breaks == max(a[[i]]$breaks))) -
                                                      1])
            df_[[i + 1]] <- co
          }
          colnames(df_) <- c("land_use", dates)
          return(df_)
        }
      }
    }
  }
