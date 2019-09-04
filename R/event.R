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


#' @title Lucc event
#' @name event
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage (rbrick, query_array)
#' 
#' @param rbrick input brick
#' @param query_array result of parse expression
#' 
#' @return Brick of result (bool)
#' @export
#' @import raster
#' @import parallel
#' 




# search LUCC event
event <- function(rbrick, query_array){
  # case rbrick is a path of raster brick .tif
  
  ensurer::ensure_that(rbrick, !is.null(rbrick),
                       err_desc = "Define a valid rbrick input.")
  ensurer::ensure_that(query_array, !is.null(query_array),
                       err_desc = "Define a valid query_array.")
  
  if(typeof(rbrick) == "character"){
    rbrick <- raster::brick(rbrick, progress = "text")
  }
  
  # import rbrick
  if(typeof(rbrick) == "S4"){
    
    #sizeblock <- n_rows*rbrick@ncols
    sizets <- dim(rbrick)[3]
    
    
    nblocks <- rbrick@nrows

    nrelations <- dim(query_array)[1]
    
    # call Fortran function
    lucc_process <- function(SB, ST, NR, BI, BO, QA) {
      out <- .Fortran("lucc_process", as.integer(SB), as.integer(ST), as.integer(NR), as.integer(BI), as.integer(BO), as.integer(QA))
      return(out[[5]])
    }
    #i=790
    # send blocks to Fortran function
    #dim(rbrick)
    
    cl <- parallel::makeCluster(parallel::detectCores())
    #cl <- parallel::clusterEvalQ(cl, {library(sf)})
    
    out <- parallel::parApply(cl, as.list(1:nblocks),function(i) {
      bcin <- raster::getValues(rbrick, row=i, nrows = 1)
      sizeblock <- dim(bcin)[1]
      
      bcin[is.na(bcin)] <- 0 
      blockin <- t(bcin)
      
      blockout <- array(0, dim=dim(blockin))
      
      out_aux <- lucc_process(SB = sizeblock, ST = sizets, NR = nrelations, BI = blockin, BO = blockout, QA = query_array)
      
      return(out_aux)
      
    })
    parallel::stopCluster(cl)
    
    out1 <- unlist(out)

    # redimension out to generate result restreBrick
    dim(out1) <- c(sizets, (dim(rbrick)[1]*dim(rbrick)[2]))
    
    out1 <- t(out1)
    
    # generate result rasterBrick
    return(raster::setValues(rbrick, values = out1))
    
    
  }
  
}
