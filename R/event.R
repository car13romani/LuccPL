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
  if(typeof(rbrick) == "character"){
    rbrick <- raster::brick(rbrick, progress = "text")
  }
  
  # import rbrick
  if(typeof(rbrick) == "S4"){
    
    sizeblock <- rbrick@ncols
    sizets <- dim(rbrick)[3]
    
    nrelations <- dim(query_array)[1]

    # call Fortran function
    lucc_process <- function(SB, ST, NR, BI, BO, QA) {
      out <- .Fortran("lucc_process", as.integer(SB), as.integer(ST), as.integer(NR), as.integer(BI), as.integer(BO), as.integer(QA))
      return(out[[5]])
    }
    
    # send blocks to Fortran function
    
    out <- parallel::mclapply(as.list(1:rbrick@nrows),function(i) {
      bcin <- raster::getValuesBlock(rbrick, row=(i), nrows = 1, col = 1, ncols = rbrick@ncols, lyrs = 1:(raster::nlayers(rbrick)))
      
      bcin[is.na(bcin)] <- 0 
      blockin <- t(bcin)
      
      blockout <- array(0, dim=dim(blockin))
      
      out_aux <- lucc_process(SB = sizeblock, ST = sizets, NR = nrelations, BI = blockin, BO = blockout, QA = query_array)
      
      return(out_aux)
      
    },  mc.cores = parallel::detectCores()-1)
  
    out1 <- unlist(out)
    
    
    
    # redimension out to generate result restreBrick
    dim(out1) <- c(sizets, (dim(rbrick)[1]*dim(rbrick)[2]))
    
    out1 <- t(out1)
    
    # generate result rasterBrick
    return(raster::setValues(rbrick, values = out1))
    
    
  }
  
}