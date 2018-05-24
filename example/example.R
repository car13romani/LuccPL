


brickpath <- "/home/carlos/DADOS_MESTRADO/classified_MT_15/bricktest.tif"
path <- "/home/carlos/DADOS_MESTRADO/classified_MT_15/recorte2/"

# to before, year init = 0; to after, year final = sizets + 1
query_array <- array(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,  #relations before,after
                       0,0,3,3,3,3,3,3,3,3,3,3,3,3, #years init 4,5
                       4,4,17,17,17,17,17,17,17,17,17,17,17,17, #years final
                       3,1,2,4,5,6,7,8,9,10,11,12,13,14,  #patterns 1,3
                       1,0,1,1,1,1,1,1,1,1,1,1,1,0), c(14,5))  #union_op 1=or, 0=and




dates <- as.character(read.table(paste(path, "/dates.txt", sep=''))$V1)
colors <- as.character(read.table(paste(path, "/colors.txt", sep=''))$V1)
patterns <- as.character(read.table(paste(path, "/patterns.txt", sep=''))$V1)





mt15cl <- LuccPL::create_brick(path=path)


LuccPL::plot_input(mt15cl,dates,patterns,colors)

LuccPL::export_brick(mt15cl, brickpath)


mt15cl <- LuccPL::import_brick(brickpath)


system.time({ 
  mtout <- LuccPL::event(mt15cl, query_array) 
})

LuccPL::plot_output(mtout,dates)










mt5 <- (mtout[[5]])

fun <- function(x) { x[x>0] <- 1; return(x)}

f <- function(rast){
  if(rast > 0){
    rast <- 1
  }
  
}

raster::plot(mt5)
rrrr <- mtout[[7]]+mtout[[8]]+mtout[[9]]+mtout[[10]]+mtout[[11]]+mtout[[12]]+mtout[[13]]+mtout[[14]]+mtout[[15]]+mtout[[16]]
rout <- raster::calc(rrrr, fun)
resu <- mtout[[6]]-rout
raster::plot(resu)


query_array <- array(c(2,3,  #relations before,after
                       0,7, #years init 4,5
                       6,17, #years final
                       3,6,  #patterns 1,3
                       0,0), c(2,5))  #union_op 1=or, 0=and

query_array <- array(c(2,  #relations before,after
                       0, #years init 4,5
                       6, #years final
                       3,  #patterns 1,3
                       0), c(1,5))  #union_op 1=or, 0=and

query_array <- array(c(3,  #relations before,after
                       7, #years init 4,5
                       17, #years final
                       6,  #patterns 1,3
                       0), c(1,5))  #union_op 1=or, 0=and

ts <- c(3,3,4,3,3,3,6,6,4,4,6,6,4,6,6,6)
blockout <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

# call Fortran function
lucc_process <- function(SB, ST, NR, BI, BO, QA) {
  out <- .Fortran("lucc_process", as.integer(SB), as.integer(ST), as.integer(NR), as.integer(BI), as.integer(BO), as.integer(QA))
  return(out[[5]])
}
lucc_process(SB = 1, ST = 16, NR = 2, BI = ts, BO = blockout, QA = query_array)
