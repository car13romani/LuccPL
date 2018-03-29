


brickpath <- "/home/car/DADOS_MESTRADO/classified_MT_15/bricktest.tif"
path <- "/home/car/DADOS_MESTRADO/classified_MT_15/recorte2/"

# to before, year init = 0; to after, year final = sizets + 1
query_array <- array(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,  #relations before,after
                       0,0,3,3,3,3,3,3,3,3,3,3,3,3, #years init 4,5
                       4,4,17,17,17,17,17,17,17,17,17,17,17,17, #years final
                       3,1,2,4,5,6,7,8,9,10,11,12,13,14,  #patterns 1,3
                       1,0,1,1,1,1,1,1,1,1,1,1,1,0), c(14,5))  #union_op 1=or, 0=and


dates <- as.character(read.table(paste(path, "/dates.txt", sep=''))$V1)
col <- as.character(read.table(paste(path, "/colors.txt", sep=''))$V1)
patterns <- as.character(read.table(paste(path, "/patterns.txt", sep=''))$V1)





rbrick <- LuccPL::create_brick(path=path)


LuccPL::plot_input(rbrick,dates,patterns,colors)


export_brick(rbrick, brickpath)


rbrick <- ste.import_brick(brickpath)


system.time({ 
  rout <- event(rbrick, query_array) 
})

plot_result(rout,dates)







