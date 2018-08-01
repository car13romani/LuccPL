install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')



path <- "/home/carlos/DADOS_MESTRADO/classified_MT_15/recorte3"

brickpath <- paste0(path, "bricktest.tif")
dates <- as.character(read.table(paste0(path, "/dates.txt"))$V1)
colors <- as.character(read.table(paste0(path, "/colors.txt"))$V1)
metadata <- as.character(read.table(paste0(path, "/metadata.txt"))$V1)


mt15cl <- LuccPL::import_brick(brickpath)

plot_input()
plot_input(rteste[[1]], dates, metadata, colors)



shinyApp(ui = ui, server = server)


# path
#   |_ metadata.txt
#   |_ dates.txt
#   |_ colors.txt





library(raster)
library(rasterVis)


jpeg(file = paste0(path,"/jpeg/plotOut1.jpeg"), bg = "transparent", height=nrow(rteste[[1]]), width=ncol(rteste[[1]]))
rasterVis::levelplot(rteste[[1]], col.regions=colors,  contour=F, margin=F, scales = list(draw=FALSE), colorkey=NULL,
                     par.settings = list(axis.line = list(line=0), mar=c(0,0,0,0), omi=c(0,0,0,0), 
                                          xaxt='n', yaxt='n', bg='transparent'))
dev.off()










rasterVis::levelplot(rteste[[1]], col.regions=colors)

?levelplot
writeJPEG(image = rasterVis::levelplot(rteste[[1]], col.regions=colors), target = "testye111.jpeg", color.space = colors)

magick::image_write(rteste[[1]], path = "tiger.jpeg", format = "jpeg")

library(magick)
plot(rteste[[1]])
?raster::plot()

mt15cl <- LuccPL::create_brick(path=path)

LuccPL::plot_input(mt15cl,dates,patterns,colors)

LuccPL::export_brick(mt15cl, brickpath)



query_array <- LuccPL::before("Pasture_2",2008)


system.time({ 
  mtout <- LuccPL::event(mt15cl, query_array) 
})

LuccPL::plot_output(mt_out,dates)

rm(mt_out)
# example of query
mt_out <- query(mt15cl, FUN_list = c(
  before("Forest",2008),
  "and",
  after(c("Pasture_1","Pasture_2"), 2008) 
))

eval(parse(text="5+3"))

query1 <- c(
  LuccPL::meets("Forest",2008),
  "and",
  LuccPL::metby(c("Pasture_1","Pasture_2"), 2008) 
)


query(mt15cl, FUN_list = c(
  meets("Forest",2008),
  "and",
  metby(c("Pasture_1","Pasture_2"), 2008) 
))


query(mt15cl, FUN_list = c(
  before("Forest",2008),
  "or",
  meets("Forest",2008),
  "and",
  after(c("Pasture_1","Pasture_2"), 2008),
  "or",
  metby(c("Pasture_1","Pasture_2"), 2008) 
))

query(mt15cl, FUN_list = c(
  holds(c("Forest","Savanna"), 2005, 2008)
))


query(mt15cl, FUN_list = c(
  convert(c("Forest","Savanna"), c("Pasture_1","Pasture_2"), 2008)
))


query(mt15cl, FUN_list = c(
  evolve(c("Forest","Savanna"), c("Pasture_1","Pasture_2"), 2008)
))



FUN_list


pattern_list1 <- c("Pasture_1","Pasture_2")

date1 <- 2008




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

# to before, year init = 0; to after, year final = sizets + 1
query_array <- array(c(2,4,  #relations before,after
                       0,8, #years init 4,5
                       6,13, #years final
                       3,6,  #patterns 1,3
                       0,0,
                       1,0), c(2,6))  #union_op 1=or, 0=and

query_array <- array(c(2,  #relations before,after
                       0, #years init 4,5
                       6, #years final
                       3,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

query_array <- array(c(3,  #relations before,after
                       7, #years init 4,5
                       17, #years final
                       6,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

query_array <- array(c(4,  #relations before,after
                       8, #years init 4,5
                       12, #years final
                       6,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

query_array <- array(c(5,  #relations before,after
                       7, #years init 4,5
                       7, #years final
                       4,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

query_array <- array(c(6,  #relations before,after
                       6, #years init 4,5
                       0, #years final
                       3,  #patterns 1,3
                       5,
                       0), c(1,6))  #union_op 1=or, 0=and

query_array <- array(c(7,  #relations before,after
                       4, #years init 4,5
                       5, #years final
                       3,  #patterns 1,3
                       6,
                       0), c(1,6))  #union_op 1=or, 0=and


query_array <- array(c(6,7,  #relations before,after
                       6,4, #years init 4,5
                       0,5, #years final
                       3,3,  #patterns 1,3
                       5,6,
                       1,0), c(2,6))  #union_op 1=or, 0=and


ts <- c(3,3,4,3,3,3,5,6,6,6,6,6,4,6,6,6)
blockout <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)


# call Fortran function
lucc_process <- function(SB, ST, NR, BI, BO, QA) {
  out <- .Fortran("lucc_process", as.integer(SB), as.integer(ST), as.integer(NR), as.integer(BI), as.integer(BO), as.integer(QA))
  #return(out[[5]])
  return(out)
}


lucc_process(SB = 1, ST = 16, NR = dim(query_array)[1], BI = ts, BO = blockout, QA = query_array)

