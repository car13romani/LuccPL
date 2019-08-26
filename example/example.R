install.packages('package_name', dependencies=TRUE, repos='http://cran.rstudio.com/')

ras

path <- "/home/car/DADOS_MESTRADO/classified_MT_15/recorte3"

co <- paste0(path, "/rasterCount.tif")
brickpath <- paste0(path, "/stInput.tif")
brickpathOut <- paste0(path, "/stOutput.tif")

dates <- as.character(read.table(paste0(path, "/dates.txt"))$V1)
colors <- as.character(read.table(paste0(path, "/colors.txt"))$V1)
metadata <- as.character(read.table(paste0(path, "/metadata.txt"))$V1)

library(raster)
cou <- raster(co)
rbrick <- raster::brick(brickpath)
rbrickOut <- raster::brick(brickpathOut)
plot(cou)
plot_input()
plot_input(rbrick[[1]], dates, metadata, colors)

library(parallel)
library(raster)
hist(mt15cl, breaks = 30)

str(mt15cl[[1]])

shinyApp(ui = ui, server = server)

ss <- NULL



aa <- count(brickRasterOutput,ss)

dim(aa) <- c(dim(brickRasterOutput)[2],dim(brickRasterOutput)[1])

aa <- t(aa)

rlout <- raster::setValues(brickRasterOutput[[1]], values = aa)

plot(rlout)

plot(rbrickOut[[5]])
rbrick





# path
#   |_ metadata.txt
#   |_ dates.txt
#   |_ colors.txt

library(ggplot2)

ggplot(as.data.frame(mt15cl[[1]]), aes(x = breaks, y = counts, fill =counts)) + ## Note the new aes fill here
  geom_bar(stat = "identity",alpha = 0.8)+
  xlab("Pearson correlation")+ ylab("Frequency")+
  scale_x_continuous(breaks = seq(-1,1,0.25),
                     labels = seq(-1,1,0.25))+
  scale_fill_gradient(low="blue", high="red")  













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

#meets
query_array <- array(c(2,  #relations before,after
                       0, #years init 4,5
                       6, #years final
                       3,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

#metby
query_array <- array(c(3,  #relations before,after
                       7, #years init 4,5
                       17, #years final
                       6,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

#holds
query_array <- array(c(4,  #relations before,after
                       6, #years init 4,5
                       12, #years final
                       6,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

#recur
query_array <- array(c(5,  #relations before,after
                       1, #years init 4,5
                       6, #years final
                       3,  #patterns 1,3
                       0,
                       0), c(1,6))  #union_op 1=or, 0=and

#convert
query_array <- array(c(6,  #relations before,after
                       6, #years init 4,5
                       0, #years final
                       3,  #patterns 1,3
                       5,
                       0), c(1,6))  #union_op 1=or, 0=and

#evolve
query_array <- array(c(7,  #relations before,after
                       1, #years init 4,5
                       6, #years final
                       3,  #patterns 1,3
                       6,
                       0), c(1,6))  #union_op 1=or, 0=and


query_array <- array(c(1,3,3,  #relations before,after
                       0,6,6, #years init 4,5
                       6,17,17, #years final
                       3,6,5,  #patterns 1,3
                       0,0,0,
                       0,1,0), c(3,6))  #union_op 1=or, 0=and



query_array <- array(c(1,3,3,3,3,3,3,3,3,3,  #relations before,after
                       0,6,6,6,6,6,6,6,6,6, #years init 4,5
                       6,17,17,17,17,17,17,17,17,17, #years final
                       3,6,5,4,7,8,9,10,11,12,  #patterns 1,3
                       0,0,0,0,0,0,0,0,0,0,
                       0,1,1,1,1,1,1,1,1,0), c(10,6))  #union_op 1=or, 0=and

query_array <- array(c(7,7,7,7,7,7,7,7,7,7,7,7,  #relations before,after
                       7,7,7,7,7,7,7,7,7,7,7,7, #years init 4,5
                       7,7,7,7,7,7,7,7,7,7,7,7, #years final
                       3,3,3,3,3,3,3,3,3,3,3,3,  #patterns 1,3
                       2,4,5,6,7,8,9,14,13,12,11,10,
                       1,1,1,1,1,1,1,1,1,1,1,0), c(12,6))  #union_op 1=or, 0=and

query_array <- array(c(7,7,7,7,7,7,7,7,7,7,7,7,  #relations before,after
                       1,1,1,1,1,1,1,1,1,1,1,1, #years init 4,5
                       7,7,7,7,7,7,7,7,7,7,7,7, #years final
                       3,3,3,3,3,3,3,3,3,3,3,3,  #patterns 1,3
                       2,4,5,6,7,8,9,14,13,12,11,10,
                       1,1,1,1,1,1,1,1,1,1,1,0), c(12,6))  #union_op 1=or, 0=and


ts <- array(c(3,3,1,3,6,3,3,5,7,3,3,3,3,3,3,3,
              3,3,6,6,6,3,3,3,3,3,3,3,3,3,3,3), c(2,16))
ts <- c(3,3,4,3,3,6,6,6,4,6,6,6,4,6,6,6)
blockout <- array(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), c(2,16))


# call Fortran function
lucc_process <- function(SB, ST, NR, BI, BO, QA) {
  out <- .Fortran("lucc_process", as.integer(SB), as.integer(ST), as.integer(NR), as.integer(BI), as.integer(BO), as.integer(QA))
  #return(out[[5]])
  return(out)
}


lucc_process(SB = 2, ST = 16, NR = dim(query_array)[1], BI = ts, BO = blockout, QA = query_array)




x <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
diffs <- x[-1L] != x[-length(x)]

idx <- c(which(diffs), length(x))
diff(c(0, idx))

bcin <- array(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), c(2,16))

a <- rle(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
a$lengths[a$values==1]


true_ <- which(x==1)
length(true_)==0

is.integer(true_)
true_

if(is.null(true_)){
  counter <- 0
}

else { counter <- (max(true_) - min(true_) - 1) }




bcout <- apply(bcin, 1, function(ts){
  counter <- 0
  first <- 0
  for(i in 1:length(ts)) {
    if(ts[i]==1){
      if(first==0){first <- i}
      else{}
      first <- i - first
      first <- i
      }
    
    
  }
  return(counter)
})
bcout



exemplo = function(x){
  res= 0
  for(i in 1 : length(x)){
    res = res + x[i]
  }
  print(res)
  res_raiz = sqrt(abs(res))
  return(res/res_raiz)
}
teste = rnorm(1000000)
exemplo(teste)


Rprof()
exemplo(teste)
Rprof(NULL)
summaryRprof()$sampling.time






require(stats)
system.time(for(i in 1:100) mad(runif(1000)))

exT <- function(n = 10000) {

  system.time(for(i in 1:n) x <- mean(rt(1000, df = 4)))
}

exT() 
system.time(exT())