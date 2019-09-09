


# set path of raster files and metadata to make rasterBrick

# path
# |- 
# |- metadata.txt
# |- timeline.txt
# |- colors.txt
# |- raster
#    |- 2001.tif
#    |- 2002.tif
#    |- 2003.tif
#    |- ...
# |- results
#    |- brickOutput.tif
#    |- barplot.jpeg
# |- ...


###############  CREATE RASTER BRICK  ###############

#path <- "/home/carlos/Dropbox/MESTRADO/DADOS_MESTRADO/13_classes_MT_recorte/"
path <- "C:/Users/carlo/Dropbox/MESTRADO/DADOS_MESTRADO/13_classes_MT_recorte/"

# list raster on folder
lst <- list.files(path=paste0(path,"/raster"),pattern='.tif$',full.names=TRUE)
# stack creation
rstack <- raster::stack(lst)
# brick creation
rbrick <- raster::brick(rstack,  progress = "text", datatype='INT4S')
# save raster brick
raster::writeRaster(rbrick, filename = paste0(path, "/stInput.tif"), datatype='INT4S', overwrite=TRUE, progress = "text")




###############  IMPORT DATA  ###############

dates <- as.character(read.table(paste0(path, "/dates.txt"))$V1)
colors <- as.character(read.table(paste0(path, "/colors.txt"))$V1)
metadata <- as.character(read.table(paste0(path, "/metadata.txt"))$V1)

rbrick <- raster::brick(paste0(path,"/stInput.tif"))



# functions to process
# "before","after","meets","metby","holds","recur","convert","evolve","count","variance"


###############  PROCESS  ###############

Flist <- c(
  LuccPL::evolve(pattern_list1 = c("03.Forest"), 
                 pattern_list2 = c("05.Soy_Corn",
                                   "06.Soy_Cotton",
                                   "07.Soy_Fallow",
                                   "08.Soy_Millet",
                                   "09.Soy_Sunflower"),
                 date1 = 2001, 
                 date2 = 2017, 
                 dates = dates, 
                 metadata = metadata))



# functions to make a query array
query_array <- LuccPL::query(rbrick,Flist)

# Function to process a query array in rasterBrick
brickRasterOutput <- LuccPL::event(rbrick, query_array)


# save raster result on disk
raster::writeRaster(brickRasterOutput, filename = paste0(path, "/stOutput.tif"), datatype='INT4S', overwrite=TRUE, progress = "text")


for (i in 1:nlayers(brickRasterOutput)) {
  raster::writeRaster(brickRasterOutput[[i]], filename = paste0(path, "/raster_out/raster_out_",i,".tif"), datatype='INT4S', overwrite=TRUE, progress = "text")
}


###############  COUNT  ###############

count_output <- LuccPL::count(brickRasterOutput)

raster::writeRaster(count_output, filename = paste0(path, "/stCount.tif"), datatype='INT4S', overwrite=TRUE, progress = "text")


###############  VARIANCE  ###############

variance_output <- LuccPL::variance(rbrick)

raster::writeRaster(variance_output, filename = paste0(path, "/stVariance.tif"), datatype='INT4S', overwrite=TRUE, progress = "text")

###############  TRUE  ###############

true_output <- LuccPL::true(brickRasterOutput)

raster::writeRaster(true_output, filename = paste0(path, "/stTrue.tif"), datatype='INT4S', overwrite=TRUE, progress = "text")

###############  PLOT  ###############

# plot input
LuccPL::plot_input(rbrick,dates,metadata,colors,paste0(path,"/mpInput.jpeg"), map_title="13 classes MT recorte", Width = 1200, Height = 1200)

# plot output
LuccPL::plot_output(brickRasterOutput,dates,paste0(path,"/mpOutput.jpeg"), map_title="Evolve Forest - Soy", Width = 1200, Height = 1200)

# create table 
graphIn <- LuccPL::count(rbrick, for_time_step = TRUE, metadata = metadata, dates = dates)
graphOut <- LuccPL::count(brickRasterOutput, for_time_step = TRUE, metadata = c("0.false","1.true"), dates = dates)

# export csv
write.csv(graphIn, paste0(path,"/graphIn.csv"))
write.csv(graphOut, paste0(path,"/graphOut.csv"))

# export jpeg bar graph
LuccPL::lucc_barplot_data(df = graphIn, dates = dates, graph_title="13 classes MT recorte", style="bar", colors = colors, path_save_jpeg = path)
LuccPL::lucc_barplot_result(df = graphOut, dates = dates, graph_title="Evolve Forest - Soy", style="bar", colors = c("white","black"), path_save_jpeg = path)

# export line graph
LuccPL::lucc_barplot_data(df = graphIn, dates = dates, graph_title="13 classes MT recorte", style="line", colors = colors, path_save_jpeg = path)
LuccPL::lucc_barplot_result(df = graphOut, dates = dates, graph_title="Evolve Forest - Soy", style="line", colors = c("white","black"), path_save_jpeg = path)







