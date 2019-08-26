


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


path <- "/home/carlos/Dropbox/MESTRADO/DADOS_MESTRADO/13_classes_MT_recorte"

dates <- as.character(read.table(paste0(path, "/dates.txt"))$V1)
colors <- as.character(read.table(paste0(path, "/colors.txt"))$V1)
metadata <- as.character(read.table(paste0(path, "/metadata.txt"))$V1)

# list files .tif in path
lst <- list.files(path=paste0(path,"/raster"),pattern='.tif$',full.names=TRUE)

# stack creation
rstack <- raster::stack(lst)

# brick creation
rbrick <- raster::brick(rstack,  progress = "text", datatype='INT4S')

#raster::writeRaster(rbrick, filename = paste0(path, "/brickRasterInput.tif"), datatype='INT4S', overwrite=TRUE, progress = "text")

# Using a created brick (disk storage)
#rbrick <- raster::brick(paste0(path,"/stInput.tif"))

# functions to process
# "before","after","meets","metby","holds","recur","convert","evolve","count","variance"

# create a list of functions
# Flist <- c(
# LuccPL::evolve(pattern_list1 = c("Forest","Savanna"), 
#        pattern_list2 = c("Pasture_1","Pasture_2"), 
#        date1 = 2001, 
#        date2 = 2016, 
#        dates = dates, 
#        metadata = metadata),
# "or",
# LuccPL::convert(pattern_list1 = c("Forest","Savanna"), 
#         pattern_list2 = c("Pasture_1","Pasture_2"), 
#         date1 = 2001, 
#         date2 = 2016, 
#         dates = dates, 
#         metadata = metadata)
# )


Flist <- c(
  LuccPL::evolve(pattern_list1 = c("03.Forest"), 
                 pattern_list2 = c("04.Pasture"), 
                 date1 = 2001, 
                 date2 = 2017, 
                 dates = dates, 
                 metadata = metadata))



# functions to make a query array
query_array <- LuccPL::query(rbrick,Flist)

# Function to process a query array in rasterBrick
brickRasterOutput <- LuccPL::event(rbrick, query_array)

# save raster result on disk
#raster::writeRaster(brickRasterOutput, filename = paste0(path, "/brickRasterOutput.tif"), datatype='INT4S', overwrite=TRUE, progress = "text")


#for (i in 1:nlayers(brickRasterOutput)) {
#  raster::writeRaster(brickRasterOutput[[i]], filename = paste0(path, "/raster_out/raster_out_",i,".tif"), datatype='INT4S', overwrite=TRUE, progress = "text")
#}


#rastervar <- variance(rbrick)
#count_output <- count(brickRasterOutput)

#f <- hist(count_output)


graphIn <- count(rbrick, for_time_step = TRUE, metadata = metadata, dates = dates)
graphOut <- count(brickRasterOutput, for_time_step = TRUE, metadata = c("0.false","1.true"), dates = dates)

lucc_barplot_data(df = graphIn, dates = dates, style="bar", colors = colors, path_save_jpeg = path)
lucc_barplot_result(df = graphOut, dates = dates, style="bar", colors = c("white","black"), path_save_jpeg = path)


lucc_barplot_data(df = graphIn, dates = dates, style="line", colors = colors, path_save_jpeg = path)
lucc_barplot_result(df = graphOut, dates = dates, style="line", colors = c("white","black"), path_save_jpeg = path)




graph
#raster::plot(count_output)





