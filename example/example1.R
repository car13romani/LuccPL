

lst <- list.files(path=path,pattern='.tif$',full.names=TRUE)
# stack creation
rstack <- raster::stack(lst)
# brick creation
print('Making brick')
a <- raster::brick(rstack,  progress = "text", datatype='INT4S')



path <- "/home/carlos/DADOS_MESTRADO/classified_MT_15/recorte3/"

shiny::withProgress()

dataPath <- strsplit(x=path, split = "stBrick.tif")

rteste <- import_brick(path)

rteste[[1]]


function(input, output) {
  datasetInput <- reactive({
    # Fetch the appropriate data object, depending on the value
    # of input$dataset.
    switch(input$dataset,
           "Rock" = rock,
           "Pressure" = pressure,
           "Cars" = cars)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  
  jpeg::writeJPEG()

  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$dataset, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
}






pracma::tic("Process")
a = 0
for(i in 100000000){ a <- a+ 1}
t <- pracma::toc("Process")
#print(paste("Processed in:", (end.time-start.time), "seconds"))
showNotification(paste("Processed in:", as.numeric(t[1])-as.numeric(t$tic), "seconds"), duration = NULL)
t
as.numeric(t)

