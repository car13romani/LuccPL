library(jpeg)
library(shiny)
library(shinyFiles)
source("R/server.R")
source("R/ui.R")







path <- "/home/carlos/DADOS_MESTRADO/classified_MT_15/recorte3/stInput.tif"



shinyApp(ui = ui, server = server)



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