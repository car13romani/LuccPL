## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    plotOutput("plot")
  )
  
  server <- function(input, output) {
    output$plot <- renderPlot({
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.25)
                     }
                   })
      plot(cars)
    })
  }
  
  shinyApp(ui, server)
}



start.time <- Sys.time()
end.time <- Sys.time()
print(paste("Count in:", (end.time-start.time), "seconds"))





require(stats)

a <- as.numeric(system.time( )[3])
as.numeric(a[3])


## Not run: 
exT <- function(n = 10000) {
  # Purpose: Test if system.time works ok;   n: loop size
  system.time(for(i in 1:n) x <- mean(rt(1000, df = 4)))
}
#-- Try to interrupt one of the following (using Ctrl-C / Escape):
exT()                 #- about 4 secs on a 2.5GHz Xeon
system.time(exT())    #~ +/- same

## End(Not run)