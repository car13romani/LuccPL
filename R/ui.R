
ui <- shiny::shinyUI(navbarPage("LuccPL",
                         # Import data
                         tabPanel("Import"),
                         # Process with query
                         tabPanel("Process",
                                  
                                  
                                  br(),
                                  fluidRow(
                                    column(2, h4("Functions")),
                                    column(2, h4("Pattern I")),
                                    column(2, h4("Pattern J")),
                                    column(2, h4("Date S")),
                                    column(2, h4("Date F")),
                                    column(1, h4("Logical connector"))
                                  ),
                                  
                                  hr(),
                                  
                                  actionButton("add", "Add line"),
                                  actionButton("action", label = "Action"),
                                  
                                  #mainPanel(
                                  tableOutput("table")
                                  
                                  #)
                         ),
                         
                         # Export or download results
                         tabPanel("Export")
))