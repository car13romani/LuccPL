#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##        R script with shiny ui to web service                ##
##                                                             ##
##                                             2018-07-20      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################


library(shiny)
ui <- shiny::shinyUI(navbarPage("LuccPL",
                                
########################################  PAGE 1 (BRICK)  #####################################                                
    tabPanel("Create Brick",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                 
                   shinyFiles:: shinyDirButton('dirProject', 'Project directory','Select the folder to save project', FALSE),
                   verbatimTextOutput("dir1"),
                   br(),
                 fileInput("rawInput", "Choose tif files (in-order)",
                             multiple = TRUE,
                             accept = c("text/tif",
                                        "text/comma-separated-values,text/plain",
                                        ".tif"))
                   
                   
                 ),
                 actionButton("makeBrick", "Make brick"),
                 hr(),
                 sliderInput("tstepIn1", "Date :", min = 1, max = 16,   #################### ajustar o 16
                             value = 1, step = 1),
                 
                 actionButton("plotInput1", "Plot Input")
                 #br(),
                 
                 #verbatimTextOutput("listaa")
                   
               ),
                 
               
               mainPanel(
                 # Use imageOutput to place the image on the page
                 imageOutput("imageInput1")
                 
               )
             )
             
    ),                            
                                
                                

########################################  PAGE 2 (IMPORT)  #####################################
    tabPanel("Import",
             sidebarLayout(
               sidebarPanel(
                 
                 
                 # input path of folder with tif files to make a brick or path of a brick
                 fluidRow(
                   
                   # column(5, textInput("dataFolder", "Path of classified data folder")),
                   # column(1, actionButton("importData", "Import"))
                   fileInput("dataFolder", "Choose brick file",
                             multiple = FALSE,
                             accept = c("text/tif",
                                        "text/comma-separated-values,text/plain",
                                        ".tif"))
                   
                   
                 ),
                 
                 sliderInput("tstepIn2", "Date :", min = 1, max = 16,
                             value = 1, step = 1),
                 actionButton("plotInput2", "Plot Input"),
                 br(),
                 
                 # input the metadata file with patterns of land use
                 fluidRow(
                   fileInput("mdataPath", "Choose metadata file",
                             multiple = FALSE,
                             accept = c("text/txt",
                                        "text/comma-separated-values,text/plain",
                                        ".txt"))
                 ),
                 
                 # import the dates file to make a relation with each time step of brick                            
                 fluidRow(
                   fileInput("datesPath", "Choose Dates file",
                             multiple = FALSE,
                             accept = c("text/txt",
                                        "text/comma-separated-values,text/plain",
                                        ".txt"))
                 )
                 
                 
               ),
               
               
               
               mainPanel(
                 # Use imageOutput to place the image on the page
                 imageOutput("imageInput2")
                 
               )
             )         
             
             
             
    ),

                             
########################################  PAGE 2 (PROCESS)  #####################################
    tabPanel("Process",
             wellPanel(
               
               fluidRow(
                 column(2, h4("Functions")),
                 column(2, h4("Pattern I")),
                 column(2, h4("Pattern J")),
                 column(2, h4("Date S")),
                 column(2, h4("Date F")),
                 column(1, h4("Logical connector"))
               ),
               
               hr(),
               
               fluidRow(
                 actionButton("add", "Add line"),
                 actionButton("query", label = "Generate query array"),
                 actionButton("process", "Process")
               ),
               br(),
               textOutput("teste"),
               tableOutput("table")
               
             )         
    ),
    

########################################  PAGE 3 (EXPORT)  #####################################
    tabPanel("Export",
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput("tstepOut1", "Date :", min = 1, max = 16,
                             value = 1, step = 1),
                 
                 actionButton("plotResult1", "Plot result"),
                 
                 downloadButton('stOut.tif', 'Download')
                 
               ),
               
               
               mainPanel(
                 # Use imageOutput to place the image on the page
                 imageOutput("imageOutput1")
               )
               
             )
             
             
    )
    )

)


