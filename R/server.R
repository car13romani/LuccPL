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


#' @title server
#' @name server
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage 
#' 
#' @export server
#' @import shiny
#' @import raster
#' 


options(shiny.maxRequestSize=3000*1024^2) 
server <- function(input, output, session) {
  
########################################  PAGE 1 (BRICK)  #####################################  
  
  # get mounted volumes
  volumes <- getVolumes()
  # function to get project folder path
  shinyDirChoose(input, 'dirProject', roots=volumes, session = session)
  folderPath <- reactive({parseDirPath(volumes, input$dirProject)})
  # print project folder path
  output$dir1 <- renderPrint(
    if(length(folderPath()) == 0) return("Choose the project folder")
    else return(folderPath())
  )
  
  # if makeBrick button is true
  observeEvent(input$makeBrick,{
    # creates the brick from a list of rasters
    print('Making brick')
    brick_created <- raster::brick(raster::stack(input$rawInput$datapath),  progress = "text", datatype='INT4S')
    
    # write a brick in project folder
    filename <- paste(folderPath(), 'stInput.tif', sep = '/')
    print('Export raster')
    raster::writeRaster(brick_created, filename = filename, datatype='INT4S', overwrite=TRUE, progress = "text")
    print('create subfolder')
    
    # create folder to jpeg images
    dir.create(file.path(folderPath(), "jpeg"), showWarnings = FALSE)
    print('Export jpeg')

    # parallel export jpeg images
    parallel::mclapply(as.list(1:(raster::nlayers(brick_created))),function(i) {
        file <-  file.path(folderPath(), "jpeg", paste0("plotIn",i,".jpeg"))
        print(file)
        jpeg(file = file, bg = "transparent", height=(1080*nrow(brick_created[[i]]))/ncol(brick_created[[i]]), width=1080)
        print(rasterVis::levelplot(brick_created[[i]], col.regions=colors,  contour=F, margin=F, scales = list(draw=FALSE), colorkey=NULL,
                             par.settings = list(axis.line = list(line=0), mar=c(0,0,0,0), omi=c(0,0,0,0), 
                                                 xaxt='n', yaxt='n', bg='transparent')))
        dev.off()
    },  mc.cores = parallel::detectCores()/2)

    print('Done')
      })             
                             

  
  output$listaa <- renderPrint(input$rawInput$datapath)
  

  
  observeEvent(input$plotInput1, {
    
    output$imageInput1 <- renderImage({
      width  <- session$clientData$output_imageInput1_width
      list(
        src = file.path(folderPath(), "jpeg", paste0("plotIn",input$tstepIn1,".jpeg")),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepIn1
      )
      
    }, deleteFile = FALSE)
    
  })
  

  
########################################  PAGE 2 (IMPORT)  #####################################

    # import datacube file (tif)
    brickRasterInput <<- reactive( raster::brick(input$dataFolder$datapath) )
    
    # import metadata file
    metadata <<- reactive( as.character(read.table(input$mdataPath$datapath)$V1) )
    
    # import dates
    dates <<- reactive( as.character(read.table(input$datesPath$datapath)$V1) )
    
    # if plotResult button is clicked
    observeEvent(input$plotInput2, {
      
      # function to plot images imported
      output$imageInput2 <- renderImage({
        width  <- session$clientData$output_imageInput2_width
        list(
          src = file.path(folderPath(), "jpeg", paste0("plotIn",input$tstepIn2,".jpeg")),
          width = width,
          contentType = "image/jpeg",
          alt = input$tstepIn2
        )
        
      }, deleteFile = FALSE)
      
    })
    
    
    

#########################################  PAGE 3 (PROCESS)  #################################################
  observeEvent(input$add, {
    insertUI(
      selector = "#add",
      where = "beforeBegin",
      ui =  fluidRow(
       # hr(),
        column(2,
               selectInput(paste0("inputFun", input$add), paste("Function", input$add), c("","before","after","meets","metby","holds","recur","convert","evolve"), multiple=FALSE, selectize=FALSE)         
        ),
        column(2,
               selectInput(paste0("patternI",input$add), label = '', choices = metadata(), multiple=TRUE, selectize=TRUE)
        ),
        column(2,
               selectInput(paste0("patternJ",input$add), label = '', choices = metadata(), multiple=TRUE, selectize=TRUE)
        ),
        column(2,
               selectInput(paste0("dateS",input$add), '  ', dates(), multiple=FALSE, selectize=FALSE)
        ),
        column(2,
               selectInput(paste0("dateF",input$add), '  ', dates(), multiple=FALSE, selectize=FALSE)
        ),
        column(1,
               radioButtons(paste0("logicalConnector",input$add), '', c('and', 'or'))
               
        )
        
      )
    )
  })
  
  ################################################

  output$teste <- renderText(input$dataFolder$datapath)
    
    
  output$table <- renderTable({
    if (input$query[[1]] == 0) {
      
      NULL
    } 
    else {
      
      qout <<- reactive({ 
        fList <- NULL
        for (i in 1:input$add[[1]]) {
          
          funct <- eval(parse(text=paste0(
            eval(parse(text = paste0("input$inputFun",as.character(i)))),           #function
            "(c(",format1(eval(parse(text = paste0("input$patternI",i)))),"),",       #patternI
            "c(",format1(eval(parse(text = paste0("input$patternJ",i)))),"),'",       #patternJ
            eval(parse(text = paste0("input$dateS",i))),"','",                        #dateS
            eval(parse(text = paste0("input$dateF",i))),"', dates(), metadata())"))       #dateF
          )
            
          if(i==1){ fList <- funct }
          else { fList <- c(fList, (eval(parse(text=paste0("input$logicalConnector",i-1)))), funct) }
            
        }
        
        print(fList)
        LuccPL::query(input_dc = brickRasterInput(), FUN_list = fList)
        
      })
      #print(qout())
      as.array(qout())
      
    }
  })
  
  #############################################
  
    
    
    
    observeEvent(input$process, {
      print("Processing...")
      brickRasterOutput <<- LuccPL::event(brickRasterInput(), qout())
      print("Export brick")
      filenameOut <- paste(folderPath(), 'stOutput.tif', sep = '/')
      raster::writeRaster(brickRasterOutput, filename = filenameOut, datatype='INT4S', overwrite=TRUE, progress = "text")
      
      print('Export jpeg')
      # parallel export jpeg images
      parallel::mclapply(as.list(1:(raster::nlayers(brickRasterOutput))),function(i) {
        file <-  file.path(folderPath(), "jpeg", paste0("plotOut",i,".jpeg"))
        print(file)
        jpeg(file = file, bg = "transparent", height=(1080*nrow(brickRasterOutput[[i]]))/ncol(brickRasterOutput[[i]]), width=1080)
        print(rasterVis::levelplot(brickRasterOutput[[i]], col.regions=c("#FFFFFF","#000000"),  contour=F, margin=F, scales = list(draw=FALSE), colorkey=NULL,
                                   par.settings = list(axis.line = list(line=0), mar=c(0,0,0,0), omi=c(0,0,0,0), 
                                                       xaxt='n', yaxt='n', bg='transparent')))
        dev.off()
      },  mc.cores = parallel::detectCores()/2)
      
      print('Done')
    })
    
    #########################################  PAGE 4 (EXPORT)  #################################################    
    
   # paste0(strsplit(x=path, split = "stBrick.tif"), "stBrickOut.tif")
    
  output$stOut.tif <- downloadHandler(
    
    filename = function() {
      paste("output", "tif", sep=".")
    },
    
    content = function(file) {
      file.copy(paste0(strsplit(x=input$dataFolder$datapath, split = ".tif"), "stBrickOut.tif"),file)
      },
    contentType = "tif"
  )
      
      
      
      
      
      
      
    
     observeEvent(input$plotResult1, {

      output$imageOutput1 <- renderImage({
        width  <- session$clientData$output_imageOutput1_width
        list(
        src = file.path(folderPath(), "jpeg", paste0("plotOut",input$tstepOut1,".jpeg")),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepOut1
        )
       
      }, deleteFile = FALSE)
    
    })
 
}


