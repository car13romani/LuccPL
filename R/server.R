




server <- function(input, output, session) {
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
               selectInput(paste0("patternI",input$add), label = '', choices = metadata, multiple=TRUE, selectize=TRUE)
        ),
        column(2,
               selectInput(paste0("patternJ",input$add), label = '', choices = metadata, multiple=TRUE, selectize=TRUE)
        ),
        column(2,
               selectInput(paste0("dateS",input$add), '  ', dates, multiple=FALSE, selectize=FALSE)
        ),
        column(2,
               selectInput(paste0("dateF",input$add), '  ', dates, multiple=FALSE, selectize=FALSE)
        ),
        column(1,
               radioButtons(paste0("logicalConnector",input$add), '', c('and', 'or'))
               
        )
        
      )


    )
  })
  
  
  output$table <- renderTable({
    if (input$action[[1]] == 0) {
      NULL
    } 
    else {
      
      qout <- reactive({ 
        
        fList <- eval(parse(text=paste0(
          "LuccPL::",eval(parse(text = paste0("input$inputFun",as.character(input$add)))),           #function
          "(c(",format1(eval(parse(text = paste0("input$patternI",input$add)))),       #patternI
          "),'",eval(parse(text = paste0("input$dateS",input$add))),"')" )))            #dateS
        
        LuccPL::query(input_dc = mt15cl, FUN_list = fList)
        
      })
      as.array(qout())
    
    }
  })
  
  
  
}


# function to format many patterns to parse
format1 <- function(x){
  out <- NULL
  if(length(x) == 1) return(x)
  else {
    for (i in 1:(length(x)-1)) {
      out <- paste0(out,"'",x[i],"',")
    }
    out <- paste0(out,"'",x[length(x)],"'")
    return(out)
  }
}
