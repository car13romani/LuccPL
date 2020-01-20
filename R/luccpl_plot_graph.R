#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##       R script                                              ##
##                                                             ##
##                                             2019-08-29      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################


#' @title Plot result in bar or line graph
#' @name luccpl_barplot_result
#' @aliases luccpl_barplot_result
#' @author Carlos Alexandre Romani
#' @docType data
#' 
#' @description Creates a line or bar graph that represents the result of a query. 
#'  
#' @usage luccpl_barplot_result(df=NULL, dates=NULL, graph_title=NULL, style="bar", colors=NULL, path_save_jpeg=NULL)
#' 
#' @param df Dataframe generated from the LuccPL::luccpl_count(for_time_step = TRUE), when luccpl_count is applied to the rasterBrick result of a query.
#' @param dates Integer. A vector with the timeline dates.
#' @param graph_title Character. The name of graph.
#' @param style Character. "bar" or "line" graph.
#' @param colors Character. A vector with color names.
#' @param path_save_jpeg Character. The path of output graph in .jpeg format.
#' 
#' @return A jpeg file with bar or line graph that represents the result of a query.
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_bar ggtitle theme scale_color_manual ggsave
#' @importFrom ensurer ensure_that
#' 
#' @export 
#' 


luccpl_barplot_result <- function(df=NULL, dates=NULL, graph_title=NULL, style="bar", colors=NULL, path_save_jpeg=NULL){

  ensurer::ensure_that(df, !is.null(df),
                       err_desc = "Define a valid array.")
  ensurer::ensure_that(dates, !is.null(dates),
                       err_desc = "Define a valid timeline.")
  
  if(!is.null(colors)){
    df$colors1 <- c("white","black")
  }
  else{df$colors1 <- colors}

  
  DF1 <- reshape2::melt(df, id.var=c("land_use","colors1"))
  
  colnames(DF1) <- c("LandUse","color","Timeline","Count")

  #DF2 <- DF1[order(DF1$color),]
  

  if(style=="bar"){
    ggplot2::ggplot(DF1, aes(fill=LandUse, y=Count, x=Timeline)) +
      geom_bar( stat="identity")+
      ggtitle(graph_title)+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_fill_manual(values=DF1$color)
    
    if(!is.null(path_save_jpeg)){
      ggsave(paste0(path_save_jpeg, "/barplotO.jpeg"), width=10, height=6, dpi=600, device='jpeg')
    }
  }
  if(style=="line"){
    ggplot2::ggplot(data=DF1, ggplot2::aes(y=Count, x=Timeline, colour=LandUse, group=LandUse)) +
      #geom_point() +
      geom_line()+
      ggtitle(graph_title)+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_color_manual(values=DF1$color)

    if(!is.null(path_save_jpeg)){
      ggsave(paste0(path_save_jpeg, "/lineplotO.jpeg"), width=10, height=6, dpi=600, device='jpeg')
    }
  }
  
}




#' @title Plot input data in bar or line graph
#' @name luccpl_barplot_data
#' @aliases luccpl_barplot_data
#' @author Carlos Alexandre Romani
#' @docType data
#' 
#' @description Creates a line or bar graph that represents the input data. 
#'  
#' @usage luccpl_barplot_data(df=NULL, dates=NULL, graph_title=NULL, style="bar", colors=NULL, path_save_jpeg=NULL)
#' 
#' @param df Dataframe generated from the LuccPL::luccpl_count(for_time_step = TRUE), when luccpl_count is applied to the input rasterBrick.
#' @param dates Integer. A vector with the timeline dates.
#' @param graph_title Character. The name of graph.
#' @param style Character. "bar" or "line" graph.
#' @param colors Character. A vector with color names of original raster, same order.
#' @param path_save_jpeg Character. The path of output graph in .jpeg format.
#' 
#' @return A jpeg file with bar or line graph that represents the input data.
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_bar ggtitle theme scale_color_manual ggsave
#' @importFrom ensurer ensure_that
#' 
#' @export 
#' 


luccpl_barplot_data <- function(df=NULL, dates=NULL, style="bar", graph_title=NULL, colors=NULL, path_save_jpeg=NULL){
  
  ensurer::ensure_that(df, !is.null(df),
                       err_desc = "Define a valid array.")
  ensurer::ensure_that(dates, !is.null(dates),
                       err_desc = "Define a valid timeline.")
  ensurer::ensure_that(colors, !is.null(colors),
                       err_desc = "Define valid colors.")
  
  
  df$colors1 <- colors
  
  DF1 <- reshape2::melt(df, id.var=c("land_use","colors1"))
 
  colnames(DF1) <- c("LandUse","color","Timeline","Count")

  if(style=="bar"){
    # Stacked
    ggplot(DF1, aes(fill=LandUse, y=Count, x=Timeline)) +
      geom_bar( stat="identity")+
      ggtitle(graph_title)+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_fill_manual(values=DF1$color)
    
    # ggplot(data=dataG, aes(x=dates, y=Count)) +
    #   geom_bar(stat="identity", fill="steelblue")+
    #   geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
    #   ggtitle("BarPlot")+
    #   
    if(!is.null(path_save_jpeg)){
      ggsave(paste0(path_save_jpeg, "/barplotI.jpeg"), width=10, height=6, dpi=600, device='jpeg')
    }
  }
  else{
    ggplot(data=DF1, aes(fill=LandUse, y=Count, x=Timeline, colour=LandUse, group=LandUse)) +
      #geom_point() +
      geom_line()+
      ggtitle(graph_title)+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_color_manual(values=DF1$color)

    if(!is.null(path_save_jpeg)){
      ggsave(paste0(path_save_jpeg, "/lineplotI.jpeg"), width=10, height=6, dpi=600, device='jpeg')
    }
  }

  
}
