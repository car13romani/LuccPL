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
##                                             2018-03-29      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################


#' @title lucc_barplot_result
#' @name lucc_barplot_result
#' @author Carlos Alexandre Romani
#'
#' @description barplot
#' 
#' @usage (rbrick, query_array)
#' 
#' @param rbrick input brick
#' @param query_array result of parse expression
#' 
#' @return Brick of result (bool)
#' @export lucc_barplot_result
#' @import raster
#' @import parallel
#' 

#df <- graphOut



# df is output of count(for_time_step=TRUE)
lucc_barplot_result <- function(df=NULL, dates=NULL, graph_title=NULL, style="bar", colors=NULL, path_save_jpeg=NULL){

  df$colors1 <- c("white","black")
  
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



lucc_barplot_data <- function(df=NULL, dates=NULL, style="bar", graph_title=NULL, colors=NULL, path_save_jpeg=NULL){
  
 # ensurer::ensure_that(df, !is.null(df),
#                       err_desc = "Define a valid array.")
#ensurer::ensure_that(dates, !is.null(dates),
#                       err_desc = "Define a valid timeline.")
#  ensurer::ensure_that(colors, !is.null(colors),
#                       err_desc = "Define valid colors.")
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
