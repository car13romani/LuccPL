
mdata <- function(pattern_list, metadata){
  md_out <- NULL
  for(i in 1:length(pattern_list)){
    md_out[i] <- which(metadata == pattern_list[i])
    if(is.null(md_out[i])) stop("This pattern of Land Use dont exist in metadata!")
  }

  return(md_out)  
}


tdata <- function(date, dates){
  td_out <- NULL
  for(i in 1:length(date)){
    td_out[i] <- which(dates == date[i])
    if(is.null(td_out[i])) stop("This date dont exist in metadata!")
  }
  
  return(td_out)  
  
}

and_or_bool <- function(connector_list, n_relations){
  ao_out <- NULL
  if(is.null(connector_list)) connector_list <- "and"
  for(i in 1:length(connector_list)){
    if(pracma::strcmp(connector_list[i], "and"))  ao_out[i] <- 0 
    else if(pracma::strcmp(connector_list[i], "or")) ao_out[i] <- 1 
    #  else stop("Error in connectors 'and/or'")
  }
  if(length(ao_out) == 1) {
    ao_out[1:n_relations] <- ao_out
    ao_out[n_relations] <- 0
  }
  return(ao_out)
}
