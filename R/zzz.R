".First.lib" <- function(lib, pkg)  
{  
  library.dynam("lucc_process", package = Lucc_PL, lib.loc = lib)  
  return(invisible(0))  
}