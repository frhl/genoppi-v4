#' @title require hide
#' @description if arg 'what' is not true when put through arg 'test', then the arg string 'shinyid'
#' will be hidden and disabled if arg disable is true. Used internally only for shiny server
#' @param what an object, string, boolean etc.
#' @param shinyid a shinyitem id that can be hidden.
#' @param hideif a user defined function that yields a T/F
#' @param disable boolean to disable shinyid when hidden.
#' @return invisible T/F depending on whether the item should be hidden. 
#' @export
#' 


reqhide <- function(what, shinyid, hideif=function(x) {is.null(x) || nchar(x) == 0}, disable=T){
  if (hideif(what)){
    shinyjs::hide(shinyid)
    if (disable) shinyjs::disable(shinyid)
    return(invisible(T))
  } else {
    shinyjs::show(shinyid)
    if (disable) shinyjs::enable(shinyid)
    return(invisible(F))
  }
  
}
