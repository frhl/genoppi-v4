#' @title read input
#' @description reads
#' @param filename path to input file
#' @param sep the field character seperator, see \code{?read.table}.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line.
#' @export

read_input <- function(filename, header, sep){
  
  input = read.table(filename, header, sep)
  
  
  return(NULL)
}


