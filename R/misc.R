#' @title misc tools
#' @rdname misc
#' @export

#' @description length of unique items.
#' @param x a vector or list of items.
#' @export
lun <- function(x) length(unique(as.vector(x)))

#' @title not in
#' @description returns true for x not in y
#' @param x value x
#' @param y value y
#' @export
'%nin%' <- function(x, y) !(x %in% y)

#' @title omit nulls from list
#' @description remove NULLs in list
#' @param lst an R list
#' @export
null_omit <- function(lst) {
  lst[!vapply(lst, is.null, logical(1))]
}

#' @title warnings to stderr
#' @description sends a message to stderr (i.e shiny)
#' @param msg the message
#' @param file string, e.g. a filename.
#' @export
catf <- function(msg, file = stderr()){
  if (!is.null(file)) cat(file = file, msg)
}

#' @title as.bait
#' @description quickly format the bait so that it can be used by various overlay functions.
#' @param bait string indicating the bait.
#' @export
as.bait <- function(bait) return(list(baitlist=data.frame(gene=bait, col_significant='red', col_other='orange')))

#' @title findit
#' @description find files and lines that contain 'what'
#' @param what string to find
#' @param directory string, directory
#' @keywords internal
#' @export
findit <- function(what = 'name', directory = 'R'){
  files = list.files(directory)
  lst = lapply(files, function(x){
    lines = readLines(file.path(directory, x))
    lenlines = 1:length(lines)   
    re = grepl(what, lines)
    if (any(re)) return(lenlines[re]) else return(NULL)
  })
  names(lst) = files
  lst = null_omit(lst)
  return(lst)
}



