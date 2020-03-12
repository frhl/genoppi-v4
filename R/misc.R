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
