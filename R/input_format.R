#' @title check input format
#' @description checks input format.
#' @param tabl a data.frame
#' @note an item in the approved format list
#' can appear more than once.
#' 

input_format <- function(tabl){
  
  # allowed formats
  allowed_formats = list(
    gene_rep = c('gene', 'rep[0-9]'),
    accession_rep = c('accession_number', 'rep[0-9]')
  )
  
  # check whether column is allowed format using regex
  check = lapply(allowed_formats, function(x){
    mat = lapply(x, function(y) grepl(y,colnames(tabl)))
    vec = apply(do.call(rbind, mat), 2, any)
    return(all(vec))
  })
  
  # should we remove the remaining columns?
  names(check) = names(allowed_formats)
  return(check) 
}


# tests
tabl1 = data.frame(gene = letters,rep1=runif(26), rep2=rnorm(26))
tabl2 = data.frame(gene = letters,rep1=runif(26), rep2=rnorm(26), somethingelse=1:26)
input_format(tabl2)

