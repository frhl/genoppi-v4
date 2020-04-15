# contains functions for making direct image comparison
# based on pixel level. Ideally, these functions should
# be able to also rasterize an iamge.



# from tools package.. to extract file exntesion
file.ext = function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

# function to generate test paths
make_test_path <- function(func, id, type = 'png', create.dir = T){
  fname = paste(func,id,type, sep = '.')
  main = system.file('tests','testthat','reference', package = "genoppi")
  if (!dir.exists(file.path(main,func)) & create.dir) {
    newdir = file.path(main,func)
    dir.create(newdir)
    write(paste('created',newdir),stderr()) 
  }
  paths = file.path(main,func,fname)
  return(paths)
}

# save a reference
save_gg_reference <- function(plt, func, id, type = 'png', width = 1, height = 1){
  paths = make_test_path(func, id, type)
  ggsave(filename = paths, plt, width = width, height = height)
}


# read binary
png_make_links <- function(plt, func, id, type = 'png', width = 1, height = 1){

  ref = make_test_path(func, id, type)
  if (!file.exists(ref)) stop(paste(ref,'does not exist!'))
  #res = tempfile(fileext = paste0('.',type))
  res = 'tmp-res.png'
  ggsave(filename = res, plt, width = width, height = height)
  return(list(res=res,ref=ref))
}

# compare the result
compare_with_ref <- function(plt, func, id, type = 'png', width=1, height=1){
  
  links = png_make_links(plt, func, id, width = width, height = height)
  images = lapply(links, function(x) as.matrix(readPNG(x)[,,1]))
  if (all(dim(images$res) != dim(images$ref))) stop('image dimensions are not the same!')
  return(all(images$ref == images$res))
}

