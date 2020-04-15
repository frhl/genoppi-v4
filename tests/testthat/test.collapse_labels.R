context('collapse_labels')


test_that('get colors by artifical dataset',{

  #make dataset
  df <- read_input("data/test.BCL2vsIgG.GPiN.txt",header=T,sep="\t")$data
  df <- calc_mod_ttest(head(df))
  df <- id_enriched_proteins(df)
  df$dataset = 'pulldown'
  df$alt_text = as.character(letters[1:nrow(df)])
  
  # duplicated row (i.e. same gene name)
  #inweb = get_inweb_list('BCL2')
  #inweb = inweb[inweb$significant, ]
  
  
  #collapse_labels(combined, collapse_by = 'gene', collapse_into = 'alt_text', )


})








