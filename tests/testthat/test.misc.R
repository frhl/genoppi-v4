context('misc')

test_that('various minor misc functions',{
  
  expect_equal(lun(c(1,2,3,1)), 3)
  expect_equal(c('a','q') %nin% letters, !c('a','q') %in% letters)
  expect_equal(null_omit(list(x=NULL,y=1,Z=NULL)), list(y=1))
  expect_equal(as.bait('BCL2'), list(bait=data.frame(gene='BCL2',col_significant='red',col_other='orange')))
  
  
  orig = assign_freq(data.frame(x=c('a','b','c','d','a','a')), col = 'x')
  ref = data.frame(x=c('a','a','a','b','c','d'), Freq=c(3,3,3,1,1,1))
  expect_equal(orig,ref)

})