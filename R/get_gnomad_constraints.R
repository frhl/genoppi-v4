#' @title get gnomad constraint table
#' @description returns the gnomad constraint table based on gnomad 2.1.1
#' @param gene a string
#' @export


get_gnomad_constraints <- function(gene){
  
  # return data
  toci = function(x, y, z) return(paste0(x, ' [',y,', ',z,']'))
  row = gnomad_table[gnomad_table$gene %in% gene, ]
  if (nrow(row) != 1) stop(paste(gene, 'not in gnomAD 2.1.1'))
  
  # buil gnomad constraint frame
  gnomad = data.frame(category = c('Syn', 'Mis', 'pLoF'), #c('Synonymous', 'Missense', 'pLoF')
             exp.SNV = c(row$exp_syn, row$exp_mis, row$exp_lof),
             obs.SNV = c(row$obs_syn, row$obs_mis, row$obs_lof),
             Z = c(row$syn_z, row$mis_z, NA),
             pLI = c(NA, NA, row$pLI),
             oe.90ci = c(toci(row$oe_syn,row$oe_syn_lower, row$oe_syn_upper),
                         toci(row$oe_mis,row$oe_mis_lower, row$oe_mis_upper),
                         toci(row$oe_lof,row$oe_lof_lower, row$oe_lof_upper)))
  return(gnomad)
}