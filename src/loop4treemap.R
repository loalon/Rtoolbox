plotTreemap <- function(x, enrichment='go') {
  require(treemap)
  
  for( enr in 1:length(x)) {  
    
    if (length(rownames(x[[enr]][[enrichment]])) >0) {
      
    
    thisName <- enr
    pdf(paste0(thisName, "_", enrichment, ".pdf"))
    
    x[[enr]][[enrichment]]$size <- abs(log10(x[[enr]][[enrichment]]$padj))
    
    index = index = c('name')
    if(enrichment =='go') {
      index = c('namespace', 'name')
      
    }
    treemap(x[[enr]][[enrichment]], index = index, 
            vSize = ('size'), 
            #palette = "YlOrRd",
            type = "categorical", vColor = 'name', title="", inflate.labels = FALSE, 
            lowerbound.cex.labels = 0, position.legend = "none"
    )
    
    dev.off()
    }
  }
}

plotTreemap(controlEnr,enrichment = 'mapman')
