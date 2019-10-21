plotPCA = function(object, meta, group="condition") {
  require(ggplot2)
  # calculate the variance for each gene
  #rv <- rowVars(assay(object))
  
  # select the ntop genes by variance
  #select <- order(rv, decreasing=TRUE)[seq_len(min(ntop, length(rv)))]
  
  # perform a PCA on the data in assay(x) for the selected genes
  pca <- prcomp(((object)))
  
  # the contribution to the total variance for each component
  percentVar <- pca$sdev^2 / sum( pca$sdev^2 )
  
  if (!all(group %in% names(meta))) {
    stop("the argument 'intgroup' should specify columns of meta")
  }
  
  intgroup.df <- as.data.frame(meta[, group, drop=FALSE])
  
  #TODO turn all columns into strings?
  
  # add the intgroup factors together to create a new grouping factor
  group2 <- if (length(group) > 1) {
    factor(apply( intgroup.df, 1, paste, collapse=":"))
  } else {
    meta[[group]]
  }
  
  
  # assembly the data for the plot
  d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], group=group2, intgroup.df, name=rownames(object))
  
  # if (returnData) {
  #   attr(d, "percentVar") <- percentVar[1:2]
  #   return(d)
  # }
  
  ggplot(data=d, aes_string(x="PC1", y="PC2", color="group")) + geom_point(size=3) + 
    xlab(paste0("PC1: ",round(percentVar[1] * 100),"% variance")) +
    ylab(paste0("PC2: ",round(percentVar[2] * 100),"% variance")) +
    coord_fixed()
}
