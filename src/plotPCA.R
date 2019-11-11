plotPCA <- function(object, meta, group="condition", colors=c()) {
  require(ggplot2)
  
  # perform a PCA on the data in assay(x) for the selected genes
  pca <- prcomp(object)
  
  # the contribution to the total variance for each component
  percentVar <- pca$sdev^2 / sum( pca$sdev^2 )
  
  if (!all(group %in% names(meta))) {
    stop("the argument 'group' should specify columns of meta")
  }
  
  intgroup.df <- as.data.frame(meta[, group, drop=FALSE])
  

  #TODO turn all columns into strings?
  
  groupNames <- colnames(intgroup.df)
  

  # add the intgroup factors together to create a new grouping factor
  # 
  group2 <- NA
  n <- 0
  # 
   if (length(group) > 2) {
     group2 <- factor(apply( intgroup.df, 1, paste, collapse=":"))
     n <- length((group2))
  } else {

    group2 <- as.data.frame(lapply(intgroup.df, as.character))
    colnames(group2) <- groupNames
    n <- length(unique(group2[,1]))
  }
  print(n)

  
  if (isEmpty(colors)) {
    #n <- 42
    hues = seq(15, 375, length = n + 1)
    colors <- hcl(h = hues, l = 65, c = 100)[1:n]
    
  }  
  # assembly the data for the plot
  d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2])
  d <- cbind(d, group2)
  
	d <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], group=group2, intgroup.df, name=rownames(object))
	g <- ggplot(d) +
	  coord_fixed()
	
	if (length(group) > 2) {
	  g <- g + aes_string(x="PC1", y="PC2", color="group") + 
	    geom_point(size=3) + 
	    theme(legend.position = "none")
	} else if(length(group) == 2){
	  g <- g + aes(x=PC1, y=PC2, group=d[[groupNames[2]]]) +
	    geom_point(size=3, aes(shape=d[[groupNames[2]]],color=d[[groupNames[1]]])) +
	    theme(legend.position = "bottom",
	          legend.box = "vertical") +
	    labs(  col=groupNames[1], shape=groupNames[2]) + 
	    scale_shape_manual(values=c(16,15,18,3,4))
	} else {
	  g <- g + aes_string(x="PC1", y="PC2" )+
	    geom_point(size=3, aes(color=d[[groupNames[1]]] )) + 
	    labs(  col=groupNames[1]) +
	    theme(legend.position = "bottom",
	          legend.box = "vertical") 
	}
	g <- g + xlab(paste0("PC1: ",round(percentVar[1] * 100),"% variance")) +
	  ylab(paste0("PC2: ",round(percentVar[2] * 100),"% variance")) +
	  scale_color_manual(values=colors)
	
	return(g)
	  

}

#plotPCA(fullData$expData$combined, mutate_all(fullData$meta$combined, as.character), c("Week", "Treatment"))
#plotPCA(fullData$expData$combined, fullData$meta$combined, c("Week"))
