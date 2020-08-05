getEigenGenes <- function(data, genes, verbose=1) {
  
  d <- data[, genes]
  pca <- prcomp(scale(d))
  
  #extract mayority
  pcaPos<- rownames(pca$rotation[which (pca$rotation[,'PC1'] > 0),])
  pcaNeg<- rownames(pca$rotation[which (pca$rotation[,'PC1'] < 0),])
  
  positive <- NA
  negative <- NA
  if (length(pcaPos) > length(pcaNeg)) {
    positive <- colnames(data[,which(colnames(data) %in% pcaPos)])
    negative <- colnames(data[,which(colnames(data) %in% pcaNeg)]) 
  } else {
    positive <- colnames(data[,which(colnames(data) %in% pcaNeg)])
    negative <- colnames(data[,which(colnames(data) %in% pcaPos)])  
  }
  
  results <- list(positive, negative)
  #print(results)
  names(results) <- c("positive", "negative")
  
  
  return (results)
}

#temp <- getEigenGenes(controlData, Cluster1)
