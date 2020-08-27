##TODO, explain that this works for a condition vs control, if is reverse, 
#user needs to inverse=T
plotUpAndDown <- function(x, xlabel="Comparisons", ylabel="Number of genes", 
                          upName="Up", downName="Down",
                          upColor="#b2182b", downColor="#2166ac", inverse=F) {
  require(ggplot2)
  require('DESeq2')
  
  if (class(x) != "list")
    stop("First parameter must be a list containing at least one DESeq2 result")
  
  #x is a list of DESEQ results
  myDF <- data.frame(time = character(0), counts = numeric(0), class = character(0))
  
  prevUp <- NA
  prevDown <- NA
  ##TODO, check for names otherwise it doesn't work
  
  lapply(1:length(x), function(de){
    thisName <- names(x[de])
    upGenes <- rownames(x[[de]][x[[de]]$log2FoldChange>0,])
    downGenes <- rownames(x[[de]][x[[de]]$log2FoldChange<0,])
    #commonUp <- intersect(prevUp, upGenes)
    #commonDown <- intersect(prevDown, downGenes)
    upCounts <- ifelse(inverse, length(downGenes), length(upGenes))
    downCounts <- ifelse(inverse, -1*length(upGenes), -1*length(downGenes))
    myDF <<- rbind(myDF, data.frame(time=thisName, counts=upCounts, class=upName))
    myDF <<- rbind(myDF, data.frame(time=thisName, counts=downCounts, class=downName))
    #myDF <<- rbind(myDF, c(thisName, length(downGenes), 'down'))
  })
  
  #genes <- controlDE$W34$log2FoldChange
  #upGenes <- rownames(controlDE$W34[controlDE$W34$log2FoldChange>0,])
  #downGenes <- rownames(controlDE$W34[controlDE$W34$log2FoldChange<0,])
  #commonUp <- intersect(prevUp, upGenes)
  #commonDown <- intersect(prevDown, downGenes)
  
  names(myDF) <- c("time", "counts", "class")
  thisColors <- c(upColor, downColor)
  names(thisColors) <- c(upName, downName)
  
  ggplot(data=myDF, aes(x=time, y=counts, fill=class)) +
    geom_bar(stat="identity") + 
    geom_text(aes(label=abs(counts)),size = 5, position = position_stack(vjust = 0.5))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab(xlabel) + ylab(ylabel) +
    scale_fill_manual(values=thisColors) + 
    scale_y_continuous(labels = abs)
}
