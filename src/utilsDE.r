#' Title
#'
#' @param x 
#' @param y 
#' @param digits 
#' @param prefix 
#' @param cex.cor 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


screePlot <- function(pca, number=10) {
  proportionvariances <- ((pca$sdev^2) / (sum(pca$sdev^2)))*100
  barplot(proportionvariances[1:number], cex.names=1, 
          xlab=paste("Principal component (PC), 1-", number), 
          ylab="Proportion of variation (%)", main="Scree plot", ylim=c(0,100))
}

getRes <- function (num, den, localDDS, group) {

  comp <- paste0(num,"vs", den)
  print(paste("Comparition:", comp))
  partialRes <- results(localDDS,
                        contrast= c(group, num ,den),
                        filter = rowMedians(counts(localDDS)),
                        parallel = FALSE)
  #partialRes <- sigDeg(partialRes)
  #partialRes <- partialRes[!is.na(partialRes$padj),]
  #partialRes <- partialRes[partialRes$padj < padj,]
  #partialRes <- partialRes[order(partialRes$padj),]
  #write.table(partialRes, file=paste0("Fertilised.fix_", x, "vs", y, "_median_fold_0.5_Padj_0.01.tsv"), sep = "\t")
  # partialRes
  #partialRes <- "bla"
  #names(partialRes) <- comp
  partialRes
}

filterDE <- function(res, p = 0.01, log2fc = 0.5, genes = "all") {
  
  if (class(res) != "DESeqResults")
    stop("First parameter, must me a DESeqResults object")
  
  print(paste("Filtering by:","padj =" ,p,"and lfc=",log2fc ))
  #partialRes <- res[!is.na(res$padj),]
  #partialRes <- partialRes[partialRes$padj < p,]
  #print(length(rownames(res)))
  partialRes <- res[!is.na(res$padj) & res$padj<p,]
  #print("here")
  #print(length(rownames(partialRes)))
  partialRes <- partialRes[abs(partialRes$log2FoldChange)>log2fc,]
  #print(partialRes)
  #print(length(rownames(partialRes)))
  partialRes <- partialRes[order(partialRes$padj),]
  #print(length(rownames(partialRes)))
  return (partialRes)
}


##TODO, explain that this works for a condition vs control, if is reverse, 
#user needs to inverse=T
plotUpAndDown <- function(x, xlabel="Comparitions", ylabel="Number of genes", 
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
  ##TODO, check for namesm otherwise it doesn't work

  lapply(1:length(x), function(de){
    
    thisName <- names(x[de])
    print(thisName)
    upGenes <- rownames(x[[de]][x[[de]]$log2FoldChange>0,])
    downGenes <- rownames(x[[de]][x[[de]]$log2FoldChange<0,])
    #commonUp <- intersect(prevUp, upGenes)
    #commonDown <- intersect(prevDown, downGenes)
    upCounts <- ifelse(inverse, length(downGenes), length(upGenes))
    downCounts <- ifelse(inverse, -1*length(upGenes), -1*length(downGenes))
		print(upCounts)
		print(downCounts)
    myDF <<- rbind(myDF, data.frame(time=thisName, counts=upCounts, class=upName))
    myDF <<- rbind(myDF, data.frame(time=thisName, counts=downCounts, class=downName))
    #myDF <<- rbind(myDF, c(thisName, length(downGenes), 'down'))
    #names(myDF) <- c("time", "counts", "class")
    #print(myDF)
  })
  
  #genes <- controlDE$W34$log2FoldChange
  #upGenes <- rownames(controlDE$W34[controlDE$W34$log2FoldChange>0,])
  #downGenes <- rownames(controlDE$W34[controlDE$W34$log2FoldChange<0,])
  #commonUp <- intersect(prevUp, upGenes)
  #commonDown <- intersect(prevDown, downGenes)
  #myDF <- rbind(myDF, c('W34', length(upGenes), -1*length(downGenes)))
  
  names(myDF) <- c("time", "counts", "class")

  ggplot(data=myDF, aes(x=time, y=counts, fill=class)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    xlab(xlabel) + ylab(ylabel) +
    scale_fill_manual(values=c(upColor, downColor))
}



plotTop <- function(res, dds, plotGroup=c(""), infoVector, name) {
  require('DESeq2')
  pdf(name) 
  side <- ceiling(sqrt(length(names(res))))
  par(mfrow=c(2,3))
  for (pair in names(res)){
    partialRes <- res[[pair]][order(res[[pair]]$padj),]
    partialRes <- partialRes[order(partialRes$padj),]
    #print(length(rownames(partialRes)))
    numberOfGenes <- length(rownames(partialRes))
    if(numberOfGenes > 0) {
      numberOfGenes <- ifelse(numberOfGenes >=6, 6, numberOfGenes)
      for(i in 1:numberOfGenes){
        topGene <- rownames(partialRes[i,])
        elements <- unlist(strsplit(pair, "vs"))
        line1 <- grep(elements[1],infoVector)
        line2 <- grep(elements[2],infoVector)
        plotCounts(dds, topGene, plotGroup, main=paste(pair,"-",topGene))
        abline(v=line1, col='blue')
        abline(v=line2, col='red')
      }
    }
  }
  dev.off()
}

de4network <- function(de.res, file, lfc=0.5, padj=0.01) {
  df <- data.frame(genes = rownames(de.res[[1]]) )
  lapply(names(de.res), function(x){
    lfc <- paste0(x, "_lfc")
    padj <- paste0(x, "_padj")
    print(lfc)
    print(padj)
    
    x1 <- de.res[[x]]$log2FoldChange #obtain fold change
    #x1 <- ifelse(abs(x1)>lfc, x1, 0) #if below threshold, change val to 0
    
    x2 <- de.res[[x]]$padj #obtain padj
    x2 <- ifelse(is.na(x2), 1, x2)  #NA change to 1
    #x2 <- ifelse(x2<0.01, ceiling(1/x2), 0) #scale to positives
    #x2 <- ifelse(x2==0, 1, x2) 
    
    df[[lfc]] <<- x1
    df[[padj]] <<- x2 
    print(all(df$genes == rownames(de.res[[x]])))
    
  })
  write.table(df, file=file, sep="\t", row.names=F, quote=F)
  return (df)
}


