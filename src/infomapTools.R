

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
prepareInfomapTable <- function(df) {
  
  require(data.table)
  # calculate the number of levels, total columns -3 info columns -1 path column
  numberLevels <- length(colnames(df)) - 4
  
  # create names for the levels
  levels <- sapply(1:numberLevels, function(x){
    paste0("Level", x)
  })
  
  # add the names to the data frame
  colnames(df) <- c('Path', levels, 'Flow', 'Index', 'Gene')
  
  # create a new results data frame
  res <- data.frame(Gene = df[,length(colnames(df))],
                    Path= df[,1],
                    stringsAsFactors = F)
  
  
  # Data always have level 1 and none of the data is NA
  res$Level1 <- df[,2]

  # loop though the rest of the levels and attach the name from the previous ones
  for (level in 2:numberLevels) {
   
    currentLevel <- paste0("Level",level)
    prevLevel <- paste0("Level",(level-1))
    
    # join names
    res[[currentLevel]] <- paste0(res[[prevLevel]], ":", df[[currentLevel]])
    
    # if there is an NA inside the current name, that gene doesn't belong to a cluster in that level, it turns into NA
    res[[currentLevel]] <- ifelse(res[[currentLevel]] %like% "NA", NA, res[[currentLevel]])  
  }
  return (res)
  
}



clusterQA <- function(df, level='Level1') {
  freq1 <- as.data.frame(table(df[level]) )
  freq1$Var1 <- names(table(df[level]) )
  counts <-freq1[order(freq1$Freq, decreasing = T),]
  #how many genes in the first 20 clusters
  
  clusters <- length(unique(freq1$Var1))
  print(paste(level,"Clusters:", clusters))
  
  genes20 <- 0
  if (clusters < 20) {
    genes20 <- sum(counts[1:clusters,2])
  } else {
    genes20 <- sum(counts[1:20,2])  
  }
  
  genesTotal <- length(rownames(df))
  
  #print(level)
  print(paste("Genes in the top 20 clusters", genes20))
  print(paste("Genes in the network",genesTotal))
  
  return (round(genes20/genesTotal, digits = 4)*100)
}

getCountsPerCluster <- function(df, level='Level1'){
  freq1 <- as.data.frame(table(df[level]) )
  freq1$Var1 <- names(table(df[level]) )
  counts <- freq1[order(freq1$Freq, decreasing = T),]
  return(counts)
}

getClusterByMinSize <- function(df, level='Level1', min = 30) {
  # freq1 <- as.data.frame(table(df[level]) )
  # freq1$Var1 <- names(table(df[level]) )
  # counts <-freq1[order(freq1$Freq, decreasing = T),]
  counts <- getCountsPerCluster(df, level=level)
  numberClusters <- length(counts[counts$Freq>min, 2])
  print(paste("Number of clusters obtained:", numberClusters))
  return(numberClusters)
}

getClusters <- function(df, level='Level1', numberOfClusters=30) {
  counts <- getCountsPerCluster(df, level=level)
  clusters <- lapply(counts$Var1[1:numberOfClusters], function(x){
    as.character(df$gene[df[level] == x])
  })
  names(clusters) <- paste0(rep("Cluster",length(clusters)), counts$Var1[1:length(clusters)])
  return (clusters)
}

enrichClusters <- function(clusterList, 
                           task = list('go', 'pfam', 'kegg', 'mapman'), 
                           background=NULL, url='pabies',alpha= 0.05){
	require(devtools)
  source_url("https://raw.githubusercontent.com/UPSCb/UPSCb-common/master/src/R/gopher.R")
  enr <- lapply(clusterList, function(x) {
    print(length(x))
    if(length(x) > 1)
      gopher(x, task = task, background = background, url=url, alpha = alpha)
    else
      NULL
  })
  
  return(enr)
}

save4Cytoscape <- function(clusterList, file="infomapClusters.tsv"){
  fileConn<-file(file,"w")
  writeLines(paste0("gene","\t","cluster"), fileConn)
  
  clusterNames <- names(clusterList)
  for (i in 1:length(clusterNames)) {
    thisCluster <- clusterNames[i]
    print (thisCluster)
    for (gene in clusterList[i]) {
      writeLines(paste0(gene,"\t",thisCluster), fileConn)
    }
  }
  # for(cluster in clusterList) {
  #   print(names(cluster))
  #   # for (gene in cluster){
  #   #   #writeLines(output, fileConn)
  #   #   print(gene)
  #   # }
  #   # 
  # }
  
  close(fileConn)
}

enr2xls <- function(enrList, filePrefix="") {
  require(openxlsx) 
  
  for (x in names(enrList)) {
    filename <- paste0(filePrefix,"_", x, ".xlsx")
    print(filename)
    for(name in names(enrList[[x]])) {
      
      if(!is.null(enrList[[x]][[name]])) {
        write.xlsx(data.frame(enrList[[x]][[name]]), file=filename, sheetName=name, append=TRUE, row.names=FALSE)  
        print(name)
      }
    }
    # 
    # if(!is.null(enrList[[x]]$go))
    #   write.xlsx(data.frame(enrList[[x]]$go),     file=filename, sheetName="GO", append=T, row.names=FALSE)
    # if(!is.null(enrList[[x]]$mapman))
    #   write.xlsx(data.frame(enrList[[x]]$mapman), file=filename, sheetName="Mapman", append=TRUE, row.names=FALSE)
    # if(!is.null(enrList[[x]]$kegg))
    #   write.xlsx(data.frame(enrList[[x]]$kegg),   file=filename, sheetName="KEGG", append=TRUE, row.names=FALSE)
    # if(!is.null(enrList[[x]]$pfam))
    #   write.xlsx(data.frame(enrList[[x]]$pfam),   file=filename, sheetName="Pfam", append=TRUE, row.names=FALSE)
    # if(!is.null(enrList[[x]]$pfam))
    #   write.xlsx(data.frame(enrList[[x]]$pfam),   file=filename, sheetName="Pfam", append=TRUE, row.names=FALSE)
  }
}

enr2tsv <- function(enrList, filePrefix="") {
  #require(openxlsx) 
  
  for (x in names(enrList)) {
    # filename <- paste0(filePrefix,"_", x, ".tsv")
    # print(filename)
    #print(x)
    if(!is.null(enrList[[x]]$go))
      write.table(enrList[[x]]$go, file=paste0(filePrefix,"_", x, "_go",".tsv"), row.names=F, sep='\t', quote = F)
      
     if(!is.null(enrList[[x]]$mapman))
       write.table(enrList[[x]]$mapman, file=paste0(filePrefix,"_", x, "_mapman",".tsv"), row.names=F, sep='\t', quote = F)
    #   write.xlsx(data.frame(enrList[[x]]$mapman), file=filename, sheetName="Mapman", append=TRUE, row.names=FALSE)
     if(!is.null(enrList[[x]]$kegg))
       write.table(enrList[[x]]$kegg, file=paste0(filePrefix,"_", x, "_kegg",".tsv"), row.names=F, sep='\t', quote = F)
    #   write.xlsx(data.frame(enrList[[x]]$kegg),   file=filename, sheetName="KEGG", append=TRUE, row.names=FALSE)
     if(!is.null(enrList[[x]]$pfam))
       write.table(enrList[[x]]$pfam, file=paste0(filePrefix,"_", x, "_pfam",".tsv"), row.names=F, sep='\t', quote = F)
    #   write.xlsx(data.frame(enrList[[x]]$pfam),   file=filename, sheetName="Pfam", append=TRUE, row.names=FALSE)
  }
}


## keep this for compatibility 
prepareData <- function(df) {
  #calculate the number of levels, total columns -3 info columns -1 path column
  totalCols <- length(colnames(df))
  numberLevels <- length(colnames(df)) - 4
  
  #always a first level is going to be there
  #df$Level1 <- df[2]
  
  levels <- sapply(1:numberLevels, function(x){
    paste0("P", x)
  })
  
  daNames <- c('path', levels, 'number', 'index', 'gene')
  colnames(df) <- daNames
  # for (level in 2:numberLevels) {
  #   print(level)
  #   name <- paste0("Level",level)
  #   lastCol <- length(colnames(df))
  #   
  #   df[lastCol+1] <- paste0(df[lastCol], ":", df[level+1])
  #   
  #   #df[[level]]
  # }
  return (df)
  
}
