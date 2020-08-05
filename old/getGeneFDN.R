#' getFDN
#' Returns a dataframe of 1st degree neighbours
#' from an edge list and a collection of genes.
#' It assumes an undirected network.
#'
#' @param edgeList data frame with at least 2 columns
#' 1 with source genes and 1 with target genes. the order
#' is not important
#' @param genes a vector of the genes of interest to look for
#'
#' @return character vector with FDN
#'
#' @examples t <- getFDN(edgeList, c("MA_104203g0010","MA_86195g0010"))
#' write.table(t, file="myFile.tsv", sep='\t', row.names = F, quote = F)
getGeneFDN <- function(edgeList, gene, source.col=1, target.col=2) {
  # TODO check for data type
  # TODO check that all genes are in the edgelist

  s2t <- edgeList[edgeList[source.col] == gene,][,target.col]
  t2s <- edgeList[edgeList[target.col] == gene,][,source.col]
  res <- union(s2t,t2s)
  
  return(res)
}

#toydata
# edgeList <- read.table("toydata/controlEdgeList.tsv", header=T, sep='\t',stringsAsFactors=F)
# bla <- getGeneFDN(edgeList, "C1C_4")
