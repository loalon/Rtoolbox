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
#' @return data frame with 2 columns, FDN (first degree neighbours)
#' and GOI (genes of interest)
#'
#' @examples t <- getFDN(edgeList, c("MA_104203g0010","MA_86195g0010"))
#' write.table(t, file="myFile.tsv", sep='\t', row.names = F, quote = F)
getFDN <- function(edgeList, genes) {
  # TODO check for data type
  # TODO check that all genes are in the edgelist
  res <- lapply(genes, function(gene){
    s2t <- edgeList[edgeList[1] == gene,][2]
    t2s <- edgeList[edgeList[2] == gene,][1]
     union(s2t,t2s)
    
  })
  names(res) <- genes
  res <- setNames(unlist(res, use.names=F),rep(names(res), lengths(res)))
  myRes <- data.frame(FDN=res, GOI=names(res))
}

#toydata
# edgeList <- read.table("toydata/controlEdgeList.tsv", header=T, sep='\t')
# bla <- getFDN(edgeList, "C1C_4")
