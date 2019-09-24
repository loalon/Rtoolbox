
clusterTreemapColors <- rep(c("#9E1F63","#662D91","#2B3990","#1B75BC","#27AAE1",
                          "#2AB592","#035E31","#009444","#65BC46","#A5CE42",
                          "#F9ED32","#FBB040","#F15A29","#EF4136","#BE1E2D"),5)
clusterTreemapText <- rep(c("white","white","white","white","white",
                        "white","white","white","white","white",
                        "black","black","white","white","white"),5)
plotEnrichedTreemap <- function(x, enrichment = c('go','mapman', 'kegg', 'pfam', 'ko_pathway', 'ko'), 
                                namespace = c('BP','MF','CC', 'none'), title = "", de = "none", 
                                clusterColor = "#9E1F63", clusterText='black') {
  
  require(treemap)
  require(RColorBrewer)
  require(KEGGREST)
  
  diseases <- read.table("~/Rtoolbox/diseases2ko.txt", header=F)
  diseases <- as.character(diseases$V1)
  
  
  enrichment <- match.arg(enrichment)
  namespace <- match.arg(namespace)
  
  #default treemap
  index = c('name')
  palette = "OrRd"
  fontcolor.labels=clusterText
  fontface.labels=c(2)
  fontsize.labels=c(12)
  inflate.labels = TRUE
  align.labels=list(c("center", "center"))
  type = "categorical"
  position.legend = "none"
  vColor = 'name'
  border.col='black'
  border.lwds=c(7,2)
                    
  if(enrichment =='go' ){
    if(namespace=='none') {
      index = c('namespace', 'name')
      palette = "Set1"
      fontcolor.labels=c("#FF000000","black")
      fontface.labels=c(1,2)
      fontsize.labels=c(1,20)
      inflate.labels = TRUE
      align.labels=list(c("left", "top"), c("center", "center")
      )
      type = "index"
      position.legend = "bottom"
      border.col=c('black', 'white')
    } else {
      x[[enrichment]] <- x[[enrichment]][x[[enrichment]]$namespace==namespace,]
      #type = "value"
    }
    
  } else if(enrichment =='mapman') {
    x[[enrichment]]$name <- gsub("\\."," ",x[[enrichment]]$name)
  } else if(enrichment =='ko_pathway') {
    #enr$ko_pathway$name <- keggList(enr$ko_pathway$name)
    if(length(rownames(x[[enrichment]]))>100){ #there is a limit of keggLink querys
      x[[enrichment]] <- x[[enrichment]][1:100,]
    }
    # for (name in x[[enrichment]]$name )
    # if(x[[enrichment]]$name != 'None'){
    #   
    #   x[[enrichment]]$name <- keggList(x[[enrichment]]$name)
    # }
    x[[enrichment]]$name <- ifelse(x[[enrichment]]$name != 'None',  
                                   keggList(x[[enrichment]]$name),'None') 
    x[[enrichment]]$name[(x[[enrichment]]$id %in% diseases)] <- "mis-annotated"
    #printenr$ko_pathway
    #enr$ko_pathway$name
    } else if(enrichment =='ko') {
   
			if(length(rownames(x[[enrichment]]))>100){ #there is a limit of keggLink querys
				x[[enrichment]] <- x[[enrichment]][1:100,]
			}
			x[[enrichment]]$name <- keggList(x[[enrichment]]$name)
			#print(x[[enrichment]])
    
    }
  
  if(de !='none') {
    position.legend = "none"
    vColor = c("size")
    type = "value"
    if(de=='up') {
      palette = "OrRd"
    }
    if(de=='down') {
      palette = "GnBu"
    }
  } 
  ##TODO this has to go somewhere else in order to get different colors for 3 namesacpe treemap
  else {
  palette <- colorRampPalette(c("white", clusterColor))
  palette <- palette(10)
  vColor = c("size")
  type = "value"
  }
    

  
  x[[enrichment]]$size <- abs(log10(x[[enrichment]]$padj))
  
  treemap(x[[enrichment]], 
          index = index,
          vSize = ('size'), 
          palette = palette,
          type = type, 
          vColor = vColor,
          title=title, 
          fontcolor.labels=fontcolor.labels,    # Color of labels
          fontface.labels=fontface.labels,                 # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
          #fontsize.labels=fontsize.labels,
          bg.labels=c("transparent"), 
          inflate.labels = inflate.labels ,
          lowerbound.cex.labels = 0, 
          position.legend = position.legend,
          border.col=border.col,           # Color of borders of groups, of subgroups, of subsubgroups ....
          border.lwds=border.lwds,
          align.labels=align.labels,
          title.legend="legend",
          overlap.labels = 1,
          format.legend = list(scientific = FALSE, big.mark = " ")
  )
}






# plotEnrichedTreemap(cluster2)
# plotEnrichedTreemap(cluster2, clusterColor = 'yellow')
# plotEnrichedTreemap(cluster2, namespace='BP')
# plotEnrichedTreemap(cluster2, de='down')
# plotEnrichedTreemap(cluster2, enrichment = 'mapman')
# plotEnrichedTreemap(cluster2, enrichment = 'mapman', de='up')
# plotEnrichedTreemap(cluster2, enrichment = 'mapman', de='down')
# 
# 
# 
# colfunc <- colorRampPalette(c("#9E1F63", "white"))
# colfunc(10)
# # [1] "#000000" "#1C1C1C" "#383838" "#555555" "#717171" "#8D8D8D" "#AAAAAA"
# # [8] "#C6C6C6" "#E2E2E2" "#FFFFFF"
# 
# plot(rep(1,10),col=colfunc(10),pch=19,cex=3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plotTreemapUpAndDown <- function(x, xlabel="Comparitions", ylabel="Number of genes", 
#                           upName="Up", downName="Down",
#                           upColor="OrRd", downColor="GnBu", lfc= 0.5, padj=0.01,
#                           enrichment='go') {
#   require(ggplot2)
#   require('DESeq2')
#   
#   #if (class(x) != "list")
#    # stop("First parameter must be a list containing at least one DESeq2 result")
#   
#   index = c('name')
#   if(enrichment =='go') {
#     index = c('namespace', 'name')
#   }
#   
#   upGenes <- rownames(x[x$log2FoldChange>filter,])
#   downGenes <- rownames(x[x$log2FoldChange<filter,])
#   
# enr <- gopher(tempUpGenes, task = list('go', 'mapman'), url='pabies')
# enr$go$size <- abs(log10(enr$go$padj))
# 
# treemap(enr$go, 
#         index = c('namespace', 'name'),
#         vSize = ('size'), 
#         palette = "OrRd",
#         type = "categorical", 
#         vColor = 'name', title="", 
#         fontcolor.labels=c("blue","black"),    # Color of labels
#         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
#         fontsize.labels=c(12,12), 
#         bg.labels=c("transparent"), 
#         inflate.labels = FALSE, 
#         lowerbound.cex.labels = 0, position.legend = "none",
#         border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
#         border.lwds=c(7,2),
#         align.labels=list(
#           c("left", "top"), 
#           c("center", "center")
#         )
# )
# 
# })
# 
# cluster1 <- gopher(combinedClusters$Cluster1, task = list('go', 'mapman'), url='pabies')
# cluster2 <- gopher(combinedClusters$Cluster2, task = list('go', 'mapman'), url='pabies')
# cluster3 <- gopher(combinedClusters$Cluster3, task = list('go', 'mapman'), url='pabies')
# cluster1$go$size <- abs(log10(cluster1$go$padj))
# cluster2$go$size <- abs(log10(cluster2$go$padj))
# cluster3$go$size <- abs(log10(cluster3$go$padj))
# 
# treemap(cluster3$go, 
#         index = c('namespace', 'name'),
#         vSize = ('size'), 
#         palette = "Set1",
#         type = "index", 
#         vColor = 'name', title="", 
#         fontcolor.labels=c("blue","black"),    # Color of labels
#         fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
#         fontsize.labels=c(12,12), 
#         bg.labels=c("transparent"), 
#         inflate.labels = FALSE, 
#         lowerbound.cex.labels = 0, position.legend = "none",
#         border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
#         border.lwds=c(7,2),
#         align.labels=list(
#           c("left", "top"), 
#           c("center", "center")
#         )
# )
# 
# cluster1$mapman$size <- abs(log10(cluster1$mapman$padj))
# cluster2$mapman$size <- abs(log10(cluster2$mapman$padj))
# cluster3$mapman$size <- abs(log10(cluster3$mapman$padj))
# cluster1$mapman$name <- gsub("\\."," ",cluster1$mapman$name)
# cluster2$mapman$name <- gsub("\\."," ",cluster2$mapman$name)
# cluster3$mapman$name <- gsub("\\."," ",cluster3$mapman$name)
# treemap(cluster3$mapman, 
#         index = c('name'),
#         vSize = ('size'), 
#         #palette = "Set1",
#         type = "index", 
#         vColor = 'name', title="", 
#         fontcolor.labels=c("black"),    # Color of labels
#         fontface.labels=c(1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
#         fontsize.labels=c(12), 
#         bg.labels=c("transparent"), 
#         inflate.labels = TRUE, 
#         lowerbound.cex.labels = 0, position.legend = "none",
#         border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
#         border.lwds=c(7,2),
#         
# )
