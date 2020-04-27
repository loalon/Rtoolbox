
clusterTreemapColors <- rep(c("#9E1F63","#662D91","#2B3990","#1B75BC","#27AAE1",
                          "#2AB592","#035E31","#009444","#65BC46","#A5CE42",
                          "#F9ED32","#FBB040","#F15A29","#EF4136","#BE1E2D"),15)
clusterTreemapText <- rep(c("white","white","white","white","white",
                        "white","white","white","white","white",
                        "black","black","white","white","white"),15)

plotEnrichedTreemap <- function(x, enrichment = c('go','mapman', 'kegg', 'pfam', 'ko_pathway', 'ko', 'kog','cog'), 
                                namespace = c('BP','MF','CC', 'none'), title = "", de = "none", 
                                clusterColor = "#9E1F63", clusterText='black') {
  
  require(treemap)
  require(RColorBrewer)
  require(KEGGREST)
  
  enrData <- x
  
  
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
  } else if (enrichment != 'go') {
    palette <- colorRampPalette(c("white", clusterColor))
    palette <- palette(10)
    vColor = c("size")
    type = "value"
  }
    

  # calculate the size based on padj
  x[[enrichment]]$size <- abs(log10(x[[enrichment]]$padj))
  
  # generate treemap
  treemap(x[[enrichment]], 
          index = index,
          vSize = ('size'), 
          palette = palette,
          type = type, 
          vColor = vColor,
          title=title, 
          fontcolor.labels=fontcolor.labels, 
          fontface.labels=fontface.labels,     
          #fontsize.labels=fontsize.labels,
          bg.labels=c("transparent"), 
          inflate.labels = inflate.labels ,
          lowerbound.cex.labels = 0, 
          position.legend = position.legend,
          border.col=border.col,         
          border.lwds=border.lwds,
          align.labels=align.labels,
          title.legend="legend",
          overlap.labels = 1,
          format.legend = list(scientific = FALSE, big.mark = " ")
  )
}
