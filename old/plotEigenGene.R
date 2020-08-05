plotEigengene <- function(data, genes, condition, time, timeUnits = "Time",
                          inverse = F, title = "", noGrid = T, colors = c("#009444", "#BE9230"),
                          noLegend=T, multiline = F, legendTitle="",
                          plotBothProfiles=F) {
  require(ggplot2)
  require(RColorBrewer)
  require(reshape2)
  #require(plotly)
  #print((unique(condition)))
  
  if(plotBothProfiles) {
    if(length(unique(condition)) > 1)
      stop("Plotting both expression profiles can only be done with a one condition dataset, e.g. Control.")
    if(multiline)
      stop("Can't plot both expression profiles in multiline mode.")
    if(length(genes) == 1 )
      stop("Can't plot both expression profiles with only one gene.")
    if(inverse)
      stop("Can't plot both expression profiles with the inverse parameter activated.")
  }
  
  if (length(genes) < 1 )
    stop("This function needs at least one gene")
  
  
  ## add option to different lines shapes
  
  ## maybe not scale, or scale optional
  expr <- NA

  #d <- data[,which(colnames(data) %in% gene)]
  d <- data[, genes]
  
  
  if (length(genes) == 1 ) { #1 gene, directly the data
    expr <- scale(d)
 
  } else if (length(genes > 1)) { #cluster, eigengene
    pca <- prcomp((d))
    pc1 <- pca$x[, 1]
    #print(percents <- round(summary(pca)$importance[2,]*100))
    
    if(sum(sign(cor(pca$x[,1,drop = FALSE], d))) < 0) {
      pc1 <- pc1 * -1
    }
    if(inverse && !plotBothProfiles)
      pc1 <- pc1 * -1


    expr <- pc1

  } 
 
  myplot <- NA
  
  if(multiline && length(genes) > 1 ) {
    d <- as.data.frame(scale(d))
    d <- as.data.frame(cbind(d, time = time, sampleID=rownames(d)))
    d <- melt(d, measure.vars = genes)

    myplot <- ggplot(d, aes(x = time, y = value, group = variable, col = variable)) +
      stat_summary(fun.data = mean_se, geom = "line", lwd = 1) +
      theme_bw()
    
  } else {
    myplot <- ggplot(data.frame(x = time, y = scale(expr), g = condition),
                     aes(x = x, y = y, group = g)) +
      stat_summary(fun.data = mean_se, geom = "ribbon", fill = "lightgrey", alpha = 0.75) +
      stat_summary(fun.data = mean_se, geom = "line", aes(col = g), lwd = 2) + #      plot_output_list <- lapply(shiftedColors, function(color) {

      labs(color = legendTitle)

  }
  
  if(plotBothProfiles) {
    
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
    
    #print(length(positive))
    #print(length(negative))
    ratio <- round(length(negative)/(length(positive)+length(negative)) ,digits=2 )
    #print(ratio)
    mData <- data.frame(PC1 = expr, PC1i = expr * -1)
    #mData$PC1i <- expr * -1
    
    mData <- melt(mData, measure.vars = c("PC1", "PC1i"))
    mData$time <- time
    #mData$Treatment <- type
    #print(mData)
    
    myplot <- ggplot(mData, aes(x = time, y =scale(value), group = variable, col = variable, alpha=variable)) + 
      #stat_summary(fun.data = mean_se, alpha = 0.25, geom = "ribbon", col = "grey90") +
      
      stat_summary(fun.data = mean_se, lwd = 2, geom = "line") + 
      scale_alpha_manual(values=c(1,ratio)) 
    #scale_alpha_discrete(range = c(0.35, 0.9))
      #theme_bw() +
      #facet_wrap(~Treatment, nrow = 2, scales = "free_y") +
      #theme(text = element_text(size = 10)) +
      #scale_color_discrete(name = "")
   
  }

  
  
  # 
  # p <- colnames(embryoData)[1:3]
  # d <- as.data.frame(scale((embryoData[,p])))
  # #colnames(d) <- p
  # meta <- 
  #   d <- as.data.frame(cbind(d, Week = substr(rownames(embryoData),4,4)))
  # d <- melt(d, measure.vars = p)
  # d$Week <- as.integer(d$Week)
  # 
  # ggplot(d, aes(x = Week, y = value, group = variable, col = variable)) +
  #   stat_summary(fun.data = mean_se, geom = "line", lwd = 1) +
  #   #facet_wrap(~Treatment, nrow = 2, scales = "free_y") + 
  #   theme_bw() +
  #   ylab("Relative expression") +
  #   theme(text = element_text(size = 10))
  
  

  # ggplot(d, aes(x = time, y = value, group = variable, col = variable)) +
  #   stat_summary(fun.data = mean_se, geom = "line", lwd = 1) +
  #   #facet_wrap(~Treatment, nrow = 2, scales = "free_y") + 
  #   theme_bw() +
  #   ylab("Relative expression") +
  #   theme(text = element_text(size = 10))
  




  #TODO add orientation
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #TODO check if time is factor and advise the user to manually stablish levels if they are not numeric
  
  if(noGrid) {
    myplot <- myplot +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  }
  
  if(multiline == F && is.vector(colors) && length(colors) >= length(unique(condition))) {
    myplot <- myplot + scale_color_manual(values=colors)
  }
  
  if(noLegend) {
    myplot <- myplot +
    theme(legend.position = "none")
  }

  ##add general stuff
  myplot <- myplot +
    xlab(timeUnits) +
    ylab("z-score") +
    #ggtitle(title)
    labs(title = title)
  
  return (myplot)
  
}



