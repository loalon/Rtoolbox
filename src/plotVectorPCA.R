#' Title
#' Renders a vector plot, using a time series and a binary condition
#' calculates the average point for all replicas on each condition and
#' draws an arrow.
#' the arrow end is the lower letter in alphabetical order, 
#' the arrow tip is the upper letter in alphabetical order,
#' e.g. With conditions control and fertilised
#'  control ---------> fertilised
#'  
#' @param pcadata data from a prcomp execution
#' @param timeVector vector with time points of each sample
#' @param conditionVector vector with condition class of each sample
#' @param title optional title for the plot
#' @param timeUnits optional time units for legend name
#'
#' @return
#' @export
#'
#' @examples plotVectorPCA(pca, weeks, treatment, "myResults")
#' 

plotVectorPCA <- function(data, meta, group, 
                          title="Principal Component Vector Analysis ", 
                          colorMap = c(),
                          linetype="solid") {

  # plotVectorPCA <- function(pcadata, timeVector, 
  #                         conditionVector, title="", timeUnits="Weeks") {
  
  require('ggplot2')
  require(dplyr)
  
  #if (length(group)> 1) {
    
  #}
  #
  # if ( !( length(rownames(pcadata$x)) == length(timeVector) && 
  #         length(conditionVector) == length(timeVector)) ) { 
  #   stop("time vector, condition vector and samples sizes from PCA are different.")
  # }
  # 
  pca <- prcomp(data)
  percents <- round(summary(pca)$importance[2,]*100)
  
  #df3D$group <- meta[,group]
  comps <- as.data.frame(pca$x[, 1:2])
  #comps$Time <- timeVector
  comps$binaryCondition <- meta[,group[1]]
  comps$secondaryCondition <- meta[,group[2]]
  print(comps)

  df <- comps %>% group_by(secondaryCondition, binaryCondition) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2))
  
  df <- do.call(rbind, lapply(split(as.data.frame(df), df$secondaryCondition), function(f){
    data.frame(xstart = f[1,3], xend = f[2,3], ystart = f[1,4], yend = f[2,4],
               secondaryCondition = f[1,1])
  }))
  
  ggplot(df, aes(x = xstart, xend = xend, 
                 y = ystart, yend = yend, col = secondaryCondition)) +
    geom_segment(arrow = arrow(length = unit(0.5, "cm")), size = 2, linetype=linetype) + 
    labs(color = group[2]) +
    scale_color_manual(values=colorMap) + 
    theme_bw() + theme(text = element_text(size = 20)) + 
    xlab(paste("PC1 (",percents[1],"%)",sep="")) + 
    ylab(paste("PC2 (",percents[2],"%)",sep="")) + 
    ggtitle(title)
  
}

