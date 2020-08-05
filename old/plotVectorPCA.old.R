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
plotVectorPCA <- function(pcadata, timeVector, 
                          conditionVector, title="", timeUnits="Weeks") {
  
  require('ggplot2')
  require(dplyr)
  
  if ( !( length(rownames(pcadata$x)) == length(timeVector) && 
          length(conditionVector) == length(timeVector)) ) { 
    stop("time vector, condition vector and samples sizes from PCA are different.")
  }

  percent <- round(summary(pcadata)$importance[2,]*100)
  comps <- as.data.frame(pcadata$x[, 1:2])
  comps$Time <- timeVector
  comps$Condition <- conditionVector

  df <- comps %>% group_by(Time, Condition) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2))
  
  df <- do.call(rbind, lapply(split(as.data.frame(df), df$Time), function(f){
    data.frame(xstart = f[1,3], xend = f[2,3], ystart = f[1,4], yend = f[2,4],
               Time = f[1,1])
  }))
  
  ggplot(df, aes(x = xstart, xend = xend, 
                 y = ystart, yend = yend, col = Time)) +
    geom_segment(arrow = arrow(length = unit(0.5, "cm")), size = 2) + 
    labs(color = timeUnits) +
    theme_bw() + theme(text = element_text(size = 20)) + 
    xlab(paste("PC1 (",percent[1],"%)",sep="")) + 
    ylab(paste("PC2 (",percent[2],"%)",sep="")) + 
    ggtitle(paste("Principal Component Vector Analysis ", title))
  
}

