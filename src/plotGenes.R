
#' plotGenes
#'
#' @author Alonso Serrano, \email{alonso.serrano@slu.se}
#' @author Teitur Kalman, \email{teka0012@student.umu.se}
#' @param data 
#' @param genes 
#' @param samplingPoints 
#' @param xlabel 
#' @param ylabel 
#' @param title 
#' @param grid 
#' @param interactive 
#' @param colors 
#' @param legend 
#' @param legendTitle 
#'
#' @return
#' @export
#'
#' @examples
#' plotGenes(controlData, colnames(controlData)[1], substr(rownames(controlData), 2,3) )
#' plotGenes(controlData, colnames(controlData)[45:65], as.numeric(substr(rownames(controlData), 2,3)),interactive = T, legendTitle = "" )
#' 
plotGenes <- function(data, genes, samplingPoints, xlabel = "Time", ylabel = "VST value",
                      title = "", grid = F, interactive = T,
                      colors = c("#009444", "#BE9230"),
                          legend=T,  legendTitle="") {
  require(ggplot2)
  require(RColorBrewer)
  require(reshape2)
  require(plotly)
  
  if (length(genes) < 1 )
    stop("This function needs at least one gene")
  
  
  ## TODO add option to different lines shapes
  
  # subset the data
  d <- data[, genes, drop=FALSE]
  
  # empty plot for future modifications
  myplot <- NA
  
  # create and melt the data frame
  d <- as.data.frame(d)
  d <- as.data.frame(cbind(d, samplingPoints = samplingPoints, sampleID=rownames(d)))
  d <- melt(d, measure.vars = genes)
  
  # create plot with a line per gene with the mean value
  myplot <- ggplot(d, aes(x = samplingPoints, y = value, group = variable, col = variable)) +
    stat_summary(fun.data = mean_se, geom = "line", lwd = 1) +
    theme_bw() +
    labs(color = legendTitle)

  #TODO add orientation
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #TODO check if time is factor and advise the user to manually stablish levels if they are not numeric
  
   if(!grid) {
    myplot <- myplot +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  }

  # remove legend if requested
  if(!legend) {
    myplot <- myplot +
    theme(legend.position = "none")
  }

  # add general stuff
  myplot <- myplot +
    xlab(xlabel) +
    ylab(ylabel) +
    labs(title = title)
  
  # add plotly interactivity
  if (interactive) {
    return (ggplotly(myplot))
  } else {
    return (myplot)
  }
  
}



