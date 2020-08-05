#' Returns an interactive 2D plot.
#'
#' @param data numerical matrix or data frame, raw, normalized or transformed. Samples must be in rows and variables in columns
#' @param meta data frame containing meta data
#' @param group meta data columns to be represented in the plot
#' @param title plot's title
#' @param colors character vector of custom colors
#' @param inverse boolean vector to decide which component plot in reverse, the order is PC1, PC2
#'
#' @return plotly object containing plot data
#'
#' @examples 
#' Get the result and store it in an object
#' x <- plot2Dpca(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction")
#' 
#' direct plot with custom color
#' plot2Dpca(combinedData, metaDF, c('Treatment'), title="Treatment", colors = c("#F8766D", "#00BFC4", "#56B4E9"))
#' 
#' reverse PC1
#' plot2Dpca(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction", inverse = c(T,F,F))


plot2Dpca <- function(data, meta, group, title="", colors = c(), inverse=c(FALSE,FALSE)) {
  
  require(plotly)
  
  # check data
  if(!is.numeric(data))
    stop("\"data\" must be a numerical matrix or numerical data frame")
  
  if(!is.character(group))
    stop("\"group\" must be a character vector")
  
  if(!all(group %in% names(meta)))
    stop("Not all names in \"group\" are column names in \"meta\"")
  
  if(!is.logical(inverse))
    stop("\"inverse\" is not a logical vector")
  
  if(length(inverse) != 2)
    stop("\"inverse\" must contain exactly 2 boolean elements")
  
  if(length(colors) == 0)
    warning("No colors were specified, using default colors")
  
  # Calculate PCA and percents
  pca <- prcomp(data)
  percents <- round(summary(pca)$importance[2,]*100)
  

  df2D <- data.frame(meta[,group, drop=F])
  groupNames <- colnames(df2D)

  if(length(group) > 1) {
    df2D$group <- apply( df2D[ , group ] , 1 , paste , collapse = "-" )  
  } else{
    df2D$group <- df2D[ , group ]
  }
  
  df2D$sampleName <- rownames(data)
  
  # Add pca data
  df2D$PC1 <- pca$x[,1]
  df2D$PC2 <- pca$x[,2]

  
  if(inverse[1])
    df2D$PC1 <- df2D$PC1 * -1
  if(inverse[2])
    df2D$PC2 <- df2D$PC2 * -1

  
  # Extract group names to tag the samples
  groupName <- paste(group, collapse="-")
  print(colors)
  
  # Create plot object 
  p <- plot_ly(df2D, x = ~PC1, y = ~PC2,
               color = df2D[[groupNames[1]]],
               symbol = df2D[[groupNames[2]]],
               symbols= c("circle","square","diamond","cross","x"),
               text = ~paste(groupName, ":", group, "|", "Sample:", sampleName) ,
               colors=colors  ) %>%
    add_markers() %>%
    layout(title = title,
           xaxis = list(title = paste("PC1 (",percents[1],"%)",sep="")),
                        yaxis = list(title = paste("PC2 (",percents[2],"%)",sep=""))
           
    )

  return(p)
}

