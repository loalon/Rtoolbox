#' Returns an interactive PCA plot.
#'
#' @param data numerical matrix or data frame, raw, normalized or transformed. Samples must be in rows and variables in columns
#' @param meta data frame containing meta data
#' @param group meta data columns to be represented in the plot
#' @param title plot's title
#' @param colors character vector of custom colors
#' @param inverse boolean vector to decide which component plot in reverse, the order is PC1, PC2, PC3
#' @param model plots a "2D" (default) or "3D" version
#'
#' @return plotly object containing plot data
#'
#' @examples 
#' Get the result and store it in an object
#' x <- plotInteractivePCA(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction")
#' 
#' direct plot with custom color
#' plotInteractivePCA(combinedData, metaDF, c('Treatment'), title="Treatment", colors = c("#F8766D", "#00BFC4", "#56B4E9"))
#' 
#' reverse PC1
#' plotInteractivePCA(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction", inverse = c(T,F,F))
#' 
#' 3D version
#' plotInteractivePCA(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction", inverse = c(T,F,F), model="3D")

plotInteractivePCA <- function(data, meta, group, model= c("2D", "3D"), title="", colors = c(),  inverse=c(FALSE,FALSE,FALSE)) {
  
  require(plotly)
  require(RColorBrewer)
  
  # check data
  if(!is.numeric(data))
    stop("\"data\" must be a numerical matrix or numerical data frame")
  
  if(!is.character(group))
    stop("\"group\" must be a character vector")
  
  if(!all(group %in% names(meta)))
    stop("Not all names in \"group\" are column names in \"meta\"")
  
  if(!is.logical(inverse))
    stop("\"inverse\" is not a logical vector")
  
  if(length(inverse) != 3)
    stop("\"inverse\" must contain exactly 3 boolean elements")
  
  if(length(colors) == 0)
    warning("No colors were specified, using default colors")
   
  # argument matching
  model <- match.arg(model)
  
  # Calculate PCA and percents
  pca <- prcomp(data)
  percents <- round(summary(pca)$importance[2,]*100)
  
  # Reorder grouping names according to their unique element size
  if(length(unique(meta[[group[1]]])) < length(unique(meta[[group[2]]])))
    group <- rev(group)
  
  # Create data frame
  df3D <- data.frame(meta[,group, drop=F])
  groupNames <- colnames(df3D)
  
  # Add desired metadata
   if(length(group) == 1) 
     df3D$group <- meta[,group]
   if  (length(group) > 1)
    df3D$group <- apply( df3D[ , group ] , 1 , paste , collapse = "-" )  
   
  # Add pca data
  df3D$PC1 <- pca$x[,1]
  df3D$PC2 <- pca$x[,2]
  df3D$PC3 <- pca$x[,3]
  
  if(inverse[1])
    df3D$PC1 <- df3D$PC1 * -1
  if(inverse[2])
    df3D$PC2 <- df3D$PC2 * -1
  if(inverse[3])
    df3D$PC3 <- df3D$PC3 * -1
  
  customColors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
  
  if(is.null(colors)) {
    uniques <- length(unique(df3D[[groupNames[1]]]))
    if( uniques < 6)
      colors <-  customColors[1:uniques]#RColorBrewer::brewer.pal(uniques, "Set1")
    else
      colors <- rainbow(uniques)
  }
  
  groupName <- paste(group, collapse="-")

  # Create plot object 
  p <- if(model=="2D"){
    plot_ly(df3D, x = ~PC1, y = ~PC2,                 
            color = df3D[[groupNames[1]]],
            colors = colors,
            symbol = df3D[[groupNames[2]]],
            symbols = c("circle","square","cross","x"),
            text = ~paste(groupName, ":", group),
            marker = list(size= 10, opacity = 0.9, sizemode = 'diameter')
    ) %>%
      add_markers() %>%
      layout(title = title,
             scene = list(xaxis = list(title = paste("PC1 (",percents[1],"%)",sep="")),
                          yaxis = list(title = paste("PC2 (",percents[2],"%)",sep=""))
             )
      )
  } else {
    plot_ly(df3D, x = ~PC1, y = ~PC3, z = ~PC2,
            color = df3D[[groupNames[1]]],
            colors=colors,
            symbol = df3D[[groupNames[2]]],
            symbols= c("circle","square","cross","x"),
            text = ~paste(groupName, ":", group)  
    ) %>%
      add_markers() %>%
      layout(title = title,
             scene = list(xaxis = list(title = paste("PC1 (",percents[1],"%)",sep="")),
                          yaxis = list(title = paste("PC3 (",percents[3],"%)",sep="")),
                          zaxis = list(title = paste("PC2 (",percents[2],"%)",sep=""))
             )
      )
  }

  ##TODO fix legend

  # Return plot
  return(p)
}

plotInteracticePCA_test <- function() {
  load("toydata/combinedData.RData")
  plotInteractivePCA(as.matrix(combinedData), metaDF, c('Treatment', 'Week'), title="Treatment-week interaction", inverse = c(T,F,F)) #,c("#F8766D", "#00BFC4", "#56B4E9") )
  plotInteractivePCA(as.matrix(combinedData), metaDF, c( 'Week', 'Treatment'), title="Treatment-week interaction", inverse = c(T,F,F), model="3D") #,c("#F8766D", "#00BFC4", "#56B4E9") )
  plotInteractivePCA(as.matrix(combinedData), metaDF, c('Treatment'), title="Treatment-week interaction", inverse = c(T,F,F),colors=c("#F8766D", "#00BFC4", "#56B4E9") )
  plotInteractivePCA(as.matrix(combinedData), metaDF, c('Treatment'), title="Treatment-week interaction", inverse = c(T,F,F))
  plotInteractivePCA(as.matrix(combinedData), metaDF, c('Treatment'), title="Treatment-week interaction", inverse = c(T,F,F), model="3D")
}


