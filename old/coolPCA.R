 #' Returns an interactive 3D plot.
  #'
  #' @param data numerical matrix or data frame, raw, normalized or transformed. Samples must be in rows and variables in columns
  #' @param meta data frame containing meta data
  #' @param group meta data columns to be represented in the plot
  #' @param title plot's title
  #' @param colors character vector of custom colors
  #' @param inverse boolean vector to decide which component plot in reverse, the order is PC1, PC2, PC3
  #'
  #' @return plotly object containing plot data
  #'
  #' @examples 
#' Get the result and store it in an object
#' x <- plot3Dpca(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction")
#' 
#' direct plot with custom color
#' plot3Dpca(combinedData, metaDF, c('Treatment'), title="Treatment", colors = c("#F8766D", "#00BFC4", "#56B4E9"))
#' 
#' reverse PC1
#' plot3Dpca(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction", inverse = c(T,F,F))


coolPCA <- function(data, meta, group, title="", colors = c(), inverse=c(FALSE,FALSE,FALSE),
                    type='2D') {
  
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
  
  if(length(inverse) != 3)
    stop("\"inverse\" must contain exactly 3 boolean elements")
  
  if(length(colors) == 0)
    warning("No colors were specified, using default colors")
  
  # Calculate PCA and percents
  pca <- prcomp(data)
  percents <- round(summary(pca)$importance[2,]*100)
  
  # Create data frame
  # df3D <- meta[,group]
  # groupNames <- colnames(df3D)
  # print(df3D)
  # Add desired metadata
  # if(length(group) > 2) {
  #  
  #   df3D <- as.data.frame(lapply(df3D, as.character))
  #   colnames(df3D) <- groupNames
  # 
  # } #else {
  
  #}
  # Create data frame
  df3D <- data.frame(meta[,group, drop=F])
  groupNames <- colnames(df3D)
  n <- 0
  # Add desired metadata
   if(length(group) == 1) {
     df3D$group <- meta[,group]
    
   } else {
  if(length(group) > 1)
    df3D$group <- apply( df3D[ , group ] , 1 , paste , collapse = "-" )  
   }
  #groupNames <- colnames(df3D)
  
  # Add pca data
  df3D$PC1 <- pca$x[,1]
  df3D$PC2 <- pca$x[,2]

  if(inverse[1])
    df3D$PC1 <- df3D$PC1 * -1
  if(inverse[2])
    df3D$PC2 <- df3D$PC2 * -1
  if(type=='3D') {
      df3D$PC3 <- pca$x[,3]
    if(inverse[3]) {
      df3D$PC3 <- df3D$PC3 * -1
    }
  }
  thisColors <- NA
  if (isEmpty(colors)) {
    print("Generating colors")
    n <- length(unique(df3D[[groupNames[1]]]))
    hues = seq(15, 375, length = n + 1)
    colors <- hcl(h = hues, l = 65, c = 100)[1:n]
    #thisColors <- setNames(thisColors, unique(df3D[[groupNames[1]]]))
  } 
  print(thisColors)
  # Extract group names to tag the samples
  groupName <- paste(group, collapse="-")
  
  p <- NA
  # Create plot object 
  if(type=="2D") {
    # thisColors <- c("#F8766D", "#E9842C", "#D69100", "#BC9D00", "#9CA700", "#6FB000", "#00B813", "#00BD61",
    #                 "#00C08E" ,"#00C0B4", "#00BDD4", "#00B5EE", "#00A7FF", "#7F96FF", "#BC81FF", "#E26EF7",
    #                  "#F863DF", "#FF62BF" ,"#FF6A9A")
    p <- plot_ly(df3D, x = ~PC1, y = ~PC2, 
                 # colors = c("#F8766D", "#E9842C", "#D69100", "#BC9D00", "#9CA700", "#6FB000", "#00B813", "#00BD61",
                 #            "#00C08E" ,"#00C0B4", "#00BDD4", "#00B5EE", "#00A7FF", "#7F96FF", "#BC81FF", "#E26EF7",
                 #            "#F863DF", "#FF62BF" ,"#FF6A9A"),
                 color = df3D[[groupNames[1]]],
                 symbol = df3D[[groupNames[2]]],
                 symbols= c("circle","square","diamonds","cross","x"),
                 text = ~paste(groupName, ":", group) ,
                 
                 marker = list(size = 10 ),
                 colors=colors
                  ) %>%
      add_markers() %>%
      layout(title = title,
             scene = list(xaxis = list(title = paste("PC1 (",percents[1],"%)",sep="")),
                          yaxis = list(title = paste("PC2 (",percents[2],"%)",sep=""))
                          
             )
    )
  
  } else if (type=="3D") {
    p <- plot_ly(df3D, x = ~PC1, y = ~PC3, z = ~PC2,
                 color = df3D[[groupNames[1]]],
                 symbol = df3D[[groupNames[2]]],
                 symbols= c("circle","square","diamonds","cross","x"),
                 text = ~paste(groupName, ":", group) ,
                 colors=thisColors  ) %>%
      add_markers() %>%
      layout(title = title,
             scene = list(xaxis = list(title = paste("PC1 (",percents[1],"%)",sep="")),
                          yaxis = list(title = paste("PC3 (",percents[3],"%)",sep="")),
                          zaxis = list(title = paste("PC2 (",percents[2],"%)",sep=""))
             )
      )
  } else {
    p <- plot.new()
  }
  
  ##TODO fix legend
  
  # p <- plot_ly(df3D,symbols= c("circle","square","diamonds","cross","x"),colors=colors) %>%
  #   # add_markers(
  #   #   x = ~PC1, 
  #   #   y = ~PC3,
  #   #   z = ~PC2,
  #   #   text = ~paste(groupNames[[1]], ":", df3D[[groupNames[1]]]),
  #   #   )%>%
  
  #   add_markers(
  #             x = ~PC1, 
  #             y = ~PC3,
  #             z = ~PC2,
  #             text = ~paste(groupNames[[2]], ":", df3D[[groupNames[2]]]),
  #             color = df3D[[groupNames[1]]],
  #             symbol = df3D[[groupNames[2]]]) 
  
  
  # Return plot
  return(p)
}

# coolPCA(fullData$expData$combined, mutate_all(fullData$meta$combined, as.character), 
#         c("Week", "Treatment"), type='2D', colors=getColors(19))
# 
# coolPCA(fullData$expData$combined, mutate_all(fullData$meta$combined, as.character), 
#         c("Week"), type='2D')
# 
# coolPCA(fullData$expData$combined, mutate_all(fullData$meta$combined, as.character), 
#         c("Treatment"), type='2D')

#plot3Dpca(fullData$expData$combined, mutate_all(fullData$meta$combined, as.character), c("Week", "Treatment"), colors=getColors(19))
# metaDF <- data.frame(Week=(substr(rownames(combinedData),2,3)), 
# Treatment=substr(rownames(combinedData),5,5))

# plot3Dpca(combinedData, metaDF, c('Treatment', 'Week'), title="Treatment-week interaction", inverse = c(T,F,F)) #,c("#F8766D", "#00BFC4", "#56B4E9") )

# temp$group <- apply( temp[ , names(temp) ] , 1 , paste , collapse = "-" )


