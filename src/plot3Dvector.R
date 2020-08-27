plot3DVectorPCA <- function(pcadata, timeVector, 
                          conditionVector, title="", timeUnits="Weeks", inverse=FALSE) {
  
  require('ggplot2')
  require(dplyr)
  require(plotly)
  
  # Renders a vector plot, using a time series and a binary condition
  # calculates the average point for all replicas on each condition and
  # draws an arrow.
  # the arrow end is the lower letter in alphabetical order, 
  # the arrow tip is the upper letter in alphabetical order,
  # e.g. With conditions control and fertilised
  #  control ---------> fertilised
  #
  # Args:
  #   pcadata: data from a prcomp execution
  #   timeVector: vector with time points of each sample
  #   conditionVector: vector with condition class of each sample
  #   title: optional title for the plot
  #   timeUnits: optional time units for legend name
  #
  # Returns:
  #   
  # Use:
  #   plotVectorPCA(pca, weeks, treatment, "myResults")
  
  #myColors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  
  myColors =  c(
    
    "#e6e9bd",
    "#b4d7bc",
    "#7dc6bb",
    "#1cb7ba",
    "#00a7b9",
    "#008b9a",
    "#849fba",
    "#a181a6",
    "#c76b97",
    "#ee2e82",
    "#8e5766",
    "#8a7b73",
    "#869b7f",
    "#42885e",
    "#1aa64a",
    "#75b443",
    "#9bbe3b",
    "#e1d51d",
    "#ffe200"
    
  )
  
  if ( !( length(rownames(pcadata$x)) == length(timeVector) && 
          length(conditionVector) == length(timeVector)) ) { 
    stop("time vector, condition vector and samples sizes from PCA are different.")
  }
  
  percent <- round(summary(pcadata)$importance[2,]*100)
  comps <- as.data.frame(pcadata$x[, 1:3])
  if(inverse)
    comps <- comps *-1
  
  comps$Time <- timeVector
  comps$Condition <- conditionVector
  
  df <- comps %>% group_by(Time, Condition) %>% summarise(PC1 = mean(PC1), PC2 = mean(PC2), PC3 = mean(PC3))
  #print(df)
  
  df <- do.call(rbind, lapply(split(as.data.frame(df), df$Time), function(f){
    data.frame(xstart = f[1,3], xend = f[2,3], ystart = f[1,4], yend = f[2,4],
               zstart = f[1,5], zend = f[2,5],
               Time = f[1,1])
  }))
  #print(df)
  # ggplot(df, aes(x = xstart, xend = xend, 
  #                y = ystart, yend = yend, col = Time)) +
  #   geom_segment(arrow = arrow(length = unit(0.5, "cm")), size = 2) + 
  #   labs(color = timeUnits) +
  #   theme_bw() + theme(text = element_text(size = 20)) + 
  #   xlab(paste("PC1 (",percent[1],"%)",sep="")) + 
  #   ylab(paste("PC2 (",percent[2],"%)",sep="")) + 
  #   ggtitle(paste("Principal Component Vector Analysis ", title))
  
  p <- plot_ly(x=df$xstart, y=df$ystart, z=df$zstart) %>%  
    add_trace(marker = list( color="#009444", symbol = 200, size = 1, line = list( color = toRGB("yellow"), width = 2 ) ) 
            , type = "scatter3d", mode = "text+markers", 
            name = "Control", linetypes = NULL, text = "C") %>%
    
    add_trace(x = df$xend, y = df$yend, z = df$zend, 
              marker = list( color="#BE9230", symbol = 'cross', size = 10, line = list( color = toRGB("yellow"), width = 2 ) ) , 
              type = "scatter3d", mode = "text+markers", 
              name = "Fertilised", linetypes = NULL, text = "F") #%>%
    
    for (i in 1:length(rownames(df))) {
      p <- p %>% add_trace(x=c(df$xstart[i], df$xend[i]), y=c(df$ystart[i], df$yend[i]), z=c(df$zstart[i], df$zend[i]),
                type="scatter3d", mode="lines", name=rownames(df)[i],
                line = list(width=8, color=myColors[i]),
                opacity = 1) 
    }
    # add_trace(x=c(df$xstart[1], df$xend[1]), y=c(df$ystart[1], df$yend[1]), z=c(df$zstart[1], df$zend[1]),
    #           type="scatter3d", mode="lines", name="Week 19",
    #           line = list(width=8, color=c("#BE9230" )),
    #           opacity = 1)  %>%
    
    p <- p %>% layout(title = title,
      scene = list(xaxis = list(title = paste("PC1 (",percent[1],"%)",sep="")),
                        yaxis = list(title = paste("PC2 (",percent[2],"%)",sep="")),
                        zaxis = list(title = paste("PC3 (",percent[3],"%)",sep="")))) %>%
      #layout(scene= list(camera = list(eye = list(x = 0, y = 0.0001, z = 2),
      layout(scene= list(camera = list(eye = list(x = 0, y = 0.0001, z = 3),
                                       up= list(x=1,y=0,z=1),
                                       center= list(x=0,y=0,z=0)))) #%>%
     # layout(autosize = F, width = 500, height = 500, margin = list(l = 50,r = 50,b = 100,t = 100,pad = 4))
   # print(p)
  return (p)
}
#plotVectorPCA(pca, meta$Sampling.date..week.., meta$Treatment)
#pl <-plot3DVectorPCA(pca, meta$Sampling.date..week.., meta$Treatment, title = "Spruce roots 3D vector PCA", inverse = T)
# 
# pl <- plot3DVectorPCA(pca, meta$Week, meta$Treatment, title = "Spruce roots 3D vector PCA")
# 
# library(widgetframe)
# htmlwidgets::saveWidget(frameableWidget(pl),'fungi.html', selfcontained = F)
# 
# #save stuff
# htmlwidgets::saveWidget(pl, "test.html")
# htmlwidgets::saveWidget(pl, "test4.html", 
#                         selfcontained = F,   
#                         sizingPolicy = htmlwidgets::sizingPolicy(
#   viewer.padding = 0,
#   viewer.paneHeight = 500,
#   browser.fill = TRUE
# ))
# #plot3DVectorPCA(pca, meta$Sampling.date..week.., meta$Treatment)
# 
