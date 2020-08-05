##TODO, explain that this works for a condition vs control, if is reverse, 
#user needs to inverse=T
plotInteractiveUpAndDown <- function(x, xlabel="Comparitions", ylabel="Number of genes", 
                          upName="Up", downName="Down",
                          upColor="#b2182b", downColor="#2166ac", inverse=F) {
  require(ggplot2)
  require('DESeq2')
  require(plotly)
  
  if (class(x) != "list")
    stop("First parameter must be a list containing at least one DESeq2 result")
  
  #x is a list of DESEQ results
  myDF <- data.frame(time = character(0), counts = numeric(0), class = character(0))
  
  prevUp <- NA
  prevDown <- NA
  ##TODO, check for namesm otherwise it doesn't work
  
  lapply(1:length(x), function(de){
    
    thisName <- names(x[de])
    print(thisName)
    upGenes <- rownames(x[[de]][x[[de]]$log2FoldChange>0,])
    downGenes <- rownames(x[[de]][x[[de]]$log2FoldChange<0,])
    #commonUp <- intersect(prevUp, upGenes)
    #commonDown <- intersect(prevDown, downGenes)
    upCounts <- ifelse(inverse, length(downGenes), length(upGenes))
    downCounts <- ifelse(inverse, length(upGenes), length(downGenes))
    print(upCounts)
    print(downCounts)
    myDF <<- rbind(myDF, data.frame(time=thisName, counts=upCounts, class=upName))
    myDF <<- rbind(myDF, data.frame(time=thisName, counts=downCounts, class=downName))
    #myDF <<- rbind(myDF, c(thisName, length(downGenes), 'down'))
    #names(myDF) <- c("time", "counts", "class")
    #print(myDF)
  })
  
  #genes <- controlDE$W34$log2FoldChange
  #upGenes <- rownames(controlDE$W34[controlDE$W34$log2FoldChange>0,])
  #downGenes <- rownames(controlDE$W34[controlDE$W34$log2FoldChange<0,])
  #commonUp <- intersect(prevUp, upGenes)
  #commonDown <- intersect(prevDown, downGenes)
  #myDF <- rbind(myDF, c('W34', length(upGenes), -1*length(downGenes)))
  
  names(myDF) <- c("time", "counts", "class")
  
  # ggplot(data=myDF, aes(x=time, y=counts, fill=class)) +
  #   geom_bar(stat="identity") + 
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #   xlab(xlabel) + ylab(ylabel) +
  #   scale_fill_manual(values=c(upColor, downColor))

  p <- plot_ly() %>%
    add_bars(
      x = myDF[myDF$class==upName,]$time,
      y = myDF[myDF$class==upName,]$counts,
      base = 0,
      marker = list(
        color = upColor
      ),
      name = upName
     )  %>%
    add_bars(
      x = myDF[myDF$class==downName,]$time,
      y = myDF[myDF$class==downName,]$counts,
      base = myDF[myDF$class==downName,]$counts *-1,
      marker = list(
        color = downColor
      ),
      name = downName
    )
    # add_bars(
    #   x = c("2016", "2017", "2018"),
    #   y = c(300,400,700),
    #   base = 0,
    #   marker = list(
    #     color = 'blue'
    #   ),
    #   name = 'revenue'
    # )
  
}

print(plotInteractiveUpAndDown(control.res.filter))
