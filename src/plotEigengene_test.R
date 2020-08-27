### test area
#load toy data
 load("~/Rtoolbox/toydata/vstData.RData")
 load("~/Rtoolbox/toydata/enrichmentData.RData")
 InfomapClusters <- read.delim("~/Rtoolbox/toydata/InfomapClusters.tsv", stringsAsFactors=FALSE)
 Cluster1 <- InfomapClusters[InfomapClusters$cluster=="Cluster1",]$gene
 Cluster2 <- InfomapClusters[InfomapClusters$cluster=="Cluster2",]$gene
 Cluster3 <- InfomapClusters[InfomapClusters$cluster=="Cluster3",]$gene
#regular example
plotEigengene(controlData, Cluster1, substr(rownames(controlData),5,5),
              as.integer(substr(rownames(controlData),2,3)), timeUnits = "Week")

#print both profiles example
plotEigengene(controlData, Cluster3, substr(rownames(controlData),5,5),
              as.integer(substr(rownames(controlData),2,3)), timeUnits = "Week", plotBothProfiles = T, colors=c("#009444", "lightgreen"))

#Error plot both profiles and 2 conditions
plotEigengene(controlData, Cluster1, c(rep("C",28),rep("F",28)),
              as.integer(substr(rownames(controlData),2,3)), timeUnits = "Week", plotBothProfiles = T)



plotEigengene(controlData, Cluster3[1:10], substr(rownames(controlData),5,5),
              as.integer(substr(rownames(controlData),2,3)), timeUnits = "Week", multiline = T)
