DLitoDAPC<- function(x){
  library(adegenet)
  library(dartR)
  gi<-gl2gi(x)
  set.seed(999)
  y = tab(gi, NA.method = "mean")
  crossval = xvalDapc(y, gi$pop, result = "groupMean", xval.plot = TRUE)
  numPCs = as.numeric(crossval$`Number of PCs Achieving Lowest MSE`)
  dapc_result = dapc(gi, gi$pop, n.pca = numPCs)
  myCol <- c("purple","green","orange","red","blue","yellow","gray","pink","brown","black")
  scatter(dapc_result, posi.da="topright", bg="white",
          pch=numPCs, cstar=1, col=myCol, scree.pca=TRUE,
          posi.pca="topleft")
  dapc.results <- as.data.frame(dapc_result$posterior)
  dapc.results$pop <- pop(glSNP)
  dapc.results$indNames <- rownames(dapc.results)
  library(tidyr)
  dapc.results <- pivot_longer(dapc.results, -c(pop, indNames))
  head(dapc.results, n = 6)
  colnames(dapc.results) <- c("Original_Pop","Sample","Assigned_Pop","Posterior_membership_probability")
  
  p <- ggplot(dapc.results, aes(x=Sample, y=Posterior_membership_probability, fill=Assigned_Pop))
  p <- p + geom_bar(stat='identity')
  p <- p + scale_fill_manual(values = cols)
  p <- p + facet_grid(~Original_Pop, scales = "free")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
  p
}

