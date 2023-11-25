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
}
