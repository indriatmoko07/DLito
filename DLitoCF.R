DLitoCF <- function(x,y){
  library(dartR)
  glSNP <- gl.read.dart(filename=x,ind.metafile=y)
  glSNP
  gl.smearplot(glSNP)
  data<-glSNP
  data_filter_callrate<-gl.filter.callrate(data,
                                           method = 'loc',
                                           threshold = 0.95,
                                           verbose = 3  )
  data_filter_reproducibility<-gl.filter.reproducibility(data_filter_callrate,
                                                         threshold = 0.95,
                                                         verbose = 3)
  data_filter_rdepth<-gl.filter.rdepth(data_filter_reproducibility,
                                       lower = 10,
                                       upper = 25,
                                       verbose = 3)
  data_filtered<-gl.filter.monomorphs(data_filter_rdepth)
  data_filtered
}
