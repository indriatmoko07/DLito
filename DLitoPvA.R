DLitoPvA <- function(x){
  library(dartR)
  library(poppr)
  a<- private_alleles(gl2gi(x))
  PvA<- apply(a, 1, function(row) sum(row > 0))
  df<- cbind(PvA,a)
  column_names_with_count_gt_zero <- apply(df[, -1], 1, function(row) {
    names(row)[row > 0]
  })
}

