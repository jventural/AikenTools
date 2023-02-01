IC_Aiken <- function(df, Z, N){
  K <- max(df)-min(df)
  l <- min(df)
  x <- rowMeans(df)
  Aiken <- function(x, k, l){
    v = (x-l)/k
    return(v)}
  v <- Aiken(x, K, l)
  IC1 = ((2 * v * N * K) + (Z^2))
  IC2 = Z * (sqrt((4 * N * K * v) * (1 - v) + (Z^2)))
  IC3 = 2 * ((N * K) + (Z^2))
  INFIC = (IC1 - IC2) / IC3
  SUPIC = (IC1 + IC2) / IC3
  INFIC <- round(INFIC, 3)
  SUPIC <- round(SUPIC, 3)
  return(tibble(INFIC, SUPIC))
}
