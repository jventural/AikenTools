V_Penfield_Giocobbi <- function(data){
  n <- nrow(data)
  l <- min(data)
  k <- max(data)-min(data)
  result_column <- numeric(n)
  for (row in 1:n) {
    x <- rowMeans(data[row, ])
    v <- (x - l) / k
    result_column[row] <- round(v, 3)
  }
  return(data.frame(V_Penfield_Giocobbi = result_column))
}
