V_Aiken_O <- function(x, n, c) {
  n <- ncol(x)
  c <- max(x)+1
  v_Aiken_O <- round(rowSums(x)/(n*(c-1)), 3)
  return(data.frame(v_Aiken_O = v_Aiken_O))
}
