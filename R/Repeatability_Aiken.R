Repeatability_Aiken <- function(df1, df2, c, type = "items") {
  if (type == "items") {
    n <- nrow(df1)
    R <- numeric(n)
    for (i in 1:n) {
      S <- 0
      for (j in 1:ncol(df1)) {
        S <- S + abs(df1[i,j] - df2[i,j])
      }
      R[i] <- 1 - (S / (ncol(df1) * (c - 1)))
    }
  } else if (type == "jueces") {
    n <- ncol(df1)
    R <- numeric(n)
    for (i in 1:n) {
      S <- 0
      for (j in 1:nrow(df1)) {
        S <- S + abs(df1[j,i] - df2[j,i])
      }
      R[i] <- 1 - (S / (nrow(df1) * (c - 1)))
    }
  }
  # return(R)
  return(data.frame(Repeatability = R))
}
