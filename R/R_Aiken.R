R_Aiken <- function(df1, df2, type = "Items") {
  if (type == "Items") {
    n <- nrow(df1)
    c <- ncol(df1)
    R_vector <- numeric(n)
    for (i in 1:n) {
      s <- 0
      for (j in 1:c) {
        s <- s + abs(df1[i,j] - df2[i,j])
      }
      R_vector[i] <- 1 - (s / (c - 1))
    }
  } else if (type == "Judges") {
    n <- nrow(df1)
    c <- ncol(df1)
    R_vector <- numeric(c)
    for (i in 1:c) {
      s <- 0
      for (j in 1:n) {
        s <- s + abs(df1[j,i] - df2[j,i])
      }
      R_vector[i] <- 1 - (s / (n - 1))
    }
  }

  R_vector <- round(R_vector, 3)
  return(data.frame(R_Aiken = R_vector))
}
