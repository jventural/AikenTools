\name{H_Aiken}
\alias{H_Aiken}
\title{Aiken's H coefficient}

\description{
This package provides a function to calculate Aiken's H coefficient whose formula is:

H=1-4sum_(j=1)^(n-1)sum_(i=j+1)^(m)|x_(i)-x_(j)|//[(c-1)(n^(2)-d)]

}

\usage{
H_Aiken <- function(matrix, c, type = "Items") {
  H_values <- numeric(ifelse(type == "Items", nrow(matrix), ncol(matrix)))
  if (type == "Items") {
    for (row_index in 1:nrow(matrix)) {
      row <- matrix[row_index, ]
      n <- length(row)
      S <- 0
      for (j in 1:(n-1)) {
        for (i in (j+1):n) {
          S <- S + abs(row[i] - row[j])
        }
      }
      H_values[row_index] <- 1 - 4 * S / (c * n * (n - 1))
    }
  } else if (type == "Jueces") {
    for (column_index in 1:ncol(matrix)) {
      column <- matrix[, column_index]
      n <- length(column)
      S <- 0
      for (j in 1:(n-1)) {
        for (i in (j+1):n) {
          S <- S + abs(column[i] - column[j])
        }
      }
      H_values[column_index] <- 1 - 4 * S / (c * n * (n - 1))
    }
  }
  resultados <- unlist(H_values)
  resultados <- round(resultados, 3)
  return(data.frame(H_value = resultados))

}

}

\arguments{
\item{matrix}{It is a matrix or dataframe}
\item{c}{Number of rating scale values}
\item{type}{The function is instructed to calculate an H coefficient for the items or for the judges.}
}


\examples{
df1 <- data.frame(Juez1 = c(0,3,3,3,3,3,3,3,3,3),
                  Juez2 = c(0,3,3,3,3,2,3,3,3,3),
                  Juez3 = c(0,3,3,3,2,2,3,3,3,3),
                  Juez4 = c(0,3,3,3,2,2,3,3,3,3))

df2 <- data.frame(Juez1 = c(1,3,3,3,2,3,2,3,3,3),
                  Juez2 = c(2,3,2,3,3,2,1,0,3,3),
                  Juez3 = c(3,2,3,2,2,2,3,3,2,3),
                  Juez4 = c(0,3,3,3,2,2,1,3,3,3))

R_Aiken(df1, df2, type = "Items")

}

\references{
Aiken, L. R. (1980). Content Validity and Reliability of Single Items or Questionnaires. Educational and Psychological Measurement, 40(4), 955-959. doi:10.1177/001316448004000419

}


