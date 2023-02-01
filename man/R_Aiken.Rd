\name{R_Aiken}
\alias{R_Aiken}
\title{Aiken's R coefficient}

\description{
This package provides a function to calculate Aiken's R coefficient whose formula is:

R=1-(sum_(j=0)^(2)sum_(i=0)^(2)n_(ij)|i-j|)/(2N)

}

\usage{
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
  } else if (type == "Jueces") {
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

}

\arguments{
\item{df1}{It is the first database}
\item{df2}{It is the second database}
\item{type}{The function is instructed to calculate an R coefficient for the items or for the judges.}
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


