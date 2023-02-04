\name{Repeatability_Aiken}
\alias{Repeatability_Aiken}
\title{Aiken's Repeatability coefficient}

\description{
This package provides a function to calculate Aiken's Repeatability coefficient whose formula is:

R==1-S /[n(c-1)]

}

\usage{
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


}

\arguments{
\item{df1}{It is the first database}
\item{df2}{It is the second database}
\item{type}{The function is instructed to calculate an R coefficient for the items or for the judges.}
}


\examples{
df1 <- data.frame(J1_t1 = c(0,3,3,3,3,3,3,3,3,3),
                  J2_t1 = c(0,3,3,3,3,2,3,3,3,3),
                  J3_t1 = c(0,3,3,3,2,2,3,3,3,3),
                  J4_t1 = c(0,3,3,3,2,2,3,3,3,3))

df2 <- data.frame(J1_t2 = c(3,3,3,3,2,3,2,3,3,3),
                  J2_t2 = c(3,3,2,3,3,2,1,0,3,3),
                  J3_t2 = c(3,3,3,2,2,2,3,3,2,3),
                  J4_t2 = c(3,3,3,3,2,2,1,3,3,3))

R_Aiken(df1, df2, type = "Items")

}

\references{
Aiken, L. R. (1985). Three Coefficients for Analyzing the Reliability and Validity of Ratings. Educational and Psychological Measurement, 45(1), 131-142. doi:10.1177/0013164485451012

}


