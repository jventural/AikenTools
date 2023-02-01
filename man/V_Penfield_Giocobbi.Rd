\name{V_Penfield_Giocobbi}
\alias{V_Penfield_Giocobbi}
\title{Aiken's V coefficient of Penfield y Giocobbi}

\description{
This package provides a function to calculate Aiken's V coefficient in the version of  Penfield y Giocobbi (2004) whose formula is:

V= (M-l)/(k)

}

\usage{
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

}

\arguments{
\item{data}{is the data base in a dataframe structure}

}


\examples{
df <- data.frame(Juez1 = c(0,3,3,3,3,3,3,3,3,3),
                  Juez2 = c(0,3,3,3,3,2,3,3,3,3),
                  Juez3 = c(0,3,3,3,2,2,3,3,3,3),
                  Juez4 = c(0,3,3,3,2,2,3,3,3,3))
V_Penfield_Giocobbi(df)

}

\references{
Penfield, R. D., & Giacobbi, Jr., P. R. (2004). Applying a Score Confidence Interval to Aiken's Item Content-Relevance Index. Measurement in Physical Education and Exercise Science, 8(4), 213-225. doi:10.1207/s15327841mpee0804_3

}


