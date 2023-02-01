\name{H_Aiken}
\alias{H_Aiken}
\title{Aiken's IC coefficient}

\description{
This package provides a function to calculate Aiken's H coefficient whose formula is:

L=(2nkV+z^(2)-zsqrt(4nkV(1-V)+z^(2)))/(2(nk+z^(2)))

U=(2nkV+z^(2)+zsqrt(4nkV(1-V)+z^(2)))/(2(nk+z^(2)))

}

\usage{
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
  return(tibble(INFIC, SUPIC))
}

}

\arguments{
\item{df}{It is a dataframe}
\item{Z}{z-score}
\item{N}{Number of judges}
}


\examples{
df <- data.frame(Juez1 = c(0,3,3,3,3,3,3,3,3,3),
                  Juez2 = c(0,3,3,3,3,2,3,3,3,3),
                  Juez3 = c(0,3,3,3,2,2,3,3,3,3),
                  Juez4 = c(0,3,3,3,2,2,3,3,3,3))

IC_Aiken(df, 1.96, 10)

}

\note{El valor de z puede ser modificado por 1.65 (90 porciento de confianza), 1.96 (95 porciento de confianza) y 2.58(99 porciento de confianza)
}

\references{
Penfield, R. D., & Giacobbi, Jr., P. R. (2004). Applying a Score Confidence Interval to Aiken's Item Content-Relevance Index. Measurement in Physical Education and Exercise Science, 8(4), 213-225. doi:10.1207/s15327841mpee0804_3

}


