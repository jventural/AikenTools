\name{V_Aiken_O}
\alias{V_Aiken_O}
\title{Aiken in the original version}

\description{
This package provides a function to calculate Aiken's V coefficient in the original version of Aiken (1980) whose formula is:

V=(S)/n(c-1)

}

\usage{
V_Aiken_O(x, n, c) {
  n <- ncol(x)
  c <- max(x)+1
  v_Aiken_O <- round(rowSums(x)/(n*(c-1)), 3)
  return(data.frame(v_Aiken_O = v_Aiken_O))
}

}

\arguments{
\item{x}{is the dataframe}
\item{n}{Number of judges}
\item{c}{Number of rating scale values (0-3)}

}


\examples{
df <- data.frame(Juez1 = c(0,3,3,3,3,3,3,3,3,3),
                  Juez2 = c(0,3,3,3,3,2,3,3,3,3),
                  Juez3 = c(0,3,3,3,2,2,3,3,3,3),
                  Juez4 = c(0,3,3,3,2,2,3,3,3,3))
V_Aiken_O(df)

}

\references{
Aiken, L. R. (1980). Content Validity and Reliability of Single Items or Questionnaires. Educational and Psychological Measurement, 40(4), 955-959. doi:10.1177/001316448004000419

}


