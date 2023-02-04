#Cargar dataframe
df1 <- data.frame(J1_1 = c(0,3,3,3,3,3,3,3,3,3),
                  J2_t1 = c(0,3,3,3,3,2,3,3,3,3),
                  J3_t1 = c(0,3,3,3,2,2,3,3,3,3),
                  J4_t1 = c(0,3,3,3,2,2,3,3,3,3))

df2 <- data.frame(J1_t2 = c(1,3,3,3,2,3,2,3,3,3),
                  J2_t2 = c(1,3,2,3,3,2,1,0,3,3),
                  J3_t2 = c(3,3,3,2,2,2,3,3,2,3),
                  J4_t2 = c(3,3,3,3,2,2,1,3,3,3))

#para un vector
RepeatabilityCoefficient <- function(df1, df2, c) {
  n <- nrow(df1)
  S <- 0
  for (i in 1:n) {
    for (j in 1:ncol(df1)) {
      S <- S + abs(df1[i,j] - df2[i,j])
    }
  }
  R <- 1 - (S / (n * (c - 1)))
  return(R)
}

RepeatabilityCoefficient(df1, df2, 3)

#por filas
RepeatabilityCoefficient_row <- function(df1, df2, c) {
  n <- nrow(df1)
  R_row <- numeric(n)
  for (i in 1:n) {
    S <- 0
    for (j in 1:ncol(df1)) {
      S <- S + abs(df1[i,j] - df2[i,j])
    }
    R_row[i] <- 1 - (S / (ncol(df1) * (c - 1)))
  }
  return(R_row)
}

R_row <- RepeatabilityCoefficient_row(df1, df2, 3)
print(R_row)

#por columnas
RepeatabilityCoefficient_col <- function(df1, df2, c) {
  n <- ncol(df1)
  R_col <- numeric(n)
  for (i in 1:n) {
    S <- 0
    for (j in 1:nrow(df1)) {
      S <- S + abs(df1[j,i] - df2[j,i])
    }
    R_col[i] <- 1 - (S / (nrow(df1) * (c - 1)))
  }
  return(R_col)
}

R_col <- RepeatabilityCoefficient_col(df1, df2, 3)
print(R_col)

#combinado
RepeatabilityCoefficient <- function(df1, df2, c, type = "items") {
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

R <- RepeatabilityCoefficient(df1, df2, 3, "items")
print(R)

R <- RepeatabilityCoefficient(df1, df2, 3, "jueces")
print(R)
