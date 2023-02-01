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
  } else if (type == "Judges") {
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
