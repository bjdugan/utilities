as.col.pct <- function(data) {
  # this function manipulates a table, adding column marginals and re-computing
  # the input table as column percentages.

  # add column marginal
  x <- data.frame(rbind(data, 1:ncol(data)))
  x[nrow(x),] <- 0
  x[nrow(x),] <- apply(x, 2, sum)
  rownames(x) <- c(rownames(data), "Column Total")
  # output
  for (i in 1:ncol(x)) {
    x[, i] <- x[,i] / x[nrow(x), i] * 100
  }
  return(x)
}
as.row.pct <- function(data) {
  # this function manipulates a table, adding row marginals and re-computing
  # the input table as row percentages.

  # add column marginal
  x <- data.frame(cbind(data, 1:nrow(data)))
  x[,ncol(x)] <- 0
  x[,ncol(x)] <- apply(x, 1, sum)
  colnames(x) <- c(colnames(data), "Row Total")
  # output
  for (i in 1:nrow(x)) {
    x[i,] <- x[i,] / x[i, ncol(x)] * 100
  }
  return(x)
}

splice.tables <- function(x, rowpct = FALSE, marginals = FALSE) {
  # this takes at least two tables or data frames and splices them to create
  # side-be-side columns for reporting. cols controls column or row-wise pctg calc

  stopifnot(any(is.data.frame(x), is.table(x)))

  sink("NUL")
  if (rowpct == TRUE) {
    y <- as.row.pct(x)
  } else {
    y <- as.col.pct(x)
  }
  sink()
  closeAllConnections()

  
  # OMIT marginals by default
  if (marginals == FALSE) {
    if (any(grepl("Total", colnames(y)))) {
      y <- y[ , 1:ncol(y) - 1]
    }
    if (any(grepl("Total", rownames(y)))) {
      y <- y[1:nrow(y) - 1, ]
    }
  } else {
    print("haven't gotten to this yet.")
  }

  # so it plays nicely
  x <- as.data.frame.matrix(x)

  # splice tables
  z <- cbind.data.frame(x, y)
  for (i in 1:(ncol(z) / 2)) {
    if (i <= 1) {
      cols <- c(i, i + ncol(z) / 2)
    } else {
      cols <- c(cols, i, i + ncol(z) / 2)
    }
    #print(c(i, i + ncol(z) / 2))
  }
  z <- z[, cols]

  # reformatting for printing
  # all even cols are %s
  a <- seq(from = 1, to = ncol(z) - 1, by = 2)
  b <- seq(from = 2, to = ncol(z), by = 2)
  if (rowpct == TRUE) {
    colnames(z)[b] <- paste0(abbreviate(colnames(z[a]), minlength = 6), ".RPct")
  } else {
    colnames(z)[b] <- paste0(abbreviate(colnames(z[a]), minlength = 6), ".CPct")
  }
  return(z)

}

