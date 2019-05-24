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

# ideally, a function would return something like Ctables - basically several tables with different metrics (counts, percentages) for common items spliced together. The above two functions take tables (counts) and return them as col or row percents. An ideal fxn might look like this:
# ctable.r <- function(table, as.col.pct = FALSE, as.row.pct = FALSE, spliced = TRUE)
# where perhaps spliced arg blends the tables (t1 col1, t2 col1, t1 col2, t2 col2, etc), and as.row.pct and as.col.pct calls the functions I have or something like them. To get even more interesting, add further functionality like a FUN arg in apply(), e.g., get means ratehr than freq dist.
# this already usefully returns a data frame, with names (provided through table() ), so it is easily assigned as a new data frame...meaining it could easily be piped %>%, too.
# this already writes.*() fairly well, too, except that colnames get "." in place of spaces, and (sometimes a problem) rownames are carried forward.
# 2018-12-13 11:18:02 EST updated these three functions to return() instead of print(), so i won't need to sink() this anymore...can't believe I hadn't thought of that sooner! Also removed all round()'s, so that a greater number of digits are retained
