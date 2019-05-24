tableN <- function(data, x, y) {
  # a function to conveniently "stack" tables, ie, have N tables nested under the same variable as one.

  # perhaps add functionality to return marginals, or as percentages, or both?
  # probably want to edit rownames as Variable column in FOR loop, to avoid xVAL1, xVAL2, etc.

  stopifnot(is.data.frame(data),
            x %in% colnames(data),
            y %in% colnames(data))

  out <- vector("list", length(x))
  names(out) <- x

  for (i in x){
    out[[i]] <- table(data[, i], data[, y], exclude = TRUE)
  }

  out <- as.data.frame(do.call(rbind, out))
  out$Variable <- rownames(out)
  out <- out[, c("Variable", colnames(out)[-ncol(out)])]
  rownames(out) <- NULL

  return(out)
}

## E.g. #####
#tableN(mtcars, x = c("wt", "gear", "carb"), y = "cyl")

