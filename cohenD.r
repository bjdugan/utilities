cohenD <- function(data, x, y) {
  # data: a data.frame object, filtered on y or not.
  # x: outcome variable of interest
  # y: grouping variable, must have exactly 2 levels. 
  # returns Cohen's d statistic https://en.wikipedia.org/wiki/Effect_size#Cohen's_d
  
  stopifnot(is.data.frame(data), 
            length(unique(data[, y])) == 2
            )
  
  data <- data[!is.na(data[, y]), c(y, x)]
  data[, y] <- as.factor(data[, y])
  
  
  n1 <- nrow(data[data[, y] == levels(data[, y])[1], ])
  n2 <- nrow(data[data[, y] == levels(data[, y])[2], ])
  
  sd1 <- sd(data[data[, y] == levels(data[, y])[1], x], na.rm = TRUE)
  sd2 <- sd(data[data[, y] == levels(data[, y])[2], x], na.rm = TRUE)
  
  m1 <- mean(data[data[, y] == levels(data[, y])[1], x], na.rm = TRUE)
  m2 <- mean(data[data[, y] == levels(data[, y])[2], x], na.rm = TRUE)
  
  meanDiff <- m1 - m2

  pooledSD <- sqrt(
    (  (n2 - 1) * sd2 ^ 2 +
         (n1 - 1) * sd1 ^ 2) /
      (n2 + n1 - 2)
  )
    
  d <- meanDiff / pooledSD
  
  
  out <- list(d = d, 
              meanDiff = meanDiff, 
              pooledSD = pooledSD,
              mean1 = m1, 
              mean2 = m2, 
              sd1 = sd1, 
              sd2 = sd2,
              n1 = n1, 
              n2 = n2
                )
  cat("Cohen's d for mean differences in", paste0("'", x, "'"), "between groups", 
      paste0("'", y, "'"), "\n")
  return(out)
  
  
}
# E.g.
#cohenD(mtcars, "mpg", "vs")
#cohenD(mtcars, "mpg", "am")$d
