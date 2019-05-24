golden <- function(a) {
  # golden ratio: a+b is to a as a is to b
  # given a, the longer side, return b
  stopifnot(is.numeric(a))
  a * 0.6180339
}