cap_first <- function(x) {
  # capitalize the first letter in a char vector, e.g., a name or sentence.
  if (is.character(x)) {
    x <- paste0(
      toupper(substr(x, 1, 1)), 
      tolower(substr(x, 2, nchar(x)))
    )
  } else {
    return(cat(x, "is not a character vector.\n"))
  }
  
  return(x)
}
