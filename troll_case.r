# translate a message into troll case.
tRoLl_cAsE <- function(x){
  stopifnot(is.character(x))
  require(stringr) 
  require(dplyr)
  require(purrr)
  x <- str_split(x, "") %>% 
    map(str_to_lower) %>% 
    unlist()
  is_even <- length(x) %/% 2 == 0 
  if (is_even) {
    i <- seq(2, length(x), by = 2)
  } else {
    i <- seq(2, length(x) - 1, by = 2)
  }
  x[i] <- toupper(x[i])
  return(paste0(x, collapse = ""))
}
tRoLl_cAsE("But socialism kills people!")
