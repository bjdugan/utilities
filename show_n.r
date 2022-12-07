# interactively show number of rows in data frame and return df
# ideally placed in pipe %>% prior to e.g. filtering
show_n <- function(x, msg = ""){
  stopifnot(is.data.frame(x))
  cat(msg, nrow(x), "rows...\n")
  return(x)
}
