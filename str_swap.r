# rename variables from tidyr::pivot_wider() where len(args) in values_from >1
# (I don't like that it prepends, can't figure out how to append values_from)

str_swap <- function(x, char = "_"){
  strsplit(x, char) |>
    lapply(rev) |>
    lapply(paste0, collapse = char) |> 
    unlist()
}
# e.g. 
#str_swap(c("a_1", "a_2", "a_3"))


