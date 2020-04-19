# takes a list with names (former factor levels) and puts it back into a tibble.
# this is most useful (if not only useful) when splitting on a factor and summarizing by group, then returning the consequent summary tables into a single table.

unsplit_rename = function(list, fac = "v1") {
  stopifnot(is.list(list))

  require(purrr)
  require(dplyr)

  fac <- sym(fac)

  map2(list, names(list), mutate) %>%
    map(rename, !!fac := last_col()) %>%
    bind_rows()

}

# # example (not run)
# mutate(mtcars, am_f = if_else(am == 1, "manual", "automatic")) %>%
# split(.$am_f) %>%
#   map(summarize,
#       avg_mpg = mean(mpg),
#       avg_hp = mean(hp)) %>%
#   unsplit_rename("am_f")
