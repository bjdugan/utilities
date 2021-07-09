# nested freqs
# return unweighted frequencies and percentages for all groups in a nesting variable,
# for all variables listed. Supports one level of nesting.
# currently only supports variables with a common number of levels (response options)

nested_freqs <- function(data, nest_var, vars) {
  stopifnot(nest_var %in% colnames(data), vars %in% colnames(data),
            all(is.character(c(nest_var, vars))),
            "str_swap" %in% ls(1))

  require(dplyr)
  require(purrr)
  require(tidyr)
  require(tibble)

  #unique(pull(vars[1]))
  select(data, nest_var, vars) %>%
    split(.[, nest_var]) %>%
    map(select, -c(nest_var)) %>%
    map(as.list) %>%
    map(map, as_tibble) %>%
    map(map, group_by_all, .drop = FALSE) %>%
    map(map, tally) %>%
    map(map, ungroup) %>%
    map(map, filter, value != "(Missing)") %>% # could make this optional {}
    map(map, mutate, value = as.character(value)) %>%
    map(map, bind_rows, tibble(value = "Total", n = 0)) %>%
    map(map, mutate,
        # prevents NaN from division of sum(0)
        p = if_else(!is.nan(n / sum(n)), n / sum(n) * 100, 0),
        n = if_else(value == "Total", sum(n), n),
        p = if_else(value == "Total", sum(p), p)
    ) %>%
    map(map2, map(names(.[[1]]), rep,
                  # this currently breaks even b/c -1 Missing +1 Total
                  length(unique(pull(data, vars[1]))) ),
        mutate) %>%
    map(bind_rows) %>%
    map2(names(.), mutate) %>%
    map(rename, "var" = last_col(1), nest_var = last_col()) %>%
    map(pivot_wider, names_from = nest_var, values_from = c(n, p)) %>%
    reduce(left_join, by = c("var", "value")) %>%
    select(var, value, everything()) %>%
    `colnames<-`(c("var", "value", str_swap(colnames(.)[-c(1:2)])))

}

