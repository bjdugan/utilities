fetch_ipeds_labels <- function(conn, name, yr) {
  # returns a tibble with variable values and labels for variable name
  
  #note  year in IPEDS universe is academic year, eg 2021-22
  # yr takes value of beginning year, e.g. yr = 21 for 2021-22 academic year
  # where NSSE is same period, but admin in spring, so NSSE22 is IPEDS 2021-22 (21)
  # hope their naming convetion hasn't changed over years
  require(dplyr)
  #require(DBI)
  # perhaps extend this to alternatively query vartable to pull other contextualizing data
  # eg for those w/o valueLabels (all counts), get vartable$varTitle or longDescription
  
  # for pmap or lapply, will need to rethink conn
  tbl(conn, paste0("valuesets", yr)) |>  
    filter(varName == name) |>  
    collect() |> 
    # most of these are stored char but need to join as ints
    transmute(!!sym(name) := as.integer(Codevalue), 
              # lots of trailing newlines etc.
              valueLabel = gsub("^\\s*|\\s*$",  "", valueLabel))
}

# consider extending for multiple vars, which might exist in multiple tables, or might not have labels in valuesets. Pivoting here only works if they all share same typie (usually int).,
# left_join(
#   tbl(ipeds22, "IC2021") |> 
#     select(UNITID, LEVEL5, CALSYS) |> 
#     pivot_longer(-UNITID),
#   tbl(ipeds22, "valuesets21") |> 
#     filter(varName %in% c("LEVEL5", "CALSYS")) |> 
#     select(varName, valueLabel),
#   by = c("name" = "varName")) |> 
#   select(-value) |> 
#   pivot_wider(names_from = c(name), values_from = valueLabel)