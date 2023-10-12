fetch_ipeds_labels <- function(conn, name, yr) {
  # returns a tibble with variable values and labels for variable name
  
  #note  year in IPEDS universe is academic year, eg 2021-22
  # yr takes value of beginning year, e.g. yr = 21 for 2021-22 academic year
  # where NSSE is same period, but admin in spring, so NSSE22 is IPEDS 2021-22 (21)
  # hope their naming convetion hasn't changed over years
  require(dplyr)
  require(DBI)
  
  tbl(conn, paste0("valuesets", yr)) |>  
    filter(varName == name) |>  
    select(!!sym(name) := Codevalue, valueLabel) |> 
    collect()
}
