# generate dictionary information (codebook) from haven labelled SPSS datasets
# this works well for an individual SPSS data frame, or map()'d over a list of data frames, and is much easier to use than looking at str() or attributes() output

# Try adding argument so that it either returns a dictionary (default, current), or the originating (SPSS) data, 
# ie so it can be used in pipes for a "side effect".

make_dict <- function(data) {
  require(tibble)
  require(dplyr)
  require(purrr)
  require(haven)

  # other control features?
  stopifnot(
    any(map(data, is.labelled)),
    is.data.frame(data)
    )

  tibble(
    variable_name = colnames(data),
    variable_label = map(data, attributes) %>%
      map(pluck, "label") %>%
      as.character(),
    variable_format = map(data, attributes) %>%
      map(pluck, "format.spss") %>%
      as.character(),
    value_labels = map(data, attributes) %>%
      map(pluck, "labels") %>%
      as.character()
  )
}

# demo 
# figure out how to write vignette?

# this will vary by install location and version
# demo <- haven::read_spss("C:/Program Files/IBM/SPSS/Statistics/27/Samples/English/demo.sav")
# 
# # if we want to read SPSS data but want to keep (survey) metainformation, we can make a dictionary or codebook.
# demo_dict <- make_dict(demo)
# 
# # if we want to manipulate data originating from SPSS and return it as SPSS, or export to SPSS with complete features (variable lable, value labels, formats) then 
# demo_r <- mutate_all(is.labelled, as_factor, "labels")
# map2(demo_r, as.list(demo_dict$label), `attr<-`, which = "label") %>%
#   bind_rows() %>%
#   write_sav("demo.sav")

